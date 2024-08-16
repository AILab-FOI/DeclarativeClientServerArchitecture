-module(database).

-behaviour(application).

-export([install/1, start/2, stop/1, dodaj_studenta/6, dohvati_korisnika/1,
         dohvati_studente/0, prijava/2, dohvati_fakultet/1, dodaj_fakultet/2, dohvati_fakultete/0,
         obrisi_studenta/1]).

-define(ID, erlang:unique_integer([positive])).

-type id() :: pos_integer().
-type adresa() ::
    #{grad := binary(),
      ulica := binary(),
      postanski_broj := number(),
      drzava := binary(),
      kucni_broj := binary()}.
-type status_djelatnika() :: nositelj | asistent.
-type datum_vrijeme() ::
    {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
-type lozinka() :: {binary(), binary()}.

-record(db_fakultet,
        {id :: id(),
         naziv :: binary(),
         adresa :: adresa(),
         id_katedre = [] :: [id()],
         id_djelatnici = [] :: [id()],
         id_student = [] :: [id()]}).
-record(db_katedra,
        {id :: id(), naziv :: binary(), id_djelatnik = [] :: [id()], id_kolegij = [] :: [id()]}).
-record(db_student,
        {id :: id(),
         ime :: binary(),
         oib :: integer(),
         prezime :: binary(),
         lozinka :: lozinka(),
         email :: binary(),
         % id_kolegij :: [id()],
         opis = "" :: binary()}).
-record(db_djelatnik_konfiguracija,
        {id :: id(), id_djelatnik :: id(), status :: status_djelatnika()}).
-record(db_djelatnik,
        {id :: id(),
         ime :: binary(),
         prezime :: binary(),
         lozinka :: lozinka(),
         oib :: integer(),
         email :: binary(),
         opis :: binary(),
         kabinet :: binary(),
         vrijeme_konzultacija = [] :: datum_vrijeme()}).
-record(db_kolegij,
        {id :: id(), naziv :: binary(), id_sudionik :: [id()], id_lekcije = [] :: [id()]}).
-record(db_ocjena, {student_id :: id()}).
-record(db_lekcija,
        {id :: id(), naziv :: binary(), id_sadrzaj = [] :: [id()], id_kviz = [] :: [id()]}).
-record(db_sadrzaj, {id :: id(), naslov :: binary(), opis :: binary()}).
-record(db_kviz,
        {id :: id(),
         naziv :: binary(),
         dostupan_od :: datum_vrijeme(),
         dostupan_do :: datum_vrijeme(),
         id_pitanja = [] :: [id()]}).
-record(db_pitanje, {id :: id(), naslov :: binary(), odgovori = [] :: [binary()]}).

start(normal, []) ->
    mnesia:wait_for_tables([db_fakultet, db_student], 5000),
    database_sup:start_link().

stop(_) ->
    ok.

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    {_, R} =
        mnesia:create_table(db_fakultet,
                            [{attributes, record_info(fields, db_fakultet)}, {disc_copies, Nodes}]),
    io:format("Table fakultet ~p~n", [mnesia:error_description(R)]),
    {_, R1} =
        mnesia:create_table(db_katedra,
                            [{attributes, record_info(fields, db_katedra)}, {disc_copies, Nodes}]),
    io:format("Table katedra ~p~n", [mnesia:error_description(R1)]),
    {_, R2} =
        mnesia:create_table(db_kolegij,
                            [{attributes, record_info(fields, db_kolegij)}, {disc_copies, Nodes}]),
    io:format("Table kolegij ~p~n", [mnesia:error_description(R2)]),
    {_, R3} =
        mnesia:create_table(db_lekcija,
                            [{attributes, record_info(fields, db_lekcija)}, {disc_copies, Nodes}]),
    io:format("Table lekcija ~p~n", [mnesia:error_description(R3)]),
    {_, R4} =
        mnesia:create_table(db_kviz,
                            [{attributes, record_info(fields, db_kviz)}, {disc_copies, Nodes}]),
    io:format("Table kviz ~p~n", [mnesia:error_description(R4)]),
    {_, R5} =
        mnesia:create_table(db_student,
                            [{attributes, record_info(fields, db_student)},
                             {index, [#db_student.oib, #db_student.email]},
                             {disc_copies, Nodes}]),
    io:format("Table student ~p~n", [mnesia:error_description(R5)]),
    {_, R6} =
        mnesia:create_table(db_djelatnik,
                            [{attributes, record_info(fields, db_djelatnik)},
                             {index, [#db_djelatnik.oib, #db_djelatnik.email]},
                             {disc_copies, Nodes}]),
    io:format("Table djelatnik ~p~n", [mnesia:error_description(R6)]),

    rpc:multicall(Nodes, application, stop, [mnesia]).

-spec dodaj_studenta(binary(), binary(), integer(), binary(), binary(), binary()) ->
                        {unable_to_insert, term()} | {done, pos_integer()}.
dodaj_studenta(Ime, Prezime, Oib, Lozinka, Email, Opis) ->
    Id = ?ID,
    Salt = crypto:strong_rand_bytes(16),
    Hash = crypto:hash(sha256, <<Salt/binary, Lozinka/binary>>),
    Fun = fun() ->
             case mnesia:index_read(db_student, Oib, #db_student.oib) == []
                  andalso mnesia:index_read(db_student, Email, #db_student.email) == []
             of
                 false -> {error, korisnik_postoji};
                 true ->
                     mnesia:write(#db_student{id = Id,
                                              ime = Ime,
                                              prezime = Prezime,
                                              oib = Oib,
                                              lozinka = {Hash, Salt},
                                              opis = Opis,
                                              email = Email})
             end
          end,
    Trans_result = mnesia:transaction(Fun),
    case Trans_result of
        {aborted, Reason} ->
            {unable_to_insert, Reason};
        {atomic, {error, korisnik_postoji}} ->
            {unable_to_insert, "Korisnik postoji!"};
        {atomic, ok} ->
            {done, Id};
        _ ->
            {unable_to_insert, "Transakcija neuspiješna"}
    end.

obrisi_studenta(Id) ->
    io:format("~p", [Id]),
    Fun = fun() -> mnesia:delete({db_student, Id}) end,
    mnesia:transaction(Fun).

dohvati_studente() ->
    Fun = fun(#db_student{id = Id,
                          ime = Ime,
                          prezime = Prezime,
                          oib = Oib,
                          opis = Opis,
                          email = Email},
              Acc) ->
             [#{id => Id,
                ime => Ime,
                prezime => Prezime,
                oib => Oib,
                email => Email,
                opis => Opis}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_student) end),
    Record.

dohvati_korisnika(Id) ->
    Fun = fun() ->
             case mnesia:read({db_student, Id}) of
                 [#db_student{ime = Ime,
                              prezime = Prezime,
                              oib = Oib,
                              email = Email,
                              opis = Opis}] ->
                     Kolegiji =
                         lists:map(fun(Kolegij) ->
                                      case mnesia:read({db_kolegij, Kolegij}) of
                                          [#db_kolegij{id = IdK, naziv = NazivK}] -> {IdK, NazivK};
                                          [] -> undefined
                                      end
                                   end,
                                   []),
                     #{id => Id,
                       ime => Ime,
                       prezime => Prezime,
                       oib => Oib,
                       email => Email,
                       opis => Opis,
                       kolegiji => Kolegiji};
                 [] ->
                     case mnesia:read({db_djelatnik, Id}) of
                         [#db_djelatnik{ime = Ime,
                                        prezime = Prezime,
                                        oib = Oib,
                                        email = Email,
                                        opis = Opis}] ->
                             #{id => Id,
                               ime => Ime,
                               prezime => Prezime,
                               oib => Oib,
                               email => Email,
                               opis => Opis};
                         [] -> {error, korisnik_ne_postoji}
                     end
             end
          end,
    mnesia:transaction(Fun).

prijava(Email, Lozinka) ->
    Fun = fun() ->
             case mnesia:index_read(db_student, Email, #db_student.email) of
                 [#db_student{id = Id, lozinka = {Hash, Salt}}] ->
                     Key = crypto:hash(sha256, <<Salt/binary, Lozinka/binary>>),
                     case Key == Hash of
                         true -> {ok, #{id => Id}};
                         false -> {error, lozinka_nije_ispravna}
                     end;
                 [] ->
                     case mnesia:index_read(db_djelatnik, Email, #db_djelatnik.email) of
                         [#db_djelatnik{id = Id, lozinka = {Hash, Salt}}] ->
                             Key = crypto:hash(sha256, <<Salt/binary, Lozinka/binary>>),
                             case Key == Hash of
                                 true -> {ok, #{id => Id}};
                                 false -> {error, lozinka_nije_ispravna}
                             end;
                         {aborted, Reason} -> {error, korisnik_ne_postoji}
                     end
             end
          end,
    mnesia:transaction(Fun).

dohvati_fakultete() ->
    Fun = fun(#db_fakultet{id = Id,
                           naziv = Naziv,
                           adresa = Adresa},
              Acc) ->
             [{Id, Naziv, Adresa} | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_fakultet) end),
    Record.

dohvati_fakultet(Id) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet, Id}) of
                 [#db_fakultet{naziv = N, adresa = A}] -> {Id, N, A};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_fakultet(Naziv, Adresa) ->
    Fun = fun() ->
             mnesia:write(#db_fakultet{id = ?ID,
                                       naziv = Naziv,
                                       adresa = Adresa})
          end,
    Trans_result = mnesia:transaction(Fun),
    case Trans_result of
        {aborted, Reason} ->
            {unable_to_insert, Reason};
        {atomic, Result} ->
            {done, Result};
        _ ->
            unable_to_insert
    end.
