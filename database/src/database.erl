-module(database).

-export([install/1, start/2, stop/1, dodaj_studenta/4, dohvati_studenta/1,
         dohvati_fakultet/1]).

-record(db_fakultet, {uuid, naziv, adresa, lokacija}).
-record(db_katedra, {uuid, naziv}).
-record(db_student, {uuid, ime, prezime, oib, lozinka}).
-record(db_djelatnik, {uuid, naziv, oib, uuid_katedra}).
-record(db_kolegij, {uuid, naziv, lekcija, uuid_nositelj}).
-record(db_lekcija, {uuid, naziv, sadrzaj}).
-record(db_kviz, {uuid, naziv, uuid_lekcija}).

start(normal, []) ->
    mnesia:wait_for_tables([db_ustanove, db_korisnik], 5000),
    database_sup:start_link().

stop(_) ->
    ok.

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    mnesia:create_table(db_fakultet,
                        [{attributes, record_info(fields, db_fakultet)},
                         {index, [#db_fakultet.uuid]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(db_katedra,
                        [{attributes, record_info(fields, db_katedra)},
                         {index, [#db_katedra.uuid]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(db_kolegij,
                        [{attributes, record_info(fields, db_kolegij)},
                         {index, [#db_kolegij.uuid]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(db_lekcija,
                        [{attributes, record_info(fields, db_lekcija)},
                         {index, [#db_lekcija.uuid]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(db_kviz,
                        [{attributes, record_info(fields, db_kviz)},
                         {index, [#db_kviz.uuid]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(db_student,
                        [{attributes, record_info(fields, db_student)},
                         {index, [#db_student.uuid]},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

dodaj_studenta(Ime, Prezime, Oib, Lozinka) ->
    io:format(Ime),
    Fun = fun() ->
             mnesia:write(#db_student{ime = Ime,
                                      prezime = Prezime,
                                      oib = Oib,
                                      lozinka = Lozinka})
          end,
    Trans_result = mnesia:transaction(Fun),
    case Trans_result of
        {aborted, Reason} ->
            unable_to_insert;
        {atomic, Result} ->
            done;
        _ ->
            unable_to_insert
    end.

dohvati_studenta(Uuid) ->
    Fun = fun() ->
             case mnesia:read({db_student, Uuid}) of
                 [#db_student{ime = I,
                              prezime = P,
                              oib = O,
                              lozinka = L}] ->
                     {Uuid, I, P, O, L};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dohvati_fakultet(Uuid) ->
    Fun = fun() ->
             case mnesia:read({db_fakultet, Uuid}) of
                 [#db_fakultet{naziv = N,
                               adresa = A,
                               lokacija = L}] ->
                     {Uuid, N, A, L};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).
