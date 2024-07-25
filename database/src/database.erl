-module(database).

-export([install/1, start/2, stop/1, dodaj_studenta/4, dohvati_studenta/1]).

-record(db_fakultet, {uuid, naziv, adresa, lokacija}).
-record(db_katedra, {uuid, naziv}).
-record(db_student, {uuid, ime, prezime, oib, lozinka}).
-record(db_djelatnik, {uuid, naziv, oib}).
-record(db_kolegij, {uuid, naziv, lekcija}).
-record(db_lekcija, {uuid, naziv, sadrzaj}).
-record(db_kviz, {uuid, naziv}).

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
    mnesia:create_table(db_student,
                        [{attributes, record_info(fields, db_student)},
                         {index, [#db_student.uuid]},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

dodaj_studenta(Ime, Prezime, Oib, Lozinka) ->
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
