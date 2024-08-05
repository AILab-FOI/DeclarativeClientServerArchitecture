-module(database).

-behaviour(application).

-export([install/1, start/2, stop/1, dodaj_studenta/4, dohvati_studenta/1,
         dohvati_studente/0, dohvati_fakultet/1, dodaj_fakultet/3, dohvati_fakultete/0]).

-record(db_fakultet, {uuid, naziv, adresa, lokacija}).
-record(db_katedra, {uuid, naziv}).
-record(db_student, {uuid, ime, prezime, oib, lozinka}).
-record(db_djelatnik, {uuid, naziv, oib, uuid_katedra}).
-record(db_kolegij, {uuid, naziv, lekcija, uuid_nositelj}).
-record(db_lekcija, {uuid, naziv, sadrzaj}).
-record(db_kviz, {uuid, naziv, uuid_lekcija}).
-record(table_id, {table_name, last_id}).

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
                            [{attributes, record_info(fields, db_student)}, {disc_copies, Nodes}]),
    io:format("Table student ~p~n", [mnesia:error_description(R5)]),
    {_, R6} =
        mnesia:create_table(table_id,
                            [{attributes, record_info(fields, table_id)}, {disc_copies, Nodes}]),
    io:format("Table student ~p~n", [mnesia:error_description(R6)]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

dodaj_studenta(Ime, Prezime, Oib, Lozinka) ->
    Uuid = uuid:get_v4(),
    Fun = fun() ->
             mnesia:write(#db_student{uuid = Uuid,
                                      ime = Ime,
                                      prezime = Prezime,
                                      oib = Oib,
                                      lozinka = Lozinka})
          end,
    Trans_result = mnesia:transaction(Fun),
    case Trans_result of
        {aborted, Reason} ->
            {unable_to_insert, Reason};
        {atomic, _} ->
            {done, uuid:uuid_to_string(Uuid)};
        _ ->
            unable_to_insert
    end.

dohvati_studente() ->
    Fun = fun(#db_student{uuid = U,
                          ime = I,
                          prezime = P,
                          lozinka = L},
              Acc) ->
             UuidString = uuid:uuid_to_string(U),
             [#{uuid => UuidString,
                ime => I,
                prezime => P,
                lozinka => L}
              | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_student) end),
    Record.

dohvati_studenta(Uuid) ->
    Fun = fun() ->
             UuidTransformed = uuid:string_to_uuid(Uuid),
             case mnesia:read({db_student, UuidTransformed}) of
                 [#db_student{ime = I,
                              prezime = P,
                              oib = O,
                              lozinka = L}] ->
                     #{uuid => Uuid,
                       ime => I,
                       prezime => P,
                       oib => O,
                       lozinka => L};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dohvati_fakultete() ->
    Fun = fun(#db_fakultet{uuid = U,
                           naziv = N,
                           adresa = A,
                           lokacija = L},
              Acc) ->
             UuidString = uuid:uuid_to_string(U),
             [{UuidString, N, A, L} | Acc]
          end,
    {atomic, Record} = mnesia:transaction(fun() -> mnesia:foldl(Fun, [], db_fakultet) end),
    Record.

dohvati_fakultet(Uuid) ->
    Fun = fun() ->
             UuidTransformed = uuid:string_to_uuid(Uuid),
             case mnesia:read({db_fakultet, UuidTransformed}) of
                 [#db_fakultet{naziv = N,
                               adresa = A,
                               lokacija = L}] ->
                     {Uuid, N, A, L};
                 [] -> undefined
             end
          end,
    mnesia:transaction(Fun).

dodaj_fakultet(Naziv, Adresa, Lokacija) ->
    Fun = fun() ->
             Uuid = uuid:get_v4(),
             mnesia:write(#db_fakultet{uuid = Uuid,
                                       naziv = Naziv,
                                       adresa = Adresa,
                                       lokacija = Lokacija})
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
