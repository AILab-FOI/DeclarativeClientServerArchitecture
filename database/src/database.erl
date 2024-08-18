-module(database).

-behaviour(application).

-include_lib("database/include/records.hrl").

-export([install/0, start/2, stop/1]).

start(normal, []) ->
    mnesia:wait_for_tables([db_fakultet, db_student], 5000),
    database_sup:start_link().

stop(_) ->
    ok.

install() ->
    Nodes = [node() | nodes()],
    mnesia:stop(),
    ok = mnesia:delete_schema(Nodes),
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
                            [{attributes, record_info(fields, db_kolegij)},
                             {index, [#db_kolegij.naziv]},
                             {disc_copies, Nodes}]),
    io:format("Table kolegij ~p~n", [mnesia:error_description(R2)]),
    {_, R3} =
        mnesia:create_table(db_sekcija,
                            [{attributes, record_info(fields, db_sekcija)},
                             {index, [#db_sekcija.naziv]},
                             {disc_copies, Nodes}]),
    io:format("Table sekcija ~p~n", [mnesia:error_description(R3)]),

    {_, R4} =
        mnesia:create_table(db_kviz,
                            [{attributes, record_info(fields, db_kviz)}, {disc_copies, Nodes}]),
    io:format("Table kviz ~p~n", [mnesia:error_description(R4)]),
    {_, R5} =
        mnesia:create_table(db_korisnik,
                            [{attributes, record_info(fields, db_korisnik)},
                             {index, [#db_korisnik.oib, #db_korisnik.email]},
                             {disc_copies, Nodes}]),
    io:format("Table korisnik ~p~n", [mnesia:error_description(R5)]),
    {_, R6} =
        mnesia:create_table(db_sadrzaj,
                            [{attributes, record_info(fields, db_sadrzaj)}, {disc_copies, Nodes}]),
    io:format("Table sadrzaj ~p~n", [mnesia:error_description(R6)]),
    {_, R7} =
        mnesia:create_table(db_pitanje,
                            [{attributes, record_info(fields, db_pitanje)}, {disc_copies, Nodes}]),
    io:format("Table pitanje ~p~n", [mnesia:error_description(R7)]),
    {_, R8} =
        mnesia:create_table(db_kviz_konfiguracija,
                            [{attributes, record_info(fields, db_kviz_konfiguracija)},
                             {disc_copies, Nodes}]),
    io:format("Table kviz_konfiguracija ~p~n", [mnesia:error_description(R8)]),

    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start().
