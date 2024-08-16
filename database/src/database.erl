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
        mnesia:create_table(db_lekcija,
                            [{attributes, record_info(fields, db_lekcija)}, {disc_copies, Nodes}]),
    io:format("Table lekcija ~p~n", [mnesia:error_description(R3)]),
    {_, R4} =
        mnesia:create_table(db_kviz,
                            [{attributes, record_info(fields, db_kviz)}, {disc_copies, Nodes}]),
    io:format("Table kviz ~p~n", [mnesia:error_description(R4)]),
    {_, R5} =
        mnesia:create_table(db_korisnik,
                            [{attributes, record_info(fields, db_korisnik)},
                             {index, [#db_korisnik.oib, #db_korisnik.email]},
                             {disc_copies, Nodes}]),
    io:format("Table student ~p~n", [mnesia:error_description(R5)]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start().
