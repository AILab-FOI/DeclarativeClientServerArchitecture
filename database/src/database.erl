-module(database).

-behaviour(application).

-include_lib("database/include/records.hrl").

-export([install/0, start/2, stop/1]).

start(normal, []) ->
    mnesia:wait_for_tables([db_fakultet, db_student], 5000),
    database_sup:start_link().

stop(_) ->
    ok.

create_table(Db, Type, Nodes, RecordInfo, Index) ->
    {_, R} =
        mnesia:create_table(Db,
                            [{attributes, RecordInfo},
                             {disc_copies, Nodes},
                             {type, Type},
                             {index, Index}]),
    io:format("Table ~p ~p~n", [Db, mnesia:error_description(R)]).

install() ->
    Nodes = [node() | nodes()],
    mnesia:stop(),
    ok = mnesia:delete_schema(Nodes),
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    create_table(db_fakultet, set, Nodes, record_info(fields, db_fakultet), []),
    create_table(db_fakultet_korisnik,
                 set,
                 Nodes,
                 record_info(fields, db_fakultet_korisnik),
                 []),
    create_table(db_fakultet_katedra,
                 set,
                 Nodes,
                 record_info(fields, db_fakultet_katedra),
                 []),
    create_table(db_katedra, set, Nodes, record_info(fields, db_katedra), []),
    create_table(db_katedra_kolegij, set, Nodes, record_info(fields, db_katedra_kolegij), []),
    create_table(db_katedra_djelatnik,
                 bag,
                 Nodes,
                 record_info(fields, db_katedra_djelatnik),
                 [#db_katedra_djelatnik.id_djelatnik]),
    create_table(db_kolegij, set, Nodes, record_info(fields, db_kolegij), []),
    create_table(db_djelatnik_kolegij,
                 set,
                 Nodes,
                 record_info(fields, db_djelatnik_kolegij),
                 []),
    create_table(db_student_kolegij, set, Nodes, record_info(fields, db_student_kolegij), []),
    create_table(db_kolegij_sekcija,
                 bag,
                 Nodes,
                 record_info(fields, db_kolegij_sekcija),
                 [#db_kolegij_sekcija.id_sekcija]),
    create_table(db_sekcija, set, Nodes, record_info(fields, db_sekcija), []),
    create_table(db_sekcija_sadrzaj, bag, Nodes, record_info(fields, db_sekcija_sadrzaj), []),
    create_table(db_sadrzaj, set, Nodes, record_info(fields, db_sadrzaj), []),
    create_table(db_kviz, set, Nodes, record_info(fields, db_kviz), []),
    create_table(db_kviz_student,
                 bag,
                 Nodes,
                 record_info(fields, db_kviz_student),
                 [#db_kviz_student.id_student]),
    create_table(db_korisnik,
                 set,
                 Nodes,
                 record_info(fields, db_korisnik),
                 [#db_korisnik.oib, #db_korisnik.email]),

    create_table(db_pitanje, set, Nodes, record_info(fields, db_pitanje), []),

    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start().
