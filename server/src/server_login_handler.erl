-module(server_login_handler).

-import(database, [dodaj_studenta/4]).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         from_json/2, content_type_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

from_json(Req, State) ->
    json_request(Req, State).

to_json(Req, State) ->
    json_request(Req, State).

json_request(Req, State) ->
    case gather(Req) of
        {error, Reason} ->
            err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_request(Map, Req2, State)
    end.

run_request(Map, Req, State) ->
    Body = jiffy:encode(Map),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    database:dodaj_studenta(<<"">>, <<"">>, <<"">>, <<"">>),
    io:format(<<"ok">>),
    Reply = cowboy_req:reply(200, Req2),
    {stop, Reply, State}.

gather(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Map = jiffy:decode(Body, [return_maps]),
    {ok, Map, Req2}.

err(Code, Reason, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Reason])),
    Err = #{type => error, message => Formatted},
    Body = jiffy:encode(#{errors => [Err]}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
