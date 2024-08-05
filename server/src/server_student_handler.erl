-module(server_student_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         from_json/2, content_type_provided/2, to_json/2, from_html/2, to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json},
      {{<<"application">>, <<"html">>, []}, from_html}],
     Req,
     State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json},
      {{<<"application">>, <<"html">>, []}, to_html}],
     Req,
     State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

from_json(Req, State) ->
    json_request(Req, State).

to_json(Req, State) ->
    json_request(Req, State).

from_html(Req, State) ->
    html_request(Req, State).

to_html(Req, State) ->
    html_request(Req, State).

html_request(Req, State) ->
    case gather_html(Req) of
        {error, Reason, _} ->
            err(400, Reason, Req, State);
        {ok, #{id := Id}, Req2} ->
            run_get_request(Req2, Id, State);
        {ok, _, Req2} ->
            run_get_all_request(Req2, State)
    end.

json_request(Req, State) ->
    case gather_json(Req) of
        {error, Reason, _} ->
            err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(Map, Req, State) ->
    case database:dodaj_studenta(
             maps:get(<<"ime">>, Map),
             maps:get(<<"prezime">>, Map),
             maps:get(<<"oib">>, Map),
             maps:get(<<"lozinka">>, Map))
    of
        {unable_to_insert, Reason} ->
            {_, Reply, _} = err(400, Reason, Req, State);
        {done, Result} ->
            Body = jiffy:encode(#{data => Result}),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Reply = cowboy_req:reply(200, Req2)
    end,
    {stop, Reply, State}.

run_get_all_request(Req, State) ->
    Result = database:dohvati_studente(),
    Body = jiffy:encode(#{data => Result}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(200, Req2),
    {stop, Reply, State}.

run_get_request(Req, Id, State) ->
    {atomic, Result} = database:dohvati_studenta(Id),
    Body = jiffy:encode(#{data => Result}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(200, Req2),

    {stop, Reply, State}.

gather_json(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Map = jiffy:decode(Body, [return_maps]),
    {ok, Map, Req2}.

gather_html(Req) ->
    {ok, _, Req2} = cowboy_req:read_body(Req),
    Bindings = cowboy_req:bindings(Req2),
    io:format("~p~n", [Bindings]),
    {ok, Bindings, Req2}.

err(Code, Reason, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Reason])),
    Err = #{type => error, message => Formatted},
    Body = jiffy:encode(#{errors => [Err]}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
