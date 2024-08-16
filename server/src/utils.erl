-module(utils).

-export([gather_json/1, gather_html/1]).

gather_json(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Map = json:decode(Body),
    {ok, Map, Req2}.

gather_html(Req) ->
    {ok, _, Req2} = cowboy_req:read_body(Req),
    Bindings = cowboy_req:bindings(Req2),
    {ok, Bindings, Req2}.
