-module(server_login_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Data, _Req} = cowboy_req:read_body(Req0),
    Req = cowboy_req:reply(200, #{}, Data, Req0),
    Obj = jiffy:decode(Data, [return_maps]),
    Ime = maps:get(<<"ime">>, Obj),
    Asd = maps:get(<<"asd">>, Obj),
    io:format(Asd),
    io:format(Ime),

    {ok, Req, State}.
