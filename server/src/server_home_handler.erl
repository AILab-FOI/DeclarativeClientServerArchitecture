-module(server_home_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Home">>, Req0),
    {ok, Req, State}.
