-module(server_login_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    {ok, Data, _Req} = cowboy_req:read_body(Req0),
    cowboy_req:reply(200, ok).

terminate(_Reason, _Req, _State) ->
    ok.
