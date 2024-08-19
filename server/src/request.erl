-module(request).

-export([send_response/3, err/4]).

send_response(Req, Data, State) ->
    Body = json:encode(#{data => Data}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(200, Req2),
    {stop, Reply, State}.

err(Code, Reason, Req, State) ->
    io:format("~p~n", [Reason]),
    Formatted = iolist_to_binary(Reason),
    Body = json:encode(#{data => Formatted}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
