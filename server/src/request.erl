-module(request).

-export([send_response/3, err/4]).

send_response(Req, Data, State) ->
    Body = json:encode(#{data => Data}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(200, Req2),
    {stop, Reply, State}.

err(Code, Reason, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Reason])),
    Body = json:encode(#{data => Formatted}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
