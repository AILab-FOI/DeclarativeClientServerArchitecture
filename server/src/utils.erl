-module(utils).

-export([err/4, gather_json/1, gather_html/1, jwt_key/0]).

jwt_key() ->
    <<"ABXw3mqeOO40n/SzaU31lD2ENJePJ1Mr9ZQE1ZBBQAU=">>.

err(Code, Reason, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Reason])),
    Err = #{type => error, message => Formatted},
    Body = json:encode(#{errors => [Err]}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.

gather_json(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Map = json:decode(Body),
    {ok, Map, Req2}.


gather_html(Req) ->
    {ok, _, Req2} = cowboy_req:read_body(Req),
    Bindings = cowboy_req:bindings(Req2),
    {ok, Bindings, Req2}.
