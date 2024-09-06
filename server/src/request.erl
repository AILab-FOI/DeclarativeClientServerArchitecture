-module(request).

-export([send_response/3, err/4, auth/2, delete/3, response/3]).

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

auth(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {{false, <<"Bearer token_type=\"JWT\"">>}, Req, State};
        <<"Bearer ", Token/binary>> ->
            case jwt_manager:verify_access_token(Token) of
                {ok, _} ->
                    {true, Req, State};
                {error, _} ->
                    {{false, <<"Bearer token_type=\"JWT\"">>}, Req, State}
            end
    end.

delete(Req, State, Fun) ->
    case utils:gather_json(Req) of
        {ok, Map, Req2} ->
            case Fun(maps:get(<<"id">>, Map)) of
                {atomic, ok} ->
                    send_response(Req2, <<"ok">>, State);
                {aborted, Reason} ->
                    err(400, Reason, Req, State)
            end;
        _ ->
            err(400, "Db Error", Req, State)
    end.

response(Req, State, Fun) ->
    case Fun() of
        {atomic, Result} ->
            case Result of
                {error, Reason} ->
                    err(403, Reason, Req, State);
                {ok, Id} ->
                    send_response(Req, Id, State);
                _ ->
                    send_response(Req, Result, State)
            end;
        {aborted, Reason} ->
            err(403, Reason, Req, State)
    end.
