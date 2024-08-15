-module(server_verify_handler).

-behaviour(cowboy_handler).

-export([init/2, is_authorized/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

% allowed_methods(Req, State) ->
%     {[<<"GET">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {{false, <<"Bearer token_type=\"JWT\"">>}, Req, State};
        Auth ->
            case utils:verify(Auth, Req, State) of
                {ok, Payload} ->
                    case is_user(Payload, Req) of
                        {ok, JwtId} ->
                            Body = json:encode(#{verified => JwtId}),
                            Req2 = cowboy_req:set_resp_body(Body, Req),
                            Reply = cowboy_req:reply(200, Req2),
                            {stop, Reply, State};
                        {error} ->
                            Reply = cowboy_req:reply(401, Req),
                            {stop, Reply, State}
                    end;
                {error, Reason} ->
                    utils:err(400, Reason, Req, State)
            end
    end.

get_jwt(Jwt) ->
    case Jwt of
        [#{sub := Id, iat := _}] ->
            {ok, Id};
        _ ->
            {error, no_jwt}
    end.

is_user(Jwt, Req) ->
    case utils:gather_html(Req) of
        {ok, #{id := Id}, _} ->
            case get_jwt(Jwt) of
                {ok, JwtId} ->
                    case binary_to_integer(Id) == JwtId of
                        true ->
                            {ok, JwtId};
                        false ->
                            {error}
                    end;
                _ ->
                    {error}
            end;
        _ ->
            {error}
    end.
