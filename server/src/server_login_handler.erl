-module(server_login_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_type_provided/2, content_types_accepted/2,
         from_json/2, to_json/2, charsets_provided/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

from_json(Req, State) ->
    json_request(Req, State).

to_json(Req, State) ->
    json_request(Req, State).

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_post_request(Map, Req2, State)
    end.

run_post_request(Map, Req, State) ->
    case user:prijava(
             maps:get(<<"email">>, Map), maps:get(<<"password">>, Map))
    of
        {atomic, Result} ->
            case Result of
                {ok, #{id := Id}} ->
                    {AccessToken, RefreshToken} = jwt_manager:generate_tokens(Id),
                    request:send_response(Req,
                                          #{access_token => AccessToken,
                                            refresh_token => RefreshToken},
                                          State);
                {error, Reason} ->
                    request:err(404, Reason, Req, State)
            end;
        {aborted, _} ->
            request:err(500, "Database error", Req, State)
    end.
