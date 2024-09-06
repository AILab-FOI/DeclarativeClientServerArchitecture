-module(server_jwt_refresh_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_type_provided/2, content_types_accepted/2,
         charsets_provided/2, from_json/2, to_json/2]).

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
            run_put_request(Map, Req2, State)
    end.

run_put_request(Map, Req, State) ->
    case jwt_manager:refresh_tokens(
             maps:get(<<"refresh_token">>, Map))
    of
        {ok, AccessToken, RefreshToken} ->
            request:send_response(Req,
                                  #{access_token => AccessToken, refresh_token => RefreshToken},
                                  State);
        {error, _} ->
            cowboy_req:reply(401, Req)
    end.
