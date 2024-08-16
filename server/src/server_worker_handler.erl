-module(server_worker_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, from_json/2, content_type_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

is_authorized(Req, State) ->
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
    case user:dodaj_djelatnika(
             maps:get(<<"ime">>, Map),
             maps:get(<<"prezime">>, Map),
             maps:get(<<"oib">>, Map),
             maps:get(<<"lozinka">>, Map),
             maps:get(<<"email">>, Map),
             maps:get(<<"opis">>, Map, <<"">>))
    of
        {atomic, Result} ->
            case Result of
                {error, Reason} ->
                    request:err(403, Reason, Req, State);
                {ok, Id} ->
                    request:send_response(Req, Id, State)
            end;
        {aborted, Reason} ->
            request:err(403, Reason, Req, State)
    end.
