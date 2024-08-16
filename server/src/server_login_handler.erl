-module(server_login_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_type_provided/2, content_types_accepted/2,
         from_json/2, to_json/2, charsets_provided/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

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
    case database:prijava(
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
                    case Reason of
                        lozinka_nije_ispravna ->
                            request:err(404, "Lozinka nije ispravna", Req, State);
                        korisnik_ne_postoji ->
                            request:err(404, "Korisnik ne postoji", Req, State)
                    end
            end;
        {aborted, _} ->
            request:err(500, "Database error", Req, State)
    end.
