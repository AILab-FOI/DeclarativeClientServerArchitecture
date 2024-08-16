-module(server_student_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, from_json/2, content_type_provided/2, to_json/2, from_html/2, to_html/2,
         delete_resource/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"GET">>, <<"DELETE">>], Req, State}.

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
    {[{{<<"application">>, <<"json">>, []}, from_json},
      {{<<"application">>, <<"html">>, []}, from_html}],
     Req,
     State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json},
      {{<<"application">>, <<"html">>, []}, to_html}],
     Req,
     State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    case utils:gather_json(Req) of
        {ok, Map, Req2} ->
            case database:obrisi_studenta(
                     maps:get(<<"id">>, Map))
            of
                {atomic, ok} ->
                    request:send_response(Req2, <<"ok">>, State);
                {aborted, Reason} ->
                    io:format("~p", Reason),
                    request:err(400, "Greška", Req, State)
            end;
        _ ->
            request:err(400, "Db Error", Req, State)
    end,
    {stop, Req, State}.

from_json(Req, State) ->
    json_request(Req, State).

to_json(Req, State) ->
    json_request(Req, State).

from_html(Req, State) ->
    html_request(Req, State).

to_html(Req, State) ->
    html_request(Req, State).

html_request(Req, State) ->
    case utils:gather_html(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, #{id := Id}, Req2} ->
            run_get_request(Req2, Id, State);
        {ok, _, Req2} ->
            run_get_all_request(Req2, State)
    end.

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(Map, Req, State) ->
    case database:dodaj_studenta(
             maps:get(<<"ime">>, Map),
             maps:get(<<"prezime">>, Map),
             maps:get(<<"oib">>, Map),
             maps:get(<<"lozinka">>, Map),
             maps:get(<<"email">>, Map),
             maps:get(<<"opis">>, Map, <<"">>))
    of
        {unable_to_insert, Reason} ->
            request:err(403, Reason, Req, State);
        {done, Result} ->
            request:send_response(Req, Result, State)
    end.

run_get_all_request(Req, State) ->
    Result = database:dohvati_studente(),
    request:send_response(Req, Result, State).

run_get_request(Req, Id, State) ->
    case database:dohvati_korisnika(binary_to_integer(Id)) of
        {atomic, Result} ->
            case Result of
                {error, _} ->
                    request:err(404, "Korisnik ne postoji", Req, State);
                _ ->
                    request:send_response(Req, Result, State)
            end;
        {aborted, _} ->
            request:err(404, "Database error", Req, State)
    end.
