-module(server_course_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, content_type_provided/2, from_html/2, to_html/2, delete_resource/2,
         from_json/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

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
    {[{{<<"application">>, <<"html">>, []}, from_html},
      {{<<"application">>, <<"json">>, []}, from_json}],
     Req,
     State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"html">>, []}, to_html},
      {{<<"application">>, <<"json">>, []}, to_json}],
     Req,
     State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    case utils:gather_json(Req) of
        {ok, Map, Req2} ->
            case user:obrisi_korisnika(
                     maps:get(<<"id">>, Map))
            of
                {atomic, ok} ->
                    request:send_response(Req2, <<"ok">>, State);
                {aborted, _} ->
                    request:err(400, "Greška", Req, State)
            end;
        _ ->
            request:err(400, "Db Error", Req, State)
    end,
    {stop, Req, State}.

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

run_get_all_request(Req, State) ->
    {atomic, Result} = kolegij:dohvati(),
    request:send_response(Req, Result, State).

run_get_request(Req, Id, State) ->
    case kolegij:dohvati(binary_to_integer(Id)) of
        {atomic, Result} ->
            case Result of
                {error, Reason} ->
                    request:err(404, Reason, Req, State);
                _ ->
                    request:send_response(Req, Result, State)
            end;
        {aborted, Reason} ->
            request:err(404, Reason, Req, State)
    end.

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
    case kolegij:dodaj(
             maps:get(<<"naziv">>, Map), maps:get(<<"skraceno">>, Map))
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
