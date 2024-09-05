-module(server_faculty_handler).

-export([init/2, allowed_methods/2, charsets_provided/2, is_authorized/2,
         content_type_provided/2, content_types_accepted/2, to_html/2, delete_resource/2,
         from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PATCH">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            {true, Req, State};
        _ ->
            request:auth(Req, State)
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_html}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    request:delete(Req, State, fun(Id) -> fakultet:obrisi(Id) end).

to_html(Req, State) ->
    html_request(Req, State).

from_json(Req, State) ->
    json_request(Req, State).

html_request(Req, State) ->
    case utils:gather_html(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, #{id := Id}, Req2} ->
            request:response(Req2, State, fun() -> fakultet:dohvati(binary_to_integer(Id)) end);
        {ok, _, Req2} ->
            request:response(Req2, State, fun() -> fakultet:dohvati() end)
    end.

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            gather_method(Map, Req2, State)
    end.

gather_method(Map, Req, State) ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            run_put_request(Map, Req, State);
        <<"PATCH">> ->
            run_patch_request(Map, Req, State)
    end.

run_put_request(#{<<"naziv">> := Naziv,
                  <<"opis">> := Opis,
                  <<"lat">> := Lat,
                  <<"long">> := Long,
                  <<"adresa">> := #{<<"ulica">> := _} = Adresa},
                Req,
                State) ->
    request:response(Req,
                     State,
                     fun() -> fakultet:dodaj(Naziv, Opis, Adresa, {Lat, Long}) end);
run_put_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).

run_patch_request(#{<<"id">> := Id,
                    <<"naziv">> := Naziv,
                    <<"opis">> := Opis,
                    <<"logo">> := Logo,
                    <<"adresa">> := #{<<"ulica">> := _} = Adresa},
                  Req,
                  State) ->
    request:response(Req, State, fun() -> fakultet:uredi(Id, Naziv, Opis, Adresa, Logo) end);
run_patch_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).
