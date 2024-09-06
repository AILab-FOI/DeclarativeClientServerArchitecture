-module(server_faculty_user_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, charsets_provided/2, is_authorized/2,
         content_type_provided/2, content_types_accepted/2, delete_resource/2, from_json/2,
         to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PATCH">>, <<"POST">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
    request:auth(Req, State).

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_html}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    request:delete(Req,
                   State,
                   fun(Id) -> fakultet_korisnik:obrisi_korisnika_na_fakultetu(Id) end).

from_json(Req, State) ->
    json_request(Req, State).

to_html(Req, State) ->
    html_request(Req, State).

html_request(Req, State) ->
    case utils:gather_html(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, #{id := Id}, Req2} ->
            request:response(Req2,
                             State,
                             fun() -> fakultet_korisnik:dohvati_korisnike(binary_to_integer(Id))
                             end);
        _ ->
            request:err(400, <<"Bad Request">>, Req, State)
    end.

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(#{<<"fakultet">> := Fakultet, <<"korisnik">> := Korisnik}, Req, State) ->
    request:response(Req,
                     State,
                     fun() -> fakultet_korisnik:dodaj_korisnika_na_fakultet(Korisnik, Fakultet)
                     end);
run_put_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).
