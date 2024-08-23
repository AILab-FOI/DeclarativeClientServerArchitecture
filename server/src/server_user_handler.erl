-module(server_user_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, charsets_provided/2, is_authorized/2, to_html/2,
         delete_resource/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
    request:auth(Req, State).

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    request:delete(Req, State, fun(Id) -> korisnik:obrisi_korisnika(Id) end).

to_html(Req, State) ->
    html_request(Req, State).

html_request(Req, State) ->
    case utils:gather_html(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, #{id := Id}, Req2} ->
            request:response(Req2,
                             State,
                             fun() -> korisnik:dohvati_korisnika(kolegiji, binary_to_integer(Id))
                             end);
        {ok, _, Req2} ->
            request:response(Req2, State, fun() -> korisnik:dohvati_korisnike() end)
    end.
