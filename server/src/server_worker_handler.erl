-module(server_worker_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"PATCH">>], Req, State}.

is_authorized(Req, State) ->
    request:auth(Req, State).

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

from_json(Req, State) ->
    json_request(Req, State).

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

run_put_request(#{<<"ime">> := Ime,
                  <<"prezime">> := Prezime,
                  <<"oib">> := Oib,
                  <<"lozinka">> := Lozinka,
                  <<"email">> := Email,
                  <<"opis">> := Opis,
                  <<"kabinet">> := Kabinet},
                Req,
                State) ->
    request:response(Req,
                     State,
                     fun() ->
                        korisnik:dodaj_djelatnika(Ime, Prezime, Oib, Lozinka, Email, Opis, Kabinet)
                     end);
run_put_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).

run_patch_request(#{<<"id">> := Id,
                    <<"ime">> := Ime,
                    <<"prezime">> := Prezime,
                    <<"oib">> := Oib,
                    <<"lozinka">> := Lozinka,
                    <<"email">> := Email,
                    <<"opis">> := Opis,
                    <<"kabinet">> := Kabinet},
                  Req,
                  State) ->
    request:response(Req,
                     State,
                     fun() ->
                        korisnik:uredi_djelatnika(Id,
                                                  Ime,
                                                  Prezime,
                                                  Oib,
                                                  Lozinka,
                                                  Email,
                                                  Opis,
                                                  Kabinet)
                     end);
run_patch_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).
