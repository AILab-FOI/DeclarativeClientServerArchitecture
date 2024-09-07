-module(server_department_worker_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, charsets_provided/2, is_authorized/2,
         content_types_accepted/2, delete_resource/2, from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"POST">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
    request:auth(Req, State).

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

delete_resource(Req, State) ->
    case utils:gather_json(Req) of
        {ok, Map, Req2} ->
            case katedra_djelatnik:obrisi_djelatnika_na_katedri(
                     maps:get(<<"id_djelatnik">>, Map), maps:get(<<"id_katedra">>, Map))
            of
                {atomic, ok} ->
                    request:send_response(Req2, <<"ok">>, State);
                {aborted, Reason} ->
                    request:err(400, Reason, Req, State)
            end;
        _ ->
            request:err(400, "Db Error", Req, State)
    end.

from_json(Req, State) ->
    json_request(Req, State).

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(#{<<"djelatnik">> := Djelatnik,
                  <<"katedra">> := Katedra,
                  <<"tip">> := Tip},
                Req,
                State) ->
    request:response(Req,
                     State,
                     fun() -> katedra_djelatnik:dodaj_djelatnika_na_katedru(Djelatnik, Katedra, Tip)
                     end);
run_put_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).
