-module(server_section_content_handler).

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
    request:delete(Req, State, fun(Id) -> sekcija_sadrzaj:obrisi_sadrzaj_na_sekciji(Id) end).

from_json(Req, State) ->
    json_request(Req, State).

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(#{<<"sekcija">> := Sekcija, <<"sadrzaj">> := Sadrzaj}, Req, State) ->
    request:response(Req,
                     State,
                     fun() -> sekcija_sadrzaj:dodaj_sadrzaj_na_sekciju(Sadrzaj, Sekcija) end);
run_put_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).
