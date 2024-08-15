-module(server_student_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, from_json/2, content_type_provided/2, to_json/2, from_html/2,
         to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"GET">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {{false, <<"Bearer token_type=\"JWT\"">>}, Req, State};
        _ ->
            {true, Req, State}
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
            utils:err(400, Reason, Req, State);
        {ok, #{id := Id}, Req2} ->
            run_get_request(Req2, Id, State);
        {ok, _, Req2} ->
            run_get_all_request(Req2, State)
    end.

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            utils:err(400, Reason, Req, State);
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
             maps:get(<<"opis">>, Map))
    of
        {unable_to_insert, Reason} ->
            {_, Reply, _} = utils:err(400, Reason, Req, State);
        {done, Result} ->
            Body = json:encode(#{data => Result}),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Reply = cowboy_req:reply(200, Req2)
    end,
    {stop, Reply, State}.

run_get_all_request(Req, State) ->
    Result = database:dohvati_studente(),
    Body = json:encode(#{data => Result}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(200, Req2),
    {stop, Reply, State}.

run_get_request(Req, Id, State) ->
    {atomic, Result} = database:dohvati_studenta(binary_to_integer(Id)),
    Body = json:encode(#{data => Result}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(200, Req2),

    {stop, Reply, State}.
