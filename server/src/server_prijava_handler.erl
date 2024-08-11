-module(server_prijava_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_type_provided/2, content_types_accepted/2, from_json/2, to_json/2, charsets_provided/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}],
     Req,
     State}.

content_type_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req,
     State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

from_json(Req, State) ->
    json_request(Req, State).

to_json(Req, State) ->
    json_request(Req, State).

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            utils:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(Map, Req, State) ->
    case database:prijava_studenta(
             maps:get(<<"email">>, Map),
             maps:get(<<"lozinka">>, Map))
    of
        {atomic, {error, Reason}} ->
            {_, Reply, _} = utils:err(400, Reason, Req, State);
        {atomic, {ok, #{id := Id}}} ->
            io:format("~p", [Id]),
            Claims = #{sub => Id, iat => calendar:datetime_to_gregorian_seconds(calendar:universal_time())},
            Jwt = jwerl:sign([Claims], hs256, utils:jwt_key()),
            Body = json:encode(#{data => Jwt}),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Reply = cowboy_req:reply(200, Req2)
    end,
    {stop, Reply, State}.


