-module(server_cors_middleware).

-export([execute/2]).

-spec execute(Req, Env) -> {ok | stop, Req, Env}
    when Req :: cowboy_req:req(),
         Env :: any().
execute(#{headers := #{<<"origin">> := HeaderVal}} = Req, Env) ->
    handle_cors_request(HeaderVal, Req, Env);
execute(Req, Env) ->
    {ok, Req, Env}.

-spec handle_cors_request(binary(), cowboy_req:req(), any()) ->
                             {ok | stop, cowboy_req:req(), any()}.
handle_cors_request(Origin, #{method := Method} = Req, Env) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req3 = cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, Req2),
    case Method of
        <<"OPTIONS">> ->
            Req4 =
                cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                           <<"GET, DELETE, PUT, POST, PATCH">>,
                                           Req3),
            Req5 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"*">>, Req4),
            Req6 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"0">>, Req5),
            Req7 = cowboy_req:reply(200, Req6),
            {stop, Req7};
        _ ->
            {ok, Req3, Env}
    end.
