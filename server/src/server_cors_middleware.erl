-module(server_cors_middleware).

%% API.
-export([execute/2]).

%% API.

-spec execute(Req, Env) -> {ok | stop, Req, Env}
    when Req :: cowboy_req:req(),
         Env :: any().
execute(#{headers := #{<<"origin">> := HeaderVal}} = Req, Env) ->
    %% NOTE: we assume we always deal with single origin
    handle_cors_request(HeaderVal, Req, Env);
execute(Req, Env) ->
    {ok, Req, Env}.

-spec handle_cors_request(binary(), cowboy_req:req(), any()) ->
                             {ok | stop, cowboy_req:req(), any()}.
handle_cors_request(Origin, #{method := Method} = Req, Env) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
    Req3 = cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, Req2),
    Req4 = cowboy_req:set_resp_header(<<"allow">>, <<"GET, PUT, POST, OPTIONS">>, Req3),
    case Method of
        <<"OPTIONS">> ->
            Req5 =
                cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                           <<"GET, PUT, POST, OPTIONS">>,
                                           Req4),
            Req6 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"*">>, Req5),
            Req8 =
                cowboy_req:set_resp_header(<<"access-control-alow-credentials">>, <<"true">>, Req6),
            Req9 = cowboy_req:reply(200, Req8),
            {stop, Req9, Env};
        _ ->
            {ok, Req4, Env}
    end.
