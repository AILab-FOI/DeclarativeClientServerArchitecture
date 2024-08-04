%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = server_http:start(),
    % ok = database:install([node()]),
    server_sup:start_link().

stop(_State) ->
    server_http:stop().

%% internal functions
