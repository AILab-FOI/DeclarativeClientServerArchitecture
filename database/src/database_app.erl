%%%-------------------------------------------------------------------
%% @doc database public API
%% @end
%%%-------------------------------------------------------------------

-module(database_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    database_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
