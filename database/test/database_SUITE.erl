-module(database_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).

all() ->
  [].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  database:install([node()]),
  application:start(mnesia),
  application:start(database),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.
