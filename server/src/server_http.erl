-module(server_http).

-export([start/0, stop/0]).

start() ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/home/", server_home_handler, []},
                                 {"/register/", server_register_handler, []}]}]),
    cowboy:start_clear(server_http_listener,
                       [{port, 5000}],
                       #{middlewares => [cowboy_router, cowboy_handler],
                         env => #{dispatch => Dispatch}}).

stop() ->
    ok = cowboy:stop_listener(server_http_listener).
