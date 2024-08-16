-module(server_http).

-export([start/0, stop/0]).

start() ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/home/", server_home_handler, []},
                                 {"/student/", server_student_handler, []},
                                 {"/student/[:id]", server_student_handler, []},
                                 {"/login/", server_login_handler, []},
                                 {"/jwt/refresh", server_jwt_refresh_handler, []}]}]),

    cowboy:start_clear(server_http_listener,
                       [{port, 5000}],
                       #{middlewares => [server_cors_middleware, cowboy_router, cowboy_handler],
                         env => #{dispatch => Dispatch}}).

stop() ->
    ok = cowboy:stop_listener(server_http_listener).
