-module(server_http).

-export([start/0, stop/0]).

start() ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/home/", server_home_handler, []},
                                 {"/faculty/[:id]", server_faculty_handler, []},
                                 {"/faculty/department/", server_faculty_department_handler, []},
                                 {"/faculty/user/", server_faculty_user_handler, []},
                                 {"/department/[:id]", server_department_handler, []},
                                 {"/department/worker/", server_department_worker_handler, []},
                                 {"/department/course/", server_department_course_handler, []},
                                 {"/quiz/[:id]", server_quiz_handler, []},
                                 {"/quiz/student", server_quiz_student_handler, []},
                                 {"/student/course/[[:student]/[:course]]",
                                  server_student_course_handler,
                                  []},
                                 {"/student/", server_student_handler, []},
                                 {"/worker/", server_worker_handler, []},
                                 {"/worker/course/[[:worker]/[:course]]",
                                  server_worker_course_handler,
                                  []},
                                 {"/course/[:id]", server_course_handler, []},
                                 {"/course/section/", server_course_section_handler, []},
                                 {"/section/[:id]", server_section_handler, []},
                                 {"/section/content/", server_section_content_handler, []},
                                 {"/content/[:id]", server_content_handler, []},
                                 {"/user/[:id]", server_user_handler, []},
                                 {"/question/[:id]", server_question_handler, []},
                                 {"/login/", server_login_handler, []},
                                 {"/images/[...]", cowboy_static, {priv_dir, server, "assets"}},
                                 {"/jwt/refresh", server_jwt_refresh_handler, []}]}]),

    cowboy:start_clear(server_http_listener,
                       [{port, 5000}],
                       #{middlewares => [server_cors_middleware, cowboy_router, cowboy_handler],
                         env => #{dispatch => Dispatch}}).

stop() ->
    ok = cowboy:stop_listener(server_http_listener).
