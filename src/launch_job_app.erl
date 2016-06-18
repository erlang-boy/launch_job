-module(launch_job_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                                         {'_', [{"/", hello_handler, []}]}
                                     ]),

    {ok, _} = cowboy:start_clear(http, 100, [{port, 8001}], #{
        env => #{dispatch => Dispatch}}),
    launch_job_sup:start_link().

stop(_State) ->
    ok.
