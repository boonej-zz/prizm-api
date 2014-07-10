-module(prism_listener_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                {"/callback", callback_handler, []}
            ]
        }
    ]),

    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    prism_listener_sup:start_link().
stop(_State) ->
    ok.
