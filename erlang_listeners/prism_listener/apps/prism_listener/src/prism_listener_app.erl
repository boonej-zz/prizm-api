-module(prism_listener_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    Dispatch = cowboy_router:compile([
        {'_', [{"/callback", callback_handler, []}]}
    ]),

    cowboy:start_http(callback_listener, 2, [{port, 8080}, {env, [{dispatch, Dispatch}]}]),

    prism_listener_sup:start_link().

stop(_State) ->
    ok.
