
-module(prism_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    InstagramProcessor = ?CHILD(prism_instagram_process, worker),
    PrismAuth = ?CHILD(prism_auth, worker),
    PostSupervisor = ?CHILD(prism_post_supervisor, supervisor),
    {ok, { {one_for_one, 5, 10}, [PrismAuth, PostSupervisor, InstagramProcessor]} }.

