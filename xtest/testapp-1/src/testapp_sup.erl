
-module(testapp_sup).

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
    %% {ok, { {one_for_one, 5, 10}, [?CHILD(testapp_p1, worker)]} }.
    %% cheating just to get a process running testapp_p1
    %% Unfortunately, if we add it as a supervisor child, the supervisor
    %% code_change() will carry that childspec over to the next version,
    %% which will cause an 'undef' exception when it tries to restart
    %% the process after the brutal purge.
    testapp_p1:start_link(),
    {ok, { {one_for_one, 5, 10}, []} }.
