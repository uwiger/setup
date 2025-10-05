%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%% Copyright 2014-2016 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%=============================================================================
%% @private
-module(setup_app).
-behaviour(application).

-export([start/2,
         start_phase/3,
         stop/1]).

start(_Type, _Args) ->
    setup_sup:start_link().

start_phase(run_setup, _Type, []) ->
    case application:get_env(setup, auto_run_phases, true) of
        true ->
            case setup_srv:run_setup() of
                ok ->
                    maybe_stop();
                {error, _} ->
                    stop_node_(0)
            end;
        false ->
            ignore
    end,
    ok.

stop(_) ->
    ok.

maybe_stop() ->
    Mode = setup:mode(),
    case setup:get_env(setup, stop_when_done, false) of
        true when Mode =/= normal ->
            stop_node();
        _ ->
            ok
    end.

stop_node() ->
    spawn_link(fun stop_node_/0).

stop_node_() ->
    StopDelay = setup:get_env(setup, stop_delay, 5000),
    stop_node_(StopDelay).

stop_node_(Delay) ->
    error_logger:info_msg("Setup stopping...(Delay=~p)~n", [Delay]),
    timer:sleep(Delay),
    rpc:eval_everywhere(init,stop,[0]).
