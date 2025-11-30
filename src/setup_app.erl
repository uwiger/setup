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

-include_lib("kernel/include/logger.hrl").

start(_Type, _Args) ->
    case setup_zomp:is_zomp_context() of
        true ->
            setup_zomp:update_env(),
            load_setup_conf(setup_zomp:setup_conf_path());
        false ->
            load_setup_conf(default_setup_conf_path())
    end,
    setup_sup:start_link().

start_phase(run_setup, _Type, []) ->
    case application:get_env(setup, auto_run_phases, true) of
        true ->
            _ = setup_srv:run_setup();
        false ->
            ignore
    end,
    ok.

stop(_) ->
    ok.

default_setup_conf_path() ->
    ["."].

load_setup_conf(Path) ->
    case setup:get_env(setup, conf) of
        {ok, F} ->
            load_setup_conf_(F);
        undefined ->
            try_path_load(Path)
    end.

load_setup_conf_(F) ->
    case lists:reverse(F) of
        "tpircs.gifnoc." ++ _ ->   %% .config.script
            Cfg = ok(setup_file:script(F, script_env()), script, F),
            process_conf(Cfg, script, F);
        "gifnoc." ++ _ ->          %% .config
            Cfg = ok(file:consult(F), consult, F),
            process_conf(Cfg, consult, F);
        _ ->
            ?LOG_WARNING("Unusual setup conf filename (~s), will try file:consult()", [F]),
            Cfg = ok(file:consult(F), consult, F),
            process_conf(Cfg, consult, F)
    end.

try_path_load(Path) ->
    case setup_file:path_script(Path, "setup.config.script") of
        {error, enoent} ->
            case setup_file:path_consult(Path, "setup.config") of
                {ok, List, Full} ->
                    process_conf(List, consult, Full);
                {error, enoent} ->
                    ok
            end;
        {ok, Cfg, Full} ->
            process_conf(Cfg, script, Full)
    end.

script_env() ->
    [].

process_conf(Cfg, _Op, _F) ->
    lists:foreach(
      fun({App, Vars}) ->
              lists:foreach(
                fun({K, V}) ->
                        application:set_env(App, K, V)
                end, Vars)
      end, Cfg).

ok({ok, Value}, _, _) ->
    Value;
ok(Error, Op, F) ->
    error({unexpected, {Error, Op, F}}).
