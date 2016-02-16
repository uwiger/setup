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
-module(setup_srv).
-behaviour(gen_server).

-export([start_link/0]).
-export([run_setup/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_setup() ->
    gen_server:call(?MODULE, run_setup).

init(_) ->
    {ok, []}.

handle_call(run_setup, _From, S) ->
    {reply, setup:run_setup(), S};
handle_call(_, _, S) ->
    {reply, {error, badarg}, S}.

handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_  , _) -> ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.
