%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

%% @doc Setup utility for erlang applications
-module(setup).
-behaviour(application).

-export([start/2,
         stop/1]).

-export([home/0,
         log_dir/0,
         data_dir/0,
         verify_directories/0,
         verify_dir/1,
         find_env_vars/1]).

-export([run_setup/2]).

%% @spec start(Type, Args) -> {ok, pid()}
%% @doc Application start function.
%% @end
%%
start(_, Args) ->
    proc_lib:start_link(?MODULE, run_setup, [self(), Args]).

%% @spec stop(State) -> ok
%% @doc Application stop function
%% end
%%
stop(_) ->
    ok.

%% @spec home() -> Directory
%% @doc Returns the configured `home' directory (where we started), or a best guess ($CWD).
%% @end
%%
home() ->
    case application:get_env(setup, home) of
        U when U == {ok, undefined};
               U == undefined ->
            {ok, CWD} = file:get_cwd(),
            D = filename:absname(CWD),
            application:set_env(setup, home, D),
            D;
        {ok, D} ->
            D
    end.

%% @spec log_dir() -> Directory
%% @doc Returns the configured log directory, or a best guess (home()/log.Node).
%% @end
%%
log_dir() ->
    setup_dir(log_dir, "log." ++ atom_to_list(node())).

%% @spec log_dir() -> Directory
%% @doc Returns the configured log directory, or a best guess (home()/log.Node).
%% @end
%%
data_dir() ->
    setup_dir(data_dir, "data." ++ atom_to_list(node())).

setup_dir(Key, Default) ->
    case application:get_env(setup, Key) of
        U when U == {ok, undefined};
               U == undefined ->
            D = filename:absname(filename:join(home(), Default)),
            application:set_env(setup, Key, D),
            D;
        {ok, D} ->
            D
    end.

%% @spec verify_directories() -> ok
%% @doc Ensures that essential directories exist and are writable.
%% Currently, only the log directory is verified.
%% @end
%%
verify_directories() ->
    verify_dir(home()),
    verify_dir(log_dir()),
    verify_dir(data_dir()),
    ok.

%% @spec verify_dir(Dir) -> Dir
%% @doc Ensures that the directory Dir exists and is writable.
%% @end
%%
verify_dir(Directory) ->
    ok = filelib:ensure_dir(filename:join(Directory, "dummy")),
    Directory.

%% @spec find_env_vars(Env) -> [{AppName, Value}]
%% @doc Searches all loaded applications for an instance of the `Env' environment variable.
%%
%% The environment variables may contain instances of `$APP', `$PRIV_DIR', `$LIB_DIR',
%% `$DATA_DIR', `$LOG_DIR', `$HOME', inside strings or binaries, and these will be replaced
%% with actual values for the current system (`$APP' simply expands to the name of the
%% current application).
%% @end
find_env_vars(Env) ->
    GEnv = global_env(),
    lists:flatmap(
      fun({A,_,_}) ->
              case application:get_env(A, Env) of
                  {ok, Val} when Val =/= undefined ->
                      NewEnv = GEnv ++ [{K, env_value(K, A)} ||
                                           K <- ["PRIV_DIR", "LIB_DIR", "APP"]],
                      [{A, expand_env(NewEnv, Val)}];
                  _ ->
                      []
              end
      end, application:loaded_applications()).


global_env() ->
    [{K, env_value(K)} || K <- ["DATA_DIR", "LOG_DIR", "HOME"]].

expand_env(Vs, T) when is_tuple(T) ->
    list_to_tuple([expand_env(Vs, X) || X <- tuple_to_list(T)]);
expand_env(Vs, L) when is_list(L) ->
    try list_to_binary(L) of
        _ ->
            do_expand_env(L, Vs, list)
    catch
        error:_ ->
            [expand_env(Vs, X) || X <- L]
    end;
expand_env(Vs, B) when is_binary(B) ->
    do_expand_env(B, Vs, binary);
expand_env(_, X) ->
    X.

do_expand_env(X, Vs, Type) ->
    lists:foldl(fun({K, Val}, Xx) ->
			re:replace(Xx, [$\\, $$ | K], Val, [{return,Type}])
		end, X, Vs).

env_value("LOG_DIR") -> log_dir();
env_value("DATA_DIR") -> data_dir();
env_value("HOME") -> home().

env_value("APP", A) -> atom_to_list(A);
env_value("PRIV_DIR", A) -> code:priv_dir(A);
env_value("LIB_DIR" , A) -> code:lib_dir(A).


%% @hidden
%%
%% Called from the start function. Will verify directories, then call
%% all setup hooks in all applications, and execute them in order.
%% Afterwards, setup will either finish and leave the system running, or
%% stop, terminating all nodes automatically.
%%
run_setup(Parent, _Args) ->
    io:fwrite("Setup running ...~n", []),
    Res = rpc:multicall(?MODULE, verify_directories, []),
    io:fwrite("Directories verified. Res = ~p~n", [Res]),
    proc_lib:init_ack(Parent, {ok, self()}),
    Hooks = find_hooks(),
    io:fwrite("Hooks = ~p~n", [Hooks]),
    run_hooks(Hooks),
    io:fwrite("Setup finished processing hooks ...~n", []),
    case application:get_env(stop_when_done) of
        {ok, true} ->
            io:fwrite("Setup stopping...~n", []),
            timer:sleep(timer:seconds(5)),
            rpc:eval_everywhere(init,stop,[0]);
        _ ->
            timer:sleep(infinity)
    end.

%% @spec find_hooks() -> [{PhaseNo, [{M,F,A}]}]
%% @doc Finds all custom setup hooks in all applications.
%% The setup hooks must be of the form
%% <pre>{'$setup_hooks', [{PhaseNo, {M, F, A}}]}</pre>,
%% where PhaseNo should be (but doesn't have to be) an integer.
%%
%% The hooks will be called in order:
%% - The phase numbers will be sorted.
%% - All hooks for a specific PhaseNo will be called in sequence,
%%   in the same order as the applications appear in the boot script
%%   (and, if included applications exist, in preorder traversal order).
%%
%% A suggested convention is:
%% - Create the database at phase 100
%% - Create tables (or configure schema) at 200
%% - Populate the database at 300
%% @end
%%
find_hooks() ->
    Applications = applications(),
    lists:foldl(
      fun(A, Acc) ->
              case application:get_env(A, '$setup_hooks') of
                  {ok, Hooks} ->
                      lists:foldl(
                        fun({N, {_, _, _} = MFA}, Acc1) ->
                                orddict:append(N, MFA, Acc1)
                        end, Acc, Hooks);
                  _ ->
                      Acc
              end
      end, orddict:new(), Applications).

%% @spec run_hooks(Hooks) -> ok
%% @doc Execute all setup hooks in order.
%% Exceptions are caught and printed. This might/should be improved, but the
%% general idea is to complete as much as possible of the setup, and perhaps
%% repair afterwards. However, the fact that something went wrong should be
%% remembered and reflected at the end.
%% @end
%%
run_hooks(Hooks) ->
    lists:foreach(
      fun({Phase, MFAs}) ->
              io:fwrite("Setup phase ~p~n", [Phase]),
              lists:foreach(fun({M, F, A}) ->
                                    Result = (catch apply(M, F, A)),
                                    MFAString = format_mfa(M, F, A),
                                    io:fwrite(MFAString ++ "-> ~p~n", [Result])
                            end, MFAs)
      end, Hooks).

format_mfa(M, F, A) ->
    lists:flatten([atom_to_list(M),":",atom_to_list(F),
                   "(", format_args(A), ")"]).

format_args([])         -> "";
format_args([A])        -> format_arg(A);
format_args([A, B | T]) -> [format_arg(A), "," | format_args([B | T])].

format_arg(A) ->
    io_lib:fwrite("~p", [A]).

%% @spec applications() -> [atom()]
%% @doc Find all applications - either from the boot script or all loaded apps.
%% @end
%%
applications() ->
    {ok, [[Boot]]} = init:get_argument(boot),
    Script = Boot ++ ".script",
    Apps =
        case file:consult(Script) of
            {ok, [{script, _, Commands}]} ->
                [A || {apply, {application, load, [{application, A, _}]}}
                          <- Commands];
            Error ->
                error_logger:format("Unable to read boot script (~s): ~p~n",
                                    [Script, Error]),
                [A || {A, _, _} <- application:loaded_applications()]
        end,
    group_applications(Apps).

%% Sort apps in preorder traversal order.
%% That is, for each "top application", all included apps follow before the
%% next top application. Normally, there will be no included apps, in which
%% case the list will maintain its original order.
%%
group_applications([H | T]) ->
    case application:get_key(H, included_applications) of
        {ok, []} ->
            [H | group_applications(T)];
        {ok, Incls} ->
            AllIncls = all_included(Incls),
            [H | AllIncls] ++ group_applications(T -- AllIncls)
    end;
group_applications([]) ->
    [].

all_included([H | T]) ->
    case application:get_key(H, included_applications) of
        {ok, []} ->
            [H | all_included(T)];
        {ok, Incls} ->
            [H | all_included(Incls)] ++ all_included(T)
    end;
all_included([]) ->
    [].
