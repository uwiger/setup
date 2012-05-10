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
         find_env_vars/1,
         patch_app/1,
         find_app/1,
         reload_app/1, reload_app/2]).
-compile(export_all).

-export([run_setup/2]).

-include_lib("kernel/include/file.hrl").

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
%% @doc Returns the configured `home' directory, or a best guess (`$CWD')
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
%% @doc Returns the configured log dir, or a best guess (`home()/log.Node')
%% @end
%%
log_dir() ->
    setup_dir(log_dir, "log." ++ atom_to_list(node())).

%% @spec data_dir() -> Directory
%% @doc Returns the configured log dir, or a best guess (`home()/log.Node')
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
    _ = verify_dir(home()),
    _ = verify_dir(log_dir()),
    _ = verify_dir(data_dir()),
    ok.

%% @spec verify_dir(Dir) -> Dir
%% @doc Ensures that the directory Dir exists and is writable.
%% @end
%%
verify_dir(Directory) ->
    ok = filelib:ensure_dir(filename:join(Directory, "dummy")),
    Directory.

%% @spec find_env_vars(Env) -> [{AppName, Value}]
%% @doc Searches all loaded apps for instances of the `Env' environment variable.
%%
%% The environment variables may contain instances of
%% `$APP', `$PRIV_DIR', `$LIB_DIR', `$DATA_DIR', `$LOG_DIR', `$HOME',
%% inside strings or binaries, and these will be replaced with actual values
%% for the current system (`$APP' simply expands to the name of the current
%% application).
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

%% @spec patch_app(AppName::atom()) -> true | {error, Reason}
%%
%% @doc Adds an application's "development" path to a target system
%%
%% This function locates the given application (`AppName') along the `$ERL_LIBS'
%% path, and prepends it to the code path of the existing system. This is useful
%% not least when one wants to add e.g. a debugging or trace application to a
%% target system.
%%
%% The function will not add the same path again, if the new path is already
%% the 'first' path entry for the application `A'.
%% @end
patch_app(A) when is_atom(A) ->
    patch_app(A, latest).

patch_app(A, Vsn) ->
    case find_app(A) of
        [_|_] = Found ->
            {_ActualVsn, Dir} = pick_vsn(A, Found, Vsn),
            io:fwrite("[~p vsn ~p] code:add_patha(~s)~n", [A, _ActualVsn, Dir]),
            code:add_patha(Dir);
        [] ->
            error(no_matching_vsn)
    end.

%% @spec pick_vsn(App::atom(), Dirs::[{Vsn::string(),Dir::string()}], Which) ->
%%          {Vsn, Dir}
%%  where
%%     Which = 'latest' | 'next' | Regexp
%%
%% @doc Picks the specified version out of a list returned by {@link find_app/1}
%%
%% * If `Which' is a string, it will be used as a `re' regexp pattern, and the
%%   first matching version will be returned.
%%
%% * If `Which = latest', the last entry in the list will be returned (assumes
%%   that the list is sorted in ascending version order).
%%
%% * If `Which = next', the next version following the current version of the
%%   application `A' is returned, assuming `A' is loaded; if `A' is not loaded,
%%   the first entry in the list is returned.
%%
%% If no matching version is found, the function raises an exception.
%% @end
pick_vsn(_, Dirs, latest) ->
    lists:last(Dirs);
pick_vsn(A, Dirs, next) ->
    case application:get_key(A, vsn) of
        {ok, Cur} ->
            case lists:dropwhile(fun({V, _}) -> V =/= Cur end, Dirs) of
                [_, {_, _} = Next |_] -> Next;
                _ -> error(no_matching_vsn)
            end;
        _ ->
            hd(Dirs)
    end;
pick_vsn(_, Dirs, Vsn) ->
    case [X || {V, _} = X <- Dirs,
               re:run(V, Vsn) =/= nomatch] of
        [Found|_] ->
            Found;
        [] ->
            error(no_matching_vsn)
    end.


%% @spec find_app(A::atom()) -> [{Vsn, Dir}]
%%
%% @doc Locates application `A' along $ERL_LIBS or under the OTP root
%% @end
find_app(A) ->
    CurDir = case code:lib_dir(A) of
                 {error,_} -> [];
                 D ->
                     [filename:join(D, "ebin")]
             end,
    LibDirs = get_user_lib_dirs(),
    CurRoots = current_roots(),
    RevPfx = lists:reverse(filename:join(atom_to_list(A), "ebin")),
    InLib = [P || P <- LibDirs,
                  lists:prefix(RevPfx, lists:reverse(P))],
    InRoots = lists:concat([in_root(A, R) || R <- CurRoots]),
    sort_vsns(lists:usort(CurDir ++ InRoots ++ InLib), atom_to_list(A)).

current_roots() ->
    CurPath = code:get_path(),
    All = lists:foldr(
            fun(D, Acc) ->
                    case lists:reverse(filename:split(D)) of
                        ["ebin",_|T] ->
                            [filename:join(lists:reverse(T)) | Acc];
                        _ ->
                            Acc
                    end
            end, [], CurPath),
    lists:usort(All).


sort_vsns(Dirs, AppStr) ->
    AppF = AppStr ++ ".app",
    lists:sort(fun({Va,_}, {Vb,_}) ->
                       compare_vsns(Va, Vb)
               end,
               lists:foldr(
                 fun(D, Acc) ->
                         case file:consult(
                                filename:join(D, AppF)) of
                             {ok, [{_, _, Attrs}]} ->
                                 {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
                                 [{Vsn, D} | Acc];
                             _ ->
                                 Acc
                         end
                 end, [], Dirs)).

compare_vsns(V1, V2) ->
    ToS = fun(V) ->
                  [pad_x(X) || X <- string:tokens(V, ".")]
          end,
    ToS(V1) < ToS(V2).

pad_x(X) ->
    S = if is_integer(X) -> integer_to_list(X);
           true -> X
        end,
    lists:duplicate(30 - length(S), $0) ++ [flip(C) || C <- S].

flip(C) when $a =< C, C =< $z -> $A + (C - $a);
flip(C) when $A =< C, C =< $Z -> $a + (C - $A);
flip(C) -> C.


in_root(A, R) ->
    Paths = filelib:wildcard(filename:join([R, "*", "ebin"])),
    Pat = atom_to_list(A) ++ "-[\\.0-9]+/ebin\$",
    [P || P <- Paths,
          re:run(P, Pat) =/= nomatch].

%% @spec reload_app(AppName::atom()) -> {ok, NotPurged} | {error, Reason}
%%
%% @equiv reload_app(AppName, latest)
reload_app(A) ->
    reload_app(A, latest).

%% @spec reload_app(AppName::atom(), ToVsn) -> {ok, Unpurged} | {error, Reason}
%%  where
%%    ToVsn = 'latest' | 'next' | Vsn,
%%    Vsn   = string()
%%
%% @doc Loads or upgrades an application to the specified version
%%
%% This function is a convenient function for 'upgrading' an application.
%% It locates the given version (using {@link find_app/1} and {@link pick_vsn/3})
%% and loads it in the most appropriate way:
%%
%% * If the application isn't already loaded, it loads the application and
%%   all its modules.
%%
%% * If the application is loaded, it generates an appup script and performs
%%   a soft upgrade. If the new version of the application has an `.appup' script
%%   on-disk, that script is used instead.
%%
%% The generated appup script is of the form:
%%
%% * add modules not present in the previous version of the application
%%
%% * do a soft upgrade on pre-existing modules, using suspend-code_change-resume
%%
%% * delete modules that existed in the old version, but not in the new.
%%
%% The purge method used is `brutal_purge' - see {@link //sasl/appup}.
%%
%% For details on how the new version is chosen, see {@link find_app/1} and
%% {@link pick_vsn/3}.
%% @end
reload_app(A, ToVsn0) ->
    case application:get_key(A, vsn) of
        undefined ->
            ok = application:load(A),
            {ok, Modules} = application:get_key(A, modules),
            _ = [c:l(M) || M <- Modules],
            {ok, []};
        {ok, FromVsn} ->
            {ToVsn, NewPath} = pick_vsn(A, find_app(A), ToVsn0),
            io:fwrite("[~p vsn ~p] soft upgrade from ~p~n", [A, ToVsn, FromVsn]),
            reload_app(
              A, FromVsn, filename:join(code:lib_dir(A), "ebin"),
              NewPath, ToVsn)
    end.

reload_app(A, OldVsn, OldPath, NewPath, NewVsn) ->
    {_NewVsn, Script, NewApp} = make_appup_script(A, OldVsn, NewPath),
    reload_app(A, OldVsn, OldPath, NewPath, NewVsn, Script, NewApp).

reload_app(A, _OldVsn, _OldPath, NewPath, NewVsn, Script, _NewApp) ->
    LibDir = filename:dirname(NewPath),
    _ = remove_path(NewPath, A),
    case release_handler:eval_appup_script(A, NewVsn, LibDir, Script) of
        {ok, Unpurged} ->
            _ = [code:purge(M) || {M, brutal_purge} <- Unpurged],
            {ok, [U || {_, Mode} = U <- Unpurged, Mode =/= brutal_purge]};
        Other ->
            Other
    end.

remove_path(P, A) ->
    CurPath = code:get_path(),
    case lists:member(P, CurPath) of
        true ->
            %% don't remove if it's the only path
            case [Px || Px <- path_entries(A, CurPath),
                        Px =/= P] of
                [] ->
                    true;
                [_|_] ->
                    code:set_path([Px || Px <- CurPath,
                                         Px =/= P])
            end;
        false ->
            true
    end.

path_entries(A) ->
    path_entries(A, code:get_path()).

path_entries(A, Path) ->
    Pat = atom_to_list(A) ++ "[^/]*/ebin\$",
    [P || P <- Path,
          re:run(P, Pat) =/= nomatch].

make_appup_script(A, OldVsn, NewPath) ->
    {application, _, NewAppTerms} = NewApp =
        read_app(filename:join(NewPath, atom_to_list(A) ++ ".app")),
    OldAppTerms = application:get_all_key(A),
    _OldApp = {application, A, OldAppTerms},
    case find_script(A, NewPath, OldVsn, up) of
        {NewVsn, Script} ->
            {NewVsn, Script, NewApp};
        false ->
            {ok, OldMods} = application:get_key(A, modules),
            {modules, NewMods} = lists:keyfind(modules, 1, NewAppTerms),
            {vsn, NewVsn} = lists:keyfind(vsn, 1, NewAppTerms),
            {DelMods,AddMods,ChgMods} = {OldMods -- NewMods,
                                         NewMods -- OldMods,
                                         intersection(NewMods, OldMods)},
            {NewVsn,
             [{load_object_code,{A, NewVsn, NewMods}}]
             ++ [point_of_no_return]
             ++ [{load, {M, brutal_purge, brutal_purge}} || M <- AddMods]
             ++ [{suspend, ChgMods} || ChgMods =/= []]
             ++ [{load, {M, brutal_purge,brutal_purge}} || M <- ChgMods]
             ++ [{code_change, up, [{M, setup} || M <- ChgMods]} ||
                    ChgMods =/= []]
             ++ [{resume, ChgMods} || ChgMods =/= []]
             ++ [{remove, {M, brutal_purge,brutal_purge}} || M <- DelMods]
             ++ [{purge, DelMods} || DelMods =/= []],
             NewApp}
    end.

read_app(F) ->
    case file:consult(F) of
        {ok, [App]} ->
            App;
        {error,_} = Error ->
            error(Error, [F])
    end.

%% slightly modified (and corrected!) version of release_handler:find_script/4.
find_script(App, Dir, OldVsn, UpOrDown) ->
    Appup = filename:join([Dir, "ebin", atom_to_list(App)++".appup"]),
    case file:consult(Appup) of
	{ok, [{NewVsn, UpFromScripts, DownToScripts}]} ->
	    Scripts = case UpOrDown of
			  up -> UpFromScripts;
			  down -> DownToScripts
		      end,
	    case lists:dropwhile(fun({Re,_}) ->
					 re:run(OldVsn, Re) == nomatch
				 end, Scripts) of
		[{_OldVsn, Script}|_] ->
		    {NewVsn, Script};
		[] ->
		    false
	    end;
	{error, enoent} ->
            false;
	{error, _} ->
            false
    end.


%% find_procs(Mods) ->
%%     Ps = release_handler_1:get_supervised_procs(),
%%     lists:flatmap(
%%       fun({P,_,_,Ms}) ->
%%               case intersection(Ms, Mods) of
%%                   [] -> [];
%%                   I  -> [{P, I}]
%%               end
%%       end, Ps).

intersection(A, B) ->
    A -- (A -- B).



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
%% @doc Execute all setup hooks in order
%%
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




%% stolen from code_server.erl:
get_user_lib_dirs() ->
    case os:getenv("ERL_LIBS") of
        LibDirs0 when is_list(LibDirs0) ->
            Sep =
                case os:type() of
                    {win32, _} -> $;;
                    _          -> $:
                end,
            LibDirs = split_paths(LibDirs0, Sep, [], []),
            get_user_lib_dirs_1(LibDirs);
        false ->
            []
    end.

get_user_lib_dirs_1([Dir|DirList]) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Dirs} ->
            {Paths,_Libs} = make_path(Dir, Dirs),
            %% Only add paths trailing with ./ebin.
            [P || P <- Paths, filename:basename(P) =:= "ebin"] ++
                get_user_lib_dirs_1(DirList);
        error ->
            get_user_lib_dirs_1(DirList)
    end;
get_user_lib_dirs_1([]) -> [].

split_paths([S|T], S, Path, Paths) ->
    split_paths(T, S, [], [lists:reverse(Path) | Paths]);
split_paths([C|T], S, Path, Paths) ->
    split_paths(T, S, [C|Path], Paths);
split_paths([], _S, Path, Paths) ->
    lists:reverse(Paths, [lists:reverse(Path)]).


make_path(BundleDir, Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir, Bundles, [], []).

choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B, ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
                     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName, ArchiveExt) ->
    BaseName = filename:basename(FullName, ArchiveExt),
    case split(BaseName, "-") of
        [_, _|_] = Toks ->
            VsnStr = lists:last(Toks),
            case vsn_to_num(VsnStr) of
                {ok, VsnNum} ->
                    Name = join(lists:sublist(Toks, length(Toks)-1),"-"),
                    {Name,VsnNum,FullName};
                false ->
                    {FullName,[0],FullName}
            end;
        _ ->
            {FullName,[0],FullName}
    end.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
        true ->
            {ok, [list_to_integer(S) || S <- split(Vsn, ".")]};
        _  ->
            false
    end.

is_vsn(Str) when is_list(Str) ->
    Vsns = split(Str, "."),
    lists:all(fun is_numstr/1, Vsns).

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true; 
                  (_)                       -> false
              end, Cs).

split(Cs, S) ->
    split1(Cs, S, []).

split1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
        true -> split1(S, Seps, Toks);
        false -> split2(S, Seps, Toks, [C])
    end;
split1([], _Seps, Toks) ->
    lists:reverse(Toks).

split2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> split1(S, Seps, [lists:reverse(Cs)|Toks]);
        false -> split2(S, Seps, Toks, [C|Cs])
    end;
split2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

join([H1, H2| T], S) ->
    H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].

choose([{Name,NumVsn,NewFullName}=New|Bs], Acc, ArchiveExt) ->
    case lists:keyfind(Name, 1, Acc) of
        {_, NV, OldFullName} when NV =:= NumVsn ->
            case filename:extension(OldFullName) =:= ArchiveExt of
                false ->
                    choose(Bs,Acc, ArchiveExt);
                true ->
                    Acc2 = lists:keystore(Name, 1, Acc, New),
                    choose(Bs,Acc2, ArchiveExt)
            end;
        {_, _, _} ->
            choose(Bs,Acc, ArchiveExt);
        false ->
            choose(Bs,[{Name,NumVsn,NewFullName}|Acc], ArchiveExt)
    end;
choose([],Acc, _ArchiveExt) ->
    Acc.

make_path(_,[],Res,Bs) ->
    {Res,Bs};
make_path(BundleDir,[Bundle|Tail],Res,Bs) ->
    Dir = filename:append(BundleDir,Bundle),
    Ebin = filename:append(Dir,"ebin"),
    %% First try with /ebin
    case erl_prim_loader:read_file_info(Ebin) of
        {ok,#file_info{type=directory}} ->
            make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
        _ ->
            %% Second try with archive
            Ext = archive_extension(),
            Base = filename:basename(Dir, Ext),
            Ebin2 = filename:join([filename:dirname(Dir), Base ++ Ext,
                                   Base, "ebin"]),
            Ebins =
                case split(Base, "-") of
                    [_, _|_] = Toks ->
                        AppName = join(lists:sublist(Toks, length(Toks)-1),"-"),
                        Ebin3 = filename:join([filename:dirname(Dir), Base ++ Ext, AppName, "ebin"]),
                        [Ebin3, Ebin2, Dir];
                    _ ->
                        [Ebin2, Dir]
                end,
            try_ebin_dirs(Ebins,BundleDir,Tail,Res,Bundle, Bs)
    end.

try_ebin_dirs([Ebin | Ebins],BundleDir,Tail,Res,Bundle,Bs) ->
    case erl_prim_loader:read_file_info(Ebin) of
        {ok,#file_info{type=directory}} -> 
            make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
        _ ->
            try_ebin_dirs(Ebins,BundleDir,Tail,Res,Bundle,Bs)
    end;
try_ebin_dirs([],BundleDir,Tail,Res,_Bundle,Bs) ->
    make_path(BundleDir,Tail,Res,Bs).

archive_extension() ->
    init:archive_extension().
