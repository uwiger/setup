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
-module(setup_gen).
-export([main/1,   % escript-style
         run/1,    % when called from within erlang
         help/0]). % prints help text.

-define(if_verbose(Expr),
        case get(verbose) of
            true -> Expr;
            _    -> ok
        end).

main([]) ->
    help(),
    halt(1);
main([H]) when H=="-h"; H=="-help" ->
    help(),
    halt(0);
main(["-" ++ _|_] = Args) ->
    put(is_escript, true),
    Opts = try options(Args)
           catch
               error:E ->
                   abort(E, [])
           end,
    run(Opts);
main([Name, Config, Out| InArgs]) ->
    put(is_escript, true),
    Opts = try options(InArgs)
           catch
               error:E ->
                   abort(E, [])
           end,
    run([{name, Name}, {conf, Config}, {outdir, Out} | Opts]).

%% @spec run(Options) -> ok
%% @doc Generates .rel file(s) and boot scripts for a given configuration.
%%
%% This function reads a configuration specification and generates the
%% files needed to start a node from an OTP boot script. Optionally, it can
%% also generate a 'setup' script, which contains the same applications, but
%% only loaded (except the `setup' application, if present, which is started).
%% This way, a node can be started with all paths set, and all environment
%% variables defined, such that a database can be created, and other setup
%% tasks be performed.
%%
%% Mandatory options:
%% * `{name, Name}'  - Name of the release (and of the .rel and .script files)
%% * `{outdir, Dir}' - Where to put the generated files. Dir is created if not
%%                     already present.
%% * `{conf, Conf}'  - Config file listing applications & perhaps other options
%%
%% Additional options:
%% * ...
%% @end
%%
run(Options) ->
    %% dbg:tracer(),
    %% dbg:tpl(?MODULE,x),
    %% dbg:p(all,[c]),
    case lists:keyfind(verbose, 1, Options) of
        {_, true} -> put(verbose, true);
        _ -> ignore
    end,
    ?if_verbose(io:fwrite("Options = ~p~n", [Options])),
    [Name, RelDir] =
        [option(K, Options) || K <- [name, outdir]],
    ensure_dir(RelDir),
    Config = read_config(Options),
    Roots = roots(Options, Config),
    check_config(Config),
    Env = env_vars(Config, Options),
    InstEnv = install_env(Env, Config, Options),
    add_paths(Roots),
    RelVsn = rel_vsn(RelDir, Options),
    Rel = {release, {Name, RelVsn}, {erts, erts_vsn()}, apps(Config, Options)},
    ?if_verbose(io:fwrite("Rel: ~p~n", [Rel])),
    in_dir(RelDir,
           fun() ->
                   write_eterm("start.rel", Rel),
                   make_boot("start", Roots),
                   write_eterm("sys.config", Env),
                   if_install(Options,
                              fun() ->
                                      InstRel = make_install_rel(Rel),
                                      write_eterm("install.rel", InstRel),
                                      write_eterm("install.config", InstEnv),
                                      make_boot("install", Roots)
                              end, ok),
                   write_eterm("setup_gen.eterm", Config)
           end).


if_install(Options, F, Else) ->
    case proplists:get_value(install,Options,false) of
        true ->
            F();
        _ ->
            Else
    end.

help() ->
    io:fwrite(
      "Usage: escript setup_gen.beam Name Conf Outdir [Options]~n"
      "   or:~n"
      "       escript setup_gen.beam Options~n~n"
      "Name  : Name of release (for .rel file)~n"
      "Conf  : Name of .conf file (file:script/2 format)~n"
      "Outdir: Where to write generated files~n~n"
      "-name Name : Name of release (for .rel file)~n"
      "-root Dir  : Installation root directories"
      " (multiple -root options allowed)~n"
      "-conf F    : setup-style Conf file~n"
      "-relconf F : RelTool-style config file~n"
      "-out OutDir: Where to write generated files~n"
      "-sys F     : Name of pre-existing sys.config file~n"
      "-vsn V     : System version (otherwise derived from outdir)~n"
      "-install B : B:: true|false - whether to create install boot script~n"
      "-v         : Verbose - generate lots of output~n"
      , []).

options(["-name"         , N|T]) -> [{name, N}|options(T)];
options(["-root"         , D|T]) -> [{root, D}|options(T)];
options(["-out"          , D|T]) -> [{outdir, D}|options(T)];
options(["-relconf"      , F|T]) -> [{relconf, F}|options(T)];
options(["-target_subdir", D|T]) -> [{target_subdir, D}|options(T)];
options(["-install"])            -> [{install, true}];
options(["-install" | ["-" ++ _|_] = T]) -> [{install, true}|options(T)];
options(["-install"      , D|T]) -> [{install, mk_bool(D)}|options(T)];
options(["-sys"          , D|T]) -> [{sys, D}|options(T)];
options(["-vsn"          , D|T]) -> [{vsn, D}|options(T)];
options(["-v"               |T]) -> [{verbose, true}|options(T)];
options(["-V" ++ VarName, ExprStr | T]) ->
    Var = list_to_atom(VarName),
    Term = parse_term(ExprStr),
    [{var, Var, Term}|options(T)];
options([_Other|_] = L) ->
    abort("Unknown_option: ~p~n", [L]);
options([]) ->
    [].

mk_bool(T) when T=="true" ; T=="1" -> true;
mk_bool(F) when F=="false"; F=="0" -> false;
mk_bool(Other) ->
    abort("Expected truth value (~p)~n", [Other]).

parse_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Ts, _} ->
            case erl_parse:parse_term(ensure_dot(Ts)) of
                {ok, T} ->
                    T;
                {error,_} = EParse ->
                    abort(EParse, [])
            end;
        {error,_} = EScan ->
            abort(EScan, [])
    end.

ensure_dot(Ts) ->
    case lists:reverse(Ts) of
        [{dot,_}|_] ->
            Ts;
        Rev ->
            lists:reverse([{dot,1}|Rev])
    end.

%% target_dir(RelDir, Config) ->
%%     D = case proplists:get_value(target_subdir, Config) of
%%          undefined ->
%%              RelDir;
%%          Sub ->
%%              filename:join(RelDir, Sub)
%%      end,
%%     ensure_dir(D),
%%     D.

ensure_dir(D) ->
    case filelib:is_dir(D) of
        true ->
            ok;
        false ->
            case filelib:ensure_dir(D) of
                ok ->
                    case file:make_dir(D) of
                        ok ->
                            ok;
                        MakeErr ->
                            abort("Could not create ~s (~p)~n", [D, MakeErr])
                    end;
                EnsureErr ->
                    abort("Parent of ~s could not be created or is not "
                          "writeable (~p)~n", [D, EnsureErr])
            end
    end.

read_config(Opts) ->
    case lists:keyfind(conf, 1, Opts) of
        false ->
            read_rel_config(Opts);
        {_, F} ->
            Dir = filename:dirname(F),
            Name = option(name, Opts),
            case file:script(F, [{'Name', Name}, {'CWD', Dir}, {'OPTIONS', Opts}]) of
                {ok, Conf} ->
                    Conf;
                Error ->
                    abort("Error reading conf (~s): ~p~n", [F, Error])
            end
    end.

read_rel_config(Opts) ->
    case lists:keyfind(relconf, 1, Opts) of
        {relconf, F} ->
            Name = option(name, Opts),
            case file:consult(F) of
                {ok, Conf} ->
                    SysConf = option(sys, Conf),
                    LibDirs = option(lib_dirs, SysConf),
                    case [As || {rel,N,_,As} <- SysConf,
                                N == Name] of
                        [] ->
                            abort("No matching 'rel' (~w) in ~s~n", [Name, F]);
                        [Apps] ->
                            [{apps, Apps} | [{root, D} || D <- LibDirs]]
                    end;
                Error ->
                    abort("Error reading relconf ~s:~n"
                          "~p~n", [F, Error])
            end;
        false ->
            abort("No usable config file~n", [])
    end.

roots(Opts, Conf) ->
    [R || {root, R} <- Opts] ++ [R || {root, R} <- Conf].

check_config(Conf) ->
    [mandatory(K, Conf) || K <- [apps]],
    ok.

option(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} ->
            V;
        false ->
            abort({mandatory, K}, [])
    end.

env_vars(Config, Options) ->
    Env0 = case proplists:get_value(sys, Options) of
               undefined ->
                   [];
               Sys ->
                   {ok, [E]} = file:consult(Sys),
                   E
           end,
    SetupEnv = if_install(Options, fun() -> [{setup,
                                              [{conf,Config}]}]
                                   end, []),
    lists:foldl(
      fun(E, Acc) ->
              merge_env(E, Acc)
      end, Env0, [E || {env, E} <- Config] ++ [SetupEnv]).

install_env(Env, Config, Options) ->
    Dist =
        case proplists:get_value(nodes, Options) of
            undefined ->
                case proplists:get_value(nodes, Config, []) of
                    [_] -> [];
                    Ns  -> Ns
                end;
            [_] ->
                [];
            [_,_|_] = Nodes ->
                [{sync_nodes_mandatory, Nodes},
                 {sync_nodes_timeout, infinity},
                 {distributed, [{setup, [hd(Nodes)]}]}]
        end,
    case lists:keyfind(kernel, 1, Env) of
        false ->
            [{kernel, Dist} | Env];
        {_, KEnv} ->
            Env1 = Dist ++
                [E || {K,_} = E <- KEnv,
                      not lists:member(K, [sync_nodes_optional,
                                           sync_nodes_mandatory,
                                           sync_nodes_timeout,
                                           distributed])],
            lists:keyreplace(kernel, 1, Env, {kernel, Env1})
    end.

merge_env(E, Env) ->
    lists:foldl(
      fun({App, AEnv}, Acc) ->
              case lists:keyfind(App, 1, Env) of
                  false ->
                      Acc ++ [{App, AEnv}];
                  {_, AEnv1} ->
                      New = {App, lists:foldl(
                                    fun({K,V}, Acc1) ->
                                            lists:keystore(K,1,Acc1,{K,V})
                                    end, AEnv1, AEnv)},
                      lists:keyreplace(App, 1, Acc, New)
              end
      end, Env, E).

mandatory(K, Conf) ->
    case lists:keymember(K, 1, Conf) of
        false ->
            abort("missing mandatory config item: ~p~n", [K]);
        true ->
            ok
    end.

in_dir(D, F) ->
    {ok, Old} = file:get_cwd(),
    try file:set_cwd(D) of
        ok ->
            ?if_verbose(io:fwrite("entering directory ~s~n", [D])),
            F();
        Error ->
            abort("Error entering rel dir (~p): ~p~n", [D,Error])
    after
        file:set_cwd(Old)
    end.

apps(Config, Options) ->
    Apps0 = proplists:get_value(apps, Config),
    Apps1 = if_install(Options,
                       fun() ->
                               ensure_setup(Apps0)
                       end, Apps0),
    ?if_verbose(io:fwrite("Apps1 = ~p~n", [Apps1])),
    AppVsns = lists:map(fun({App,load}) ->
                                {App, app_vsn(App), load};
                           ({_,_,load} = A) ->
                                A;
                            (App) ->
                                A = if is_atom(App) -> App;
                                       true -> element(1, App)
                                    end,
                                {A, app_vsn(A)}
                        end, Apps1),
    ?if_verbose(io:fwrite("AppVsns = ~p~n", [AppVsns])),
    %% setup_is_load_only(replace_versions(AppVsns, Apps1)).
    setup_is_load_only(AppVsns).

ensure_setup([setup|_] = As) -> As;
ensure_setup([A|_] = As) when element(1,A) == setup -> As;
ensure_setup([H|T]) -> [H|ensure_setup(T)];
ensure_setup([]) ->
    [setup].

setup_is_load_only(Apps) ->
    lists:map(fun({setup,V}) ->
                      {setup,V,load};
                 (A) ->
                      A
              end, Apps).

add_paths(Roots) ->
    %% Paths = lists:concat([filelib:wildcard(filename:join(R,"lib/*/ebin"))
    %%                       || R <- Roots]),
    Paths = lists:foldl(fun(R, Acc) ->
                                expand_root(R, Acc)
                        end, [], Roots),
    ?if_verbose(io:fwrite("Paths = ~p~n", [Paths])),
    Res = code:add_paths(Paths -- code:get_path()),
    ?if_verbose(io:fwrite("add path Res = ~p~n", [Res])).

expand_root(R, Acc) ->
    case filename:basename(R) of
        "ebin" ->
            [R|Acc];
        _ ->
            case file:list_dir(R) of
                {ok, Fs} ->
                    lists:foldl(fun(F, Acc1) ->
                                        expand_root(filename:join(R, F), Acc1)
                                end, Acc, Fs);
                {error,enotdir} ->
                    Acc;
                {error,_} = E ->
                    ?if_verbose(io:fwrite("warning: ~p (~s)~n", [E, R])),
                    Acc
            end
    end.

rel_vsn(RelDir, Options) ->
    case proplists:get_value(vsn, Options) of
        undefined ->
            Dir =
                case RelDir of
                    "." ->
                        {ok,Cwd} = file:get_cwd(),
                        Cwd;
                    D ->
                        D
                end,
            filename:basename(Dir);
        V when is_list(V) ->
            V;
        Other ->
            abort("Invalid release version ~p~n", [Other])
    end.

erts_vsn() ->
    erlang:system_info(version).

app_vsn(A) ->
    D = code:lib_dir(A),
    AppFile = filename:join(D, "ebin/" ++ atom_to_list(A) ++ ".app"),
    case file:consult(AppFile) of
        {ok, [{application, _, Opts}]} ->
            V = proplists:get_value(vsn, Opts),
            ?if_verbose(io:fwrite("app_vsn(~p) -> ~p~n", [A,V])),
            V;
        Other ->
            abort("Oops reading .app file (~p): ~p~n", [AppFile, Other])
    end.

%% replace_versions([App|Apps], [H|T]) ->
%%     A = element(1, App),
%%     V = element(2, App),
%%     Res =
%%         if is_atom(H) ->
%%                 A = H,  % assertion
%%                 {A, V};
%%            true ->
%%                 A = element(1, H), % assertion
%%                 setelement(2, H, V)
%%         end,
%%     [Res | replace_versions(Apps, T)];
%% replace_versions([], []) ->
%%     [].

make_boot(Rel, Roots) ->
    Path = path(Roots),
    {Vars,_} = lists:mapfoldl(
                 fun(R, N) ->
                         V = var_name(N),
                         {{V, R}, N+1}
                 end, 1, Roots),
    ?if_verbose(io:fwrite("Path = ~p~n", [Path])),
    Res = systools:make_script(Rel, [no_module_tests, local,
                                     {variables, Vars},
                                     {path, path(Roots)}]),
    ?if_verbose(io:fwrite("make_script() -> ~p~n", [Res])).


make_install_rel({release, R, Erts, Apps}) ->
    Apps1 =
        lists:map(
          fun({setup,V,load}) ->
                  {setup, V};
              (A) ->
                  case lists:member(element(1,A), [stdlib,kernel,sasl]) of
                      true ->
                          A;
                      false ->
                          case A of
                              {Nm,Vsn} ->
                                  {Nm,Vsn,load};
                              {Nm,Vsn,Inc} when is_list(Inc) ->
                                  {Nm,Vsn,load,Inc};
                              _ ->
                                  A
                          end
                  end
          end, Apps),
    %% Apps2 = case app_vsn(setup) of
    %%          undefined ->
    %%              Apps1;
    %%          V ->
    %%              Apps1 ++ [{setup, V}]
    %%      end,
    {release, R, Erts, Apps1}.


path(Roots) ->
    [filename:join(R, "lib/*/ebin") || R <- Roots].


var_name(N) ->
    "V" ++ integer_to_list(N).

write_eterm(F, Rel) ->
    case file:open(F, [write]) of
        {ok, Fd} ->
            try
                io:fwrite(Fd, "~p.~n", [Rel])
            after
                file:close(Fd)
            end;
        Error ->
            abort("Error writing .rel file (~p): ~p~n", [F, Error])
    end.


abort(Fmt, Args) ->
    E = io_lib:fwrite(Fmt, Args),
    case get(is_escript) of
        true ->
            io:fwrite(E),
            help(),
            halt(1);
        _ ->
            erlang:error(lists:flatten(E))
    end.
