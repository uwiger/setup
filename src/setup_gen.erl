%% -*- erlang -*-
%%==============================================================================
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
%%==============================================================================
-module(setup_gen).
-export([main/1,  % escript-style
         run/1]). % when called from within erlang


main([Name, Config| InArgs]) ->
    put(is_escript, true),
    Opts = try options(InArgs)
           catch
               error:E ->
                   abort(E, [])
           end,
    run([{name, Name}, {con, Config} | Opts]).

%% @spec run(Options) -> ok
%% @doc Generates .rel file(s) and boot scripts for a given configuration.
%%
%% This function reads a configuration specification and generates the 
%% files needed to start a node from an OTP boot script. Optionally, it can
%% also generate a 'setup' script, which contains the same applications, but
%% only loaded (except the `setup' application, if present, which is started).
%% This way, a node can be started with all paths set, and all environment 
%% variables defined, such that a database can be created, and other setup tasks
%% be performed.
%%
%% Mandatory options:
%% * `{name, Name}'  - Name of the release (and of the .rel and .script files)
%% * `{outdir, Dir}' - Where to put the generated files. Dir is created if not
%%                     already present.
%%
%% Additional options:
%% @end
%%
run(Options) ->
    io:fwrite("Options = ~p~n", [Options]),
    [Name, RelDir] =
        [proplists:get_value(K, Options) || K <- [name, outdir]],
    Roots = roots(Options),
    Config = read_config(Options),
    check_config(Config),
    TargetDir = target_dir(RelDir, Config),
    Env = env_vars(Config, Options),
    InstEnv = install_env(Env, Config, Options),
    add_paths(Roots),
    RelVsn = rel_vsn(RelDir),
    Rel = {release, {Name, RelVsn}, {erts, erts_vsn()}, apps(Config, Options)},
    io:fwrite("Rel: ~p~n", [Rel]),
    in_dir(TargetDir,
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

options(["-outdir", D|T]) -> [{outdir, D}|options(T)];
options(["-root"  , D|T]) -> [{root, D}|options(T)];
options(["-V" ++ VarName, ExprStr | T]) ->
    Var = list_to_atom(VarName),
    Term = parse_term(ExprStr),
    [{var, Var, Term}|options(T)];
options([_Other|_] = L) ->
    erlang:error({unknown_option, L});
options([]) ->
    [].

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

target_dir(RelDir, Config) ->
    D = case proplists:get_value(target_subdir, Config) of
	    undefined ->
		RelDir;
	    Sub ->
		filename:join(RelDir, Sub)
	end,
    ensure_dir(D),
    D.

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
    F = option(conf, Opts),
    Dir = filename:dirname(F),
    Name = option(name, Opts),
    case file:script(F, [{'Name', Name}, {'CWD', Dir}, {'OPTIONS', Opts}]) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    abort("Error reading conf (~s): ~p~n", [F, Error])
    end.

roots(Opts) ->
    [R || {root, R} <- Opts].

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
	    io:fwrite("entering directory ~s~n", [D]),
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
                               (Apps0 -- [setup]) ++ [setup]
                       end, Apps0),
    AppVsns = lists:map(fun(App) ->
				A = if is_atom(App) -> App;
				       true -> element(1, App)
				    end,
				{A, app_vsn(A)}
			end, Apps1),
    setup_is_load_only(replace_versions(AppVsns, Apps1)).

setup_is_load_only(Apps) ->
    lists:map(fun({setup,V}) ->
		      {setup,V,load};
		 (A) ->
		      A
	      end, Apps).
    

add_paths(Roots) ->
    Paths = lists:concat([filelib:wildcard(filename:join(R,"lib/*/ebin"))
			  || R <- Roots]),
    io:fwrite("Paths = ~p~n", [Paths]),
    Res = code:add_paths(Paths -- code:get_path()),
    io:fwrite("add path Res = ~p~n", [Res]).
    

rel_vsn(RelDir) ->
    Dir =
	case RelDir of
	    "." ->
		{ok,Cwd} = file:get_cwd(),
		Cwd;
	    D ->
		D
	end,
    filename:basename(Dir).

erts_vsn() ->
    erlang:system_info(version).

app_vsn(A) ->
    D = code:lib_dir(A),
    AppFile = filename:join(D, "ebin/" ++ atom_to_list(A) ++ ".app"),
    case file:consult(AppFile) of
	{ok, [{application, _, Opts}]} ->
	    V = proplists:get_value(vsn, Opts),
	    io:fwrite("app_vsn(~p) -> ~p~n", [A,V]),
	    V;
	Other ->
	    abort("Oops reading .app file (~p): ~p~n", [AppFile, Other])
    end.
    
replace_versions([{A,V}|Apps], [H|T]) ->
    Res = 
	if is_atom(H) ->
		A = H,  % assertion
		{A, V};
	   true ->
		A = element(1, H), % assertion
		setelement(2, H, V)
	end,
    [Res | replace_versions(Apps, T)];
replace_versions([], []) ->
    [].

make_boot(Rel, Roots) ->
    Path = path(Roots),
    {Vars,_} = lists:mapfoldl(
		 fun(R, N) ->
			 V = var_name(N),
			 {{V, R}, N+1}
		 end, 1, Roots),
    io:fwrite("Path = ~p~n", [Path]),
    Res = systools:make_script(Rel, [no_module_tests, local,
                                     {variables, Vars},
                                     {path, path(Roots)}]),
    io:fwrite("make_script() -> ~p~n", [Res]).


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
    %% 		undefined ->
    %% 		    Apps1;
    %% 		V ->
    %% 		    Apps1 ++ [{setup, V}]
    %% 	    end,
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
            halt(1);
        _ ->
            erlang:error(lists:flatten(E))
    end.

