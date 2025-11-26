-module(setup_zomp).

-export([ is_zomp_context/0
        , update_env/0
        , setup_conf_path/0
        , default_dir/1 ]).

-include_lib("kernel/include/logger.hrl").

is_zomp_context() ->
    is_pid(whereis(zx_daemon)).

update_env() ->
    Args = init:get_plain_arguments(),
    ?LOG_INFO("Plain args: ~p", [Args]),
    look_for_setup_env(Args).

default_dir(home) -> ppath(etc);
default_dir(data) -> ppath(var);
default_dir(log) ->
    #{package_id := {Realm, App, _Vsn}} = zx_daemon:meta(),
    zx_lib:ppath(log, {Realm, App}).

ppath(Type) ->
    zx_lib:ppath(Type, package_id()).

package_id() ->
    #{package_id := PId} = zx_daemon:meta(),
    PId.

setup_conf_path() ->
    TopPPath = ppath(lib),
    [".", TopPPath].

look_for_setup_env(["-setup", K, V | Rest]) ->
    ?LOG_INFO("Processing env: ~p ~p", [K, V]),
    process_env(K, V),
    look_for_setup_env(Rest);
look_for_setup_env([_|T]) ->
    look_for_setup_env(T);
look_for_setup_env([]) ->
    ok.

process_env(K, V) ->
    case valid_env(K, V) of
        {true, {Key, Value}} ->
            ?LOG_INFO("Valid env: ~p: ~p", [Key, Value]),
            application:set_env(setup, Key, Value);
        false ->
            ?LOG_INFO("Invalid env", []),
            ignore
    end.

valid_env("log_dir" , D) -> {true, {log_dir , D}};
valid_env("data_dir", D) -> {true, {data_dir, D}};
valid_env("conf",     D) -> {true, {conf    , D}};
valid_env(_, _) -> 
    false.
