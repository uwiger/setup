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

default_dir(data) ->
    #{package_id := PId} = zx_daemon:meta(),
    Dir = zx_lib:ppath(var, PId),
    filename:join(Dir, "setup.data");
default_dir(log) ->
    try zomp_default_log_dir()
    catch
        error:_ ->
            undefined
    end.

setup_conf_path() ->
    #{package_id := TopPId} = zx_daemon:meta(),
    TopPPath = zx_lib:ppath(lib, TopPId),
    [".", TopPPath].

zomp_default_log_dir() ->
    {ok, H} = logger:get_handler_config(default),
    #{config := #{file := F}} = H,
    [Base,_] = re:split(F,"\\.log$",[{return,list}]),
    Base.

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
