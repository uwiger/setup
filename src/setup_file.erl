-module(setup_file).

-export([list_dir/1,
         open/2,
         close/1,
         read_file/1,
         consult/1,
         script/1,
         script/2]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

list_dir(Dir) ->
    case file:list_dir(Dir) of
        {error, enotdir} = Err ->
            case contains_zip_file(Dir) of
                {true, D} ->
                    prim_loader_list_dir(D);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

read_file(File) ->
    case file:read_file(File) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    prim_loader_read_file(F);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

consult(File) ->
    case file:consult(File) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    consult_zip(F);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

script(File) ->
    script(File, []).

script(File, Bindings) ->
    case file:script(File, Bindings) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    eval_zip(F, Bindings);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

contains_zip_file(File) when is_atom(File) ->
    contains_zip_file(atom_to_binary(File, utf8));
contains_zip_file(File) ->
    case re:run(File, "\\.ez", []) of
        {match, _} -> {true, File};
        nomatch    -> false
    end.

consult_zip(File) ->
    case prim_loader_read_file(File) of
        {ok, Bin} ->
            {ok, Fd} = open_bin(Bin, [read]),
            R = consult_stream(Fd),
            _ = close(Fd),
            R;
        {error, _} = ReadError ->
            ReadError
    end.

open(File, Opts) ->
    case file:open(File, Opts) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    open_zip(F, Opts);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

open_zip(File, Opts) ->
    case check_opts(Opts) of
        ok ->
            case prim_loader_read_file(File) of
                {ok, Bin} ->
                    open_bin(Bin, Opts);
                Other ->
                    Other
            end;
        Error ->
            Error
    end.

open_bin(Bin, Opts) ->
    Opts1 = [O || O <- Opts,
                  O =/= raw], % will this work anyway?
    setup_file_io_server:start_link(self(), Bin, Opts1).

check_opts([O | _])
  when O == write;
       O == append;
       O == exclusive;
       O == delayed_write;
       O == directory ->
    {error, eacces};
check_opts([{delayed_write,_}|_]) ->
    {error, eacces};
check_opts([_|Opts]) ->
    check_opts(Opts);
check_opts([]) ->
    ok.



close(Fd) ->
    file:close(Fd).

consult_stream(Fd) ->
    _ = epp:set_encoding(Fd),
    consult_stream(Fd, 1, []).

consult_stream(Fd, Line, Acc) ->
    case io:read(Fd, '', Line) of
        {ok,Term,EndLine} ->
            consult_stream(Fd, EndLine, [Term|Acc]);
        {error,Error,_Line} ->
            {error,Error};
        {eof,_Line} ->
            {ok,lists:reverse(Acc)}
    end.

eval_zip(File, Bs) ->
    case prim_loader_read_file(File) of
        {ok, Bin} ->
            {ok, Fd} = open_bin(Bin, [read]),
            R = eval_stream(Fd, return, Bs),
            _ = file:close(Fd),
            R;
        Error ->
            Error
    end.

%% Copied from file.erl (OTP 24.3.4.2)
eval_stream(Fd, Handling, Bs) ->
    _ = epp:set_encoding(Fd),
    eval_stream(Fd, Handling, 1, undefined, [], Bs).

eval_stream(Fd, H, Line, Last, E, Bs) ->
    eval_stream2(io:parse_erl_exprs(Fd, '', Line), Fd, H, Last, E, Bs).

eval_stream2({ok,Form,EndLine}, Fd, H, Last, E, Bs0) ->
    try erl_eval:exprs(Form, Bs0) of
        {value,V,Bs} ->
            eval_stream(Fd, H, EndLine, {V}, E, Bs)
    catch Class:Reason:StackTrace ->
            Error = {EndLine,?MODULE,{Class,Reason,StackTrace}},
            eval_stream(Fd, H, EndLine, Last, [Error|E], Bs0)
    end;
eval_stream2({error,What,EndLine}, Fd, H, Last, E, Bs) ->
    eval_stream(Fd, H, EndLine, Last, [What | E], Bs);
eval_stream2({eof,EndLine}, _Fd, H, Last, E, _Bs) ->
    case {H, Last, E} of
        {return, {Val}, []} ->
            {ok, Val};
        {return, undefined, E} ->
            {error, hd(lists:reverse(E, [{EndLine,?MODULE,undefined_script}]))};
        {ignore, _, []} ->
            ok;
        {_, _, [_|_] = E} ->
            {error, hd(lists:reverse(E))}
    end.
%% - - - - 

prim_loader_list_dir(Dir0) ->
    Dir = fix_path_for_prim_loader(Dir0),
    case erl_prim_loader:list_dir(Dir) of
        error ->
            %% TODO: We could improve the error reasons here
            {error, enoent};
        {ok, _} = Ok ->
            Ok
    end.

prim_loader_read_file(File0) ->
    File = fix_filename_for_prim_loader(File0),
    case erl_prim_loader:get_file(File) of
        {ok, Bin, _} ->
            {ok, Bin};
        error ->
            %% TODO: perhaps we can improve error reporting here
            {error, enoent}
    end.

fix_filename_for_prim_loader(F) when is_binary(F) ->
    binary_to_list(F);
fix_filename_for_prim_loader(F) when is_atom(F) ->
    atom_to_list(F);
fix_filename_for_prim_loader(F) when is_list(F) ->
    F.

%% file:list_dir/1 accepts a binary argument, incl trailing / (even multiple)
fix_path_for_prim_loader(Dir) when is_atom(Dir) ->
    fix_path_for_prim_loader(atom_to_binary(Dir, utf8));
fix_path_for_prim_loader(Dir) ->
    re:replace(Dir, <<"/+$">>, <<>>, [{return,list}]).

-ifdef(TEST).

setup_file_test_() ->
    {foreach,
     fun() ->
             create_zip()
     end,
     fun(Zip) ->
             file:delete(Zip)
     end,
     [
      fun(Zip) ->
              [
               fun() -> t_consult(Zip) end,
               fun() -> t_script(Zip) end,
               fun() -> t_read_file(Zip) end,
               fun() -> t_list_dir(Zip) end
              ]
      end]}.

create_zip() ->
    Z = "test.ez",
    {ok, Cwd} = file:get_cwd(),
    {ok, Bin1} = file:read_file("rebar.config"),
    {ok, Bin2} = file:read_file("rebar.config.script"),
    {ok, ZipF} = zip:create(Z, [{"rebar.config", Bin1},
                                {"rebar.config.script", Bin2}],
                            [{cwd, Cwd}]),
    ZipF.

t_consult(Zip) ->
    F = "rebar.config",
    {ok, Res} = file:consult(F),
    {ok, Res} = setup_file:consult(F),
    {ok, Res} = setup_file:consult(filename:join(Zip, F)),
    ok.

t_script(Zip) ->
    {ok, Cfg} = file:consult("rebar.config"),
    F = "rebar.config.script",
    Opts = [{'CONFIG', Cfg}],
    {ok, Res} = file:script(F, Opts),
    {ok, Res} = setup_file:script(F, Opts),
    {ok, Res} = setup_file:script(filename:join(Zip, F), Opts),
    ok.

t_read_file(Zip) ->
    F = "rebar.config",
    ZipF = filename:join(Zip, F),
    {ok, Bin} = file:read_file(F),
    {ok, Bin} = setup_file:read_file(F),
    {ok, Bin} = setup_file:read_file(ZipF),
    {ok, Bin} = setup_file:read_file(list_to_binary(ZipF)),
    {ok, Bin} = setup_file:read_file(list_to_atom(ZipF)),
    ok.

t_list_dir(Zip) ->
    {ok, Fs} = file:list_dir("."),
    {ok, Fs} = setup_file:list_dir("."),
    {ok, Fs1} = setup_file:list_dir(Zip),
    {ok, Fs1} = setup_file:list_dir(Zip ++ "/"),
    [] = Fs1 -- Fs,
    ["rebar.config", "rebar.config.script"] = lists:sort(Fs1),
    ok.

-endif.
