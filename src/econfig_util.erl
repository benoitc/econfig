%%% -*- erlang -*-
%%%
%%% This file is part of econfig released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(econfig_util).

-export([find_ini_files/1,
         find_files/1, find_files/2, find_files/3,
         implode/2,
         abs_pathname/1,
         to_list/1,
         trim_whitespace/1]).


find_ini_files(Path) ->
    {ok, Files} = file:list_dir(Path),
    IniFiles = [filename:join(Path, Name) || Name <- Files,
        ".ini" =:= filename:extension(Name)],
    lists:usort(IniFiles).


find_files(Paths) ->
    find_files(Paths, [], fun(F) -> F end).

find_files(Paths, Fun) ->
    find_files(Paths, [], Fun).

find_files([], Acc, _Fun) ->
    Acc;
find_files([Path | Rest], Acc, Fun) ->
    case filelib:is_dir(Path) of
        true ->
            IniFiles = econfig_util:find_ini_files(Path),
            Acc1 = Acc ++ lists:map(Fun, IniFiles),
            find_files(Rest, Acc1, Fun);
        false ->
            Acc1 = Acc ++  [Fun(Path)],
            find_files(Rest, Acc1, Fun)
    end.


implode(List, Sep) ->
    implode(List, Sep, []).

implode([], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc));
implode([H], Sep, Acc) ->
    implode([], Sep, [H|Acc]);
implode([H|T], Sep, Acc) ->
    implode(T, Sep, [Sep,H|Acc]).

% given a pathname "../foo/bar/" it gives back the fully qualified
% absolute pathname.
abs_pathname(" " ++ Filename) ->
    % strip leading whitespace
    abs_pathname(Filename);
abs_pathname([$/ |_]=Filename) ->
    Filename;
abs_pathname(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    {Filename2, Args} = separate_cmd_args(Filename, ""),
    abs_pathname(Filename2, Cwd) ++ Args.

abs_pathname(Filename, Dir) ->
    Name = filename:absname(Filename, Dir ++ "/"),
    OutFilename = filename:join(fix_path_list(filename:split(Name), [])),
    % If the filename is a dir (last char slash, put back end slash
    case string:right(Filename,1) of
    "/" ->
        OutFilename ++ "/";
    "\\" ->
        OutFilename ++ "/";
    _Else->
        OutFilename
    end.

to_list(V) when is_list(V) ->
    V;
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_integer(V) ->
    integer_to_list(V);
to_list(V) ->
    lists:flatten(io_lib:format("~p", [V])).

%% @doc trims whitespace
trim_whitespace(Value) ->
    re:replace(
        re:replace(Value, "^[\s\t]+", "", [{return, list}, global]),
        "[\s\t]+$", "", [{return, list}, global]).

%% --
%% private functions
%%

%% @doc takes a heirarchical list of dirs and removes the dots ".", double dots
%% ".." and the corresponding parent dirs.
fix_path_list([], Acc) ->
    lists:reverse(Acc);
fix_path_list([".."|Rest], [_PrevAcc|RestAcc]) ->
    fix_path_list(Rest, RestAcc);
fix_path_list(["."|Rest], Acc) ->
    fix_path_list(Rest, Acc);
fix_path_list([Dir | Rest], Acc) ->
    fix_path_list(Rest, [Dir | Acc]).

%% @doc if this as an executable with arguments, seperate out the arguments
%% ""./foo\ bar.sh -baz=blah" -> {"./foo\ bar.sh", " -baz=blah"}
separate_cmd_args("", CmdAcc) ->
    {lists:reverse(CmdAcc), ""};
separate_cmd_args("\\ " ++ Rest, CmdAcc) -> % handle skipped value
    separate_cmd_args(Rest, " \\" ++ CmdAcc);
separate_cmd_args(" " ++ Rest, CmdAcc) ->
    {lists:reverse(CmdAcc), " " ++ Rest};
separate_cmd_args([Char|Rest], CmdAcc) ->
    separate_cmd_args(Rest, [Char | CmdAcc]).

