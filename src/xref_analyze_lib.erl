%%% ---------------------------------------------------------------------------
%%% @author Terry Buhl
%%% ---------------------------------------------------------------------------
-module(xref_analyze_lib).

-include("../include/xref_analyze.hrl").

-export([fmt_msg/2,
         parse_args/2,
         copy_beams_to_temp/2,
         get_dirs_and_mods/1,
         delete_temp_dir/1]).

%%% ---------------------------------------------------------------------------
%%% API
%%% ---------------------------------------------------------------------------

-spec fmt_msg(string(), list(term())) ->
    string().
fmt_msg(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

parse_args(Parameters, OptTypes) ->
    parse_args(Parameters, [], [], [], OptTypes).

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------
parse_args([], Mods, Opts, Entries, _OptTypes) ->
    {Mods, Opts, Entries};
parse_args([[$-, $-, $o, $p, $t, $s, $= | Options] | T], Mods, Opts, Entries, OptTypes) ->
    parse_args(T, Mods, parse_opts(Options, OptTypes) ++ Opts, Entries, OptTypes);
parse_args([[$-, $-, $e, $n, $t, $r, $i, $e, $s, $= | EntryStrings] | T], Mods, Opts, Entries, OptTypes) ->
    parse_args(T, Mods, Opts, parse_entries(EntryStrings) ++ Entries, OptTypes);
parse_args([Module | T], Mods, Opts, Entries, OptTypes) ->
    parse_args(T, [Module | Mods], Opts, Entries, OptTypes).

parse_opts(Opts, OptTypes) ->
    Opts1 = string:tokens(Opts, ","),
    lists:map(fun(Opt) ->
            [OptName, OptVal] = string:tokens(Opt, "="),
            OptKey = list_to_atom(OptName),
            {OptKey, convert_opt(OptKey, OptVal, OptTypes)}
        end, Opts1).

-spec convert_opt(atom(), string(), list({option_key(), option_type()})) ->
    term().
convert_opt(Key, Value, OptTypes) ->
    case lists:keyfind(Key, 1, OptTypes) of
        false ->
            throw({bad_option, Key, Value});
        {_Key, Type} ->
            convert_option(Type, Value)
    end.

-spec convert_option(option_type(), string()) ->
    term().
convert_option(string, V) ->
    V;
convert_option(boolean, V) ->
    list_to_atom(V).

parse_entries(Entries) ->
    Entries1 = string:tokens(Entries, ","),
    lists:map(fun(Entry) ->
            [Module, FunctionArity] = string:tokens(Entry, ":"),
            [Function, Arity] = string:tokens(FunctionArity, "/"),
            {list_to_atom(Module), list_to_atom(Function), list_to_integer(Arity)}
        end, Entries1).

-spec copy_beams_to_temp(proplists:proplist(), list({string(), atom()})) ->
    string().
copy_beams_to_temp(Opts, DirsAndMods) ->
    TempDir = proplists:get_value(temp_dir, Opts, ?DEF_TEMP_DIR),
    file:make_dir(TempDir),
    lists:foreach(fun({Dir, Mod}) ->
            File = Dir ++ "/" ++ atom_to_list(Mod) ++ ".beam",
            file:copy(File, TempDir ++ "/" ++ atom_to_list(Mod) ++ ".beam")
        end, DirsAndMods),
    file:set_cwd(TempDir),
    TempDir.

-spec get_dirs_and_mods(Names :: list(string())) ->
    Result :: list({Dir :: string(), Mod :: atom()}).
get_dirs_and_mods(Names) ->
    get_dirs_and_mods(Names, []).

-spec get_dirs_and_mods(Names, Res) -> Result when
      Names :: list(string()),
      Res :: list({string(), atom()}),
      Result :: list({string(), atom()}).
get_dirs_and_mods([], Res) ->
    Res;
get_dirs_and_mods([Name | T], Res) ->
    Dir = filename:dirname(Name),
    [Name2 | _] = lists:reverse(string:tokens(Name, "/")),
    [Name3 | _] = string:tokens(Name2, "."),
    get_dirs_and_mods(T, [{Dir, list_to_atom(Name3)} | Res]).

delete_temp_dir(Dir) ->
    os:cmd("rm -rf " ++ Dir).

