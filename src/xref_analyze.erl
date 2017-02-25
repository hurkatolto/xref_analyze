%%% ---------------------------------------------------------------------------
%%% @author Terry Buhl
%%% ---------------------------------------------------------------------------
-module(xref_analyze).

-export([main/1]).

-include("../include/xref_analyze.hrl").

%%% ---------------------------------------------------------------------------
%%% API
%%% ---------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% <p>
%% Generate callgraphs for the given modules and function entries. The functions
%% given to the escript should be exported. If no functions are given, then
%% all exported functions will be drawn.
%% </p>
%%
%% <p>
%% Usage:
%% </p>
%%
%% <p><code>
%% xref_analyze Beam_modules --opts="opt=val,..." --entries="module:function/arity,..."
%% </code></p>
%%
%% @end
%%------------------------------------------------------------------------------

main(Parameters) ->
    io:format("~p ~p main: '~p' ~n", [?MODULE, ?LINE, main]),
    Res = load_modules(),
    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]),
    execute_cmd(Parameters).

execute_cmd([]) ->
    display_usage();
execute_cmd(["-h"]) ->
    display_usage();
execute_cmd(["--help"]) ->
    display_usage();
execute_cmd([Cmd | Parameters]) ->
    CmdMod = find_cmd_mod(Cmd),
    CmdMod:execute(Parameters).

-spec cb_modules() ->
    list(module()).
cb_modules() ->
    lists:filter(fun is_cmd_module/1, [M || {M, _} <- code:all_loaded()]).

-spec is_cmd_module(module()) ->
    boolean().
is_cmd_module(Mod) ->
    string:str(atom_to_list(Mod), "xref_cmd_") =:= 1 andalso
        has_functions(Mod, [{command_name, 0}, {execute, 1}]).

-spec has_functions(module(), list({module(), integer()})) ->
    boolean().
has_functions(Mod, Functions) ->
    io:format("~p ~p {Mod, Functions}: '~p' ~n", [?MODULE, ?LINE, {Mod, Functions}]),
    Exported = Mod:module_info(exports),
    io:format("~p ~p Exported: '~p' ~n", [?MODULE, ?LINE, Exported]),
    Res = Functions -- Exported =:= [],
    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]),
    Res.

-spec display_usage() ->
    ok.
display_usage() ->
    io:format("xref_analyze:\n\n"
              "Execute commands related to xref/calltree graphs.\n"
              "Implemented commands:\n\n" ++ display_cmds()).

-spec display_cmds() ->
    list(string()).
display_cmds() ->
    Cmds = cb_modules(),
    io:format("~p ~p Cmds: '~p' ~n", [?MODULE, ?LINE, Cmds]),
    lists:map(fun(Mod) ->
            Command = Mod:command_name(),
            Usage = Mod:usage(),
            "  - " ++ Command ++ ":\n" ++ Usage ++ "\n\n"
        end, Cmds).

-spec find_cmd_mod(string()) ->
    module().
find_cmd_mod(Command) ->
    Commands = [{M, M:command_name()} || M <- cb_modules()],
    case find_commands(Command, Commands) of
        [{CbMod, _}] ->
            CbMod;
        [] ->
            io:format("No such command: ~p~n", [Command]),
            halt(1);
        Modules ->
            io:format("Multiple matching commands for (~p): ~p", [Command, Modules]),
            halt(1)
    end.

-spec find_commands(string(), list({module(), string()})) ->
    list({module(), string()}).
find_commands(Cmd, Cmds) ->
    lists:filter(fun({_Mod, MaybeMatch}) -> string:str(MaybeMatch, Cmd) =:= 1 end,
                 Cmds).

%% TODO:
%% Don't use the cmd module names here, but load everything in the beam directory,
%% something like:
%%    BeamDir = filename:dirname(code:which(?MODULE)),
%%    Mods = modules_in_dir(BeamDir),
%%    [code:ensure_loaded(M) || M <- Mods].
-spec load_modules() ->
    term().
load_modules() ->
    [code:load_file(M) || M <- [xref_cmd_callgraphs]].

