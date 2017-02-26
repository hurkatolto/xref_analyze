%%% ---------------------------------------------------------------------------
%%% @author Terry Buhl
%%%
%%% @doc
%%% <p>
%%% Display cross-calls between different applications running on different
%%% nodes.
%%% Calls using rpc:call and rpc:multicall will be displayed.
%%% </p>
%%%
%%% <p>
%%% Usage:
%%% </p>
%%%
%%% <p><code>
%%% xref_analyze external_appcalls Beam_modules --opts="opt=val,..."
%%% </code></p>
%%% @end
%%% ---------------------------------------------------------------------------

-module(xref_cmd_external_appcalls).

-include("../include/xref_analyze.hrl").

-behaviour(xref_gen_cmd).

-export([command_name/0,
         execute/1,
         usage/0]).

%%% ---------------------------------------------------------------------------
%%% API
%%% ---------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% Return the name of the command the module implements.
%% @end
%%------------------------------------------------------------------------------
-spec command_name() ->
    string().
command_name() ->
    "external_appcalls".

%%------------------------------------------------------------------------------
%% @doc
%% Execute command
%% @end
%%------------------------------------------------------------------------------
-spec execute(list(string())) ->
    ok.
execute(Parameters) ->
    {ModNames, Opts, _Entries} = xref_analyze_lib:parse_args(Parameters, opt_types()),
    DirsAndMods = xref_analyze_lib:get_dirs_and_mods(ModNames),
    Mods = [M || {_, M} <- DirsAndMods],
    {ok, _Cwd} = file:get_cwd(),
    TempDir = xref_analyze_lib:copy_beams_to_temp(Opts, DirsAndMods),
    [{module, _} = code:load_file(M) || M <- Mods],
    xref_analyze_lib:delete_temp_dir(TempDir),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Return command help.
%% @end
%%------------------------------------------------------------------------------
usage() ->
    "Display cross-calls between different applications running on different\n"
    "nodes. Calls using rpc:call and rpc:multicall will be displayed.\n"
    "\n"
    "Usage:\n"
    "\n"
    "xref_analyze external_appcalls Beam_modules --opts=\"opt=val,...\"\n".


%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------
-spec opt_types() ->
    list({option_key(), option_type()}).
opt_types() ->
    [{generate_clusters, boolean},
     {temp_dir, string}].
