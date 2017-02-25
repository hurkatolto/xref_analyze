%%% ---------------------------------------------------------------------------
%%% @author Terry Buhl
%%% ---------------------------------------------------------------------------
-module(xref_cmd_callgraphs).

-include("../include/xref_analyze.hrl").

-behaviour(xref_gen_cmd).

-export([command_name/0,
         execute/1,
         usage/0]).

%%% ---------------------------------------------------------------------------
%%% API
%%% ---------------------------------------------------------------------------

-spec command_name() ->
    string().
command_name() ->
    "callgraphs".

-spec execute(list(string())) ->
    ok.
execute(Parameters) ->
    io:format("~p ~p Parameters: '~p' ~n", [?MODULE, ?LINE, Parameters]),
    {ModNames, Opts, Entries} = parse_args(Parameters),
    DirsAndMods = get_dirs_and_mods(ModNames),
    Mods = [M || {_, M} <- DirsAndMods],
    {ok, Cwd} = file:get_cwd(),
    TempDir = copy_beams_to_temp(Opts, DirsAndMods),
    [{module, _} = code:load_file(M) || M <- Mods],
    Entries1 = case Entries of
                    [] -> lists:flatten(get_all_exported(Mods));
                    _ -> Entries
                end,
    analyze_code("xref_analyze", Entries1, Opts, Mods, Cwd),
    delete_temp_dir(TempDir),
    ok.

usage() ->
    "Generate callgraphs for the given modules and function entries. The functions\n"
    "given to the escript should be exported. If no functions are given, then\n"
    "all exported functions will be drawn.\n"
    "\n"
    "Usage:\n"
    "\n"
    "xref_analyze Beam_modules --opts=\"opt=val,...\" --entries=\"module:function/arity,...\"".

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------

-spec analyze_code(
    OutFileName :: string(),
    Entries :: list(mfa()),
    Opts :: options(),
    Modules :: list(atom()),
    Dir :: string()
) ->  string().
analyze_code(OutFileName, Entries, Opts, Modules, Dir) ->
    analyze(Modules, OutFileName, Entries, Opts, Dir).

analyze(Modules, OutFileName, Entries, Opts, Dir) ->
    Res = xref:start(s),
    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]),
    xref:set_default(s, [{verbose, false}, {warnings, false}]),
    [xref:add_module(s, M) || M <- Modules],
    Highlight = lists:member(highlight_pure_functions, Opts),
    CallGraphs0 = [{highlight(Entry, Highlight, Modules), walk_call_graph(Entry, Modules, Opts, [])} || Entry <- Entries],
    SepEntries = proplists:get_value(separate_entries, Opts, ?DEFAULT_SEPARATE_ENTRIES),
    Output =
        generate_digraph_output(SepEntries, CallGraphs0, Opts),
    file:set_cwd(Dir),
    generate_tree_file(CallGraphs0, OutFileName),
    generate_dot_output(OutFileName, Output),
    generate_ps_file(OutFileName),
    try_show_ps_file(OutFileName).

walk_call_graph({_, module_info, _}, _Modules, _Opts, _CallPath) ->
    [];
walk_call_graph(MFAs, Modules, Opts, CallPath) ->
    {ok, Functions} = xref:analyze(s, {call, MFAs}),
    IncludeSub = proplists:get_value(include_sub, Opts, ?DEF_INCLUDE_SUB),
    Highlight = proplists:get_value(highlight_pure_functions, Opts, ?DEF_HIGHLIGHT_PURE_FUNCTIONS),

    Res =
        lists:foldl(fun({M, _F, _A} = Function, Acc) ->
                case lists:member(M, Modules) orelse IncludeSub of
                    true ->
                        case lists:member(Function, CallPath) of
                            true ->
                                [{highlight(Function, Highlight, Modules), []} | Acc];
                            false ->
                                [{highlight(Function, Highlight, Modules),
                                 walk_call_graph(Function, Modules, Opts, [Function | CallPath])} | Acc]
                        end;
                    false ->
                        Acc
                end
            end, [], Functions),
    Res.

highlight(Function, false = _Highlight, _Modules) ->
    {Function, false};
highlight(Function, true = _Highlight, Modules) ->
    {Function, is_pure(Function, Modules)}.

is_pure({_, module_info, _}, _Modules) ->
    false;
is_pure(Mfa, Modules) ->
    {ok, CalledMfas} = xref:analyse(s, {call, Mfa}),
    calls_sending_function(CalledMfas) orelse is_direct_message_sending(Mfa, Modules).

is_direct_message_sending({M, F, A} = _MFA, Modules) ->
    case lists:member(M, Modules) of
        true ->
            case get_abstract_code({M, F, A}) of
                {ok, FunctionCode} ->
                    parse_clauses(FunctionCode);
                {error, no_code} ->
                    false
            end;
        false ->
            false
    end.

get_abstract_code({M, F, A}) ->
    {ok, {M, [{abstract_code, {_, ModuleRepr}}]}} = beam_lib:chunks(M, [abstract_code]),
    case get_function_repr(ModuleRepr, F, A) of
        [{_, _, _, _, FunctionRepr}] ->
            {ok, FunctionRepr};
        [] ->
            {error, no_code}
    end.

get_function_repr(ModuleRepr, F, A) ->
    lists:filter(fun
            ({function, _, Function, Arity, _}) when Function =:= F, Arity =:= A-> true;
            (_) -> false
        end, ModuleRepr).

parse_clauses([]) ->
    false;
parse_clauses([{clause, _ID, _Head, _, Code} | T]) ->
    parse_abstract_code(Code) orelse parse_clauses(T).

parse_abstract_code([]) ->
    false;
parse_abstract_code([{op, _ID, '!', _ProcSpec, _Msg} | _T]) ->
    true;
parse_abstract_code([{'case', _ID, _Condition, Clauses} | T]) ->
    parse_clauses(Clauses) orelse parse_abstract_code(T);
parse_abstract_code([{'if', _ID, Clauses} | T]) ->
    parse_clauses(Clauses) orelse parse_abstract_code(T);
parse_abstract_code([{'call', _ID, _CallSpec, CallBody} | T]) ->
    parse_call_body(CallBody) orelse parse_abstract_code(T);
parse_abstract_code([{'match', _ID, Fun, {'fun', _, {clauses, Clauses}}} | T]) ->
    (is_fun_called(T, Fun) andalso parse_clauses(Clauses)) orelse parse_abstract_code(T);
parse_abstract_code([_ | T]) ->
    parse_abstract_code(T).

parse_call_body([]) ->
    false;
parse_call_body([{'fun', _ID, {clauses, Clauses}} | T]) ->
    parse_clauses(Clauses) orelse parse_call_body(T);
parse_call_body([_ | T]) ->
    parse_call_body(T).

is_fun_called(_Code, _Fun) ->
    true.

calls_sending_function(Mfas) ->
    lists:any(fun(Mfa) ->
                  lists:member(Mfa, Mfas)
              end, ?SENDING_FUNCTIONS).

render_pure_functions([]) ->
    [];
render_pure_functions([{{Mfa, true = _IsPure}, CalledMfas} | T]) ->
    fmt_pure(Mfa) ++
        render_pure_functions(CalledMfas) ++ render_pure_functions(T);
render_pure_functions([{{Mfa, false = _IsPure}, CalledMfas} | T]) ->
    case has_pure_descendant(CalledMfas) of
        true ->
            fmt_pure(Mfa) ++
                render_pure_functions(CalledMfas) ++ render_pure_functions(T);
        false ->
            render_pure_functions(CalledMfas) ++ render_pure_functions(T)
    end.

fmt_pure(Mfa) ->
    lists:flatten(
        io_lib:format(
            "    \"~p\" [color=red,style=filled];\n",   [Mfa]
        )
    ).

has_pure_descendant([]) ->
    false;
has_pure_descendant([{{_MFA, true = _IsPure}, _CalledMfas} | _T]) ->
    true;
has_pure_descendant([{{_Mfa, false = _IsPure}, CalledMfas} | T]) ->
    has_pure_descendant(CalledMfas) orelse
        has_pure_descendant(T).

generate_dot_repr([]) -> [];
generate_dot_repr([{{Mfa, _IsPure}, CalledMfas} | T]) ->
    FunctionCalls =
        lists:map(fun(CMfa) ->
                fmt(Mfa) ++ "->" ++
                    fmt(element(1, element(1, CMfa))) ++ ";\n"
            end, CalledMfas),
    SubCalls = generate_dot_repr(CalledMfas),
    FunctionCalls ++ SubCalls ++ generate_dot_repr(T).

fmt(Mfa) ->
    lists:flatten(
        io_lib:format(
            "    \"~150p\"", [Mfa]
        )
    ).

generate_dot_output(OutFile0, Output) ->
    OutFile = OutFile0 ++ ".gv",
    file:write_file(OutFile, list_to_binary(Output)).

generate_ps_file(OutFile) ->
    GvName = OutFile ++ ".gv",
    PsName = OutFile ++ ".ps",
    Cmd = "dot -x -Tps2  -Gratio=auto -Gdpi=40 " ++ GvName ++ " -o " ++ PsName,
    io:format("~p ~p Cmd: '~p' ~n", [?MODULE, ?LINE, Cmd]),
    Res = os:cmd(Cmd),
    io:format("~p ~p Res: '~s' ~n", [?MODULE, ?LINE, Res]),
    Res.

try_show_ps_file(OutFile) ->
    PsName = OutFile ++ ".ps",
    os:cmd("evince " ++ PsName ++ "&").

generate_tree_file(Output, OutFileName0) ->
    OutFileName = OutFileName0 ++ ".txt",
    TextOutput = lists:flatten(generate_tree_output(Output, 0)),
    file:write_file(OutFileName, list_to_binary(TextOutput)).

generate_tree_output([], _D) ->
    [];
generate_tree_output([{MFA, Calls} | T], D) ->
    fmt_line(MFA, D) ++ generate_tree_output(Calls, D + 1) ++ generate_tree_output(T, D).

fmt_line({{M,F,A}, _IsHighlighted}, D) ->
    lists:flatten(
        string:copies(" ", 4 * D) ++
            io_lib:format("~p,~p/~p~n", [M,F,A])
    ).

generate_clusters(CallGraphs, Opts) ->
    GenerateClusters = proplists:get_value(generate_clusters, Opts, ?DEF_GENERATE_CLUSTERS),
    case GenerateClusters of
        true ->
            ModuleFunctions = gen_module_funs(CallGraphs),
            generate_cluster_output(ModuleFunctions);
        false ->
            []
    end.

get_all_exported(Modules) ->
    [lists:flatten(get_mod_functions(M)) || M <- Modules].

get_mod_functions(Mod) ->
    [{Mod, F, A} || {F, A} <- Mod:module_info(exports)].

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

delete_duplicates(_Prev, [], DrawnEdges, Res) ->
    {DrawnEdges, Res};
delete_duplicates(Prev, [{{Edge, IsPure}, Edges}  | T], DrawnEdges0, Res) ->
    DrawnEdges = [{Prev, Edge} | DrawnEdges0],
    case lists:member({Prev, Edge}, DrawnEdges0) of
        false ->
            {DrawnEdges1, Result} = delete_duplicates(Edge, Edges, DrawnEdges, []),
            NewRes = [{{Edge, IsPure}, Result} | Res],
            delete_duplicates(Prev, T, DrawnEdges1, NewRes);
        true ->
            delete_duplicates(Prev, T, DrawnEdges, Res)
    end.

get_nodes_header(Entries) ->
    "\t{\n" ++ node_entry_fmt() ++ get_nodes_header1(Entries, []) ++ "\t}\n".

get_nodes_header1([], Res) ->
    Res;
get_nodes_header1([Entry], Res) ->
    Res ++ get_node_header(Entry);
get_nodes_header1([Entry | T], Res) ->
    get_nodes_header1(T, Res ++ get_node_header(Entry)).

get_node_header(Entry) ->
    "\t\t" ++ fmt(Entry) ++ " [fillcolor=red]\n".

node_entry_fmt() ->
    "\t\tnode [margin=0 fontcolor=blue fontsize=22 width=0.5 shape=ellipse style=filled]\n".

generate_digraph_output(true, CallGraphs, Opts) ->
    lists:map(fun(Graph) ->
            generate_digraph_output([Graph], Opts)
        end, CallGraphs);
generate_digraph_output(false, CallGraphs, Opts) ->
    generate_digraph_output(CallGraphs, Opts).

generate_digraph_output(CallGraphs0, Opts) ->
    Entries = get_entries_from_graph(CallGraphs0),
    timer:sleep(100),
    {_, CallGraphs} = delete_duplicates(undefined, CallGraphs0, [], []),
    PureFunctionsRepr = lists:flatten(render_pure_functions(CallGraphs)),
    ClusterRepr = lists:flatten(generate_clusters(CallGraphs, Opts)),
    DotRepr = lists:flatten(generate_dot_repr(CallGraphs)),
    NodesHeader = get_nodes_header(Entries),
    Output =
        "digraph G {\n" ++ NodesHeader ++ ClusterRepr ++ PureFunctionsRepr ++ DotRepr ++ "\n}\n\n",
    Output.

get_entries_from_graph(Graphs) ->
    [E || {{E, _IsPure}, _} <- Graphs].

parse_args(Parameters) ->
    parse_args(Parameters, [], [], []).

parse_args([], Mods, Opts, Entries) ->
    {Mods, Opts, Entries};
parse_args([[$-, $-, $o, $p, $t, $s, $= | Options] | T], Mods, Opts, Entries) ->
    parse_args(T, Mods, parse_opts(Options) ++ Opts, Entries);
parse_args([[$-, $-, $e, $n, $t, $r, $i, $e, $s, $= | EntryStrings] | T], Mods, Opts, Entries) ->
    parse_args(T, Mods, Opts, parse_entries(EntryStrings) ++ Entries);
parse_args([Module | T], Mods, Opts, Entries) ->
    parse_args(T, [Module | Mods], Opts, Entries).

parse_opts(Opts) ->
    Opts1 = string:tokens(Opts, ","),
    lists:map(fun(Opt) ->
            [OptName, OptVal] = string:tokens(Opt, "="),
            OptKey = list_to_atom(OptName),
            {OptKey, convert_opt(OptKey, OptVal)}
        end, Opts1).

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
            Res = file:copy(File, TempDir ++ "/" ++ atom_to_list(Mod) ++ ".beam"),
            io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res])
        end, DirsAndMods),
    file:set_cwd(TempDir),
    TempDir.

delete_temp_dir(Dir) ->
    os:cmd("rm -rf " ++ Dir).

-spec opt_types() ->
    list({option_key(), option_type()}).
opt_types() ->
    [{include_sub, boolean},
     {highlight_pure_functions, boolean},
     {generate_clusters, boolean},
     {separate_entries, boolean},
     {temp_dir, string}].

-spec convert_opt(atom(), string()) ->
    term().
convert_opt(Key, Value) ->
    case lists:keyfind(Key, 1, opt_types()) of
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

gen_module_funs(CallGraphs) ->
    MFAS = gen_module_funs(CallGraphs, []),
    lists:foldl(fun add_mfa/2, [], MFAS).

gen_module_funs([], Res) ->
    Res;
gen_module_funs([{{MFA, _IsPure}, Children} | T], Res) ->
   gen_module_funs(T, [MFA] ++ gen_module_funs(Children, []) ++ Res).

add_mfa({Mod, _Fun, _Arity} = MFA, Acc) ->
    case lists:keyfind(Mod, 1, Acc) of
        false ->
            [{Mod, [MFA]} | Acc];
        {Mod, ModFuns} ->
            case lists:member(MFA, ModFuns) of
                false ->
                    ModFuns2 = [MFA | ModFuns],
                    NewVal = {Mod, ModFuns2},
                    lists:keystore(Mod, 1, Acc, NewVal);
                true ->
                    Acc
            end
    end.

generate_cluster_output(ModFuns) ->
    generate_cluster_output(ModFuns, [], 0).

generate_cluster_output([], Res, _C) ->
    lists:reverse(Res);
generate_cluster_output([{Mod, MFAS} | T], Res, C) ->
    Fmt =
      xref_analyze_lib:fmt_msg(
        "\tsubgraph ~s {\n"
        "\t\tstyle=filled;\n"
        "\t\tcolor=lightgrey;\n"
        "\t\tnode [style=filled,color=blue];\n"
        "\t\t~s;\n"
        "\t\tlabel = \"~p\";\n"
        "\t}\n\n",
        ["cluster_" ++ integer_to_list(C), fmt_node_mfas(MFAS, []), Mod]),
    generate_cluster_output(T, [Fmt | Res], C + 1).

fmt_node_mfas([], Res) ->
    Res;
fmt_node_mfas([MFA], Res) ->
    Res ++ xref_analyze_lib:fmt_msg("\"~p\"", [MFA]);
fmt_node_mfas([MFA | T], Res) ->
    fmt_node_mfas(T, Res ++ xref_analyze_lib:fmt_msg("\"~p\"", [MFA]) ++ " ").
