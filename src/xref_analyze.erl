%%% ---------------------------------------------------------------------------
%%% @author Laszlo Toth
%%% ---------------------------------------------------------------------------
-module(xref_analyze).

-export([main/1]).

-export([analyze_mods/1,
         analyze_mods/2,
         analyze_mods/3,
         analyze_code/3,
         analyze_code/4,
         analyze/3,
         analyze_all_modules/2,
         analyze_all_modules/3,
         analyze_all/2,
         analyze_all/3,
         get_deps/1,
         get_dep_modules/2]).


-type option_key() :: include_sub | highlight_pure_functions | generate_clusters | use_multiple_lines.
-type option() :: {option_key(), term()}.
-type options() :: list(option()).

-define(DEF_OPTS,
        [include_sub, highlight_pure_functions, generate_clusters]).
-define(DEFAULT_OUTPUT, "xref_functions").
-define(SENDING_FUNCTIONS, [{gen_server, call, 2},
                            {gen_server, call, 3},
                            {gen_server, multi_call, 2},
                            {gen_server, multi_call, 3},
                            {gen_server, multi_call, 4},
                            {gen_server, cast, 2},
                            {gen_server, abcast, 2},
                            {gen_server, abcast, 3},
                            {gen_fsm, send_event, 2},
                            {gen_fsm, send_all_state_event, 2},
                            {gen_fsm, sync_send_event, 2},
                            {gen_fsm, sync_send_event, 3},
                            {gen_fsm, sync_send_all_state_event, 2},
                            {gen_fsm, sync_send_all_state_event, 3},
                            {gen_fsm, send_event_after, 2},
                            {gen_event, notify, 2},
                            {gen_event, sync_notify, 2},
                            {gen_event, call, 3},
                            {gen_event, call, 4}]).

%%% ---------------------------------------------------------------------------
%%% API
%%% ---------------------------------------------------------------------------

main(ModNames) ->
    {Dirs, Mods} = get_dirs_and_mods(ModNames),
    io:format("~p ~p {Dirs, Mods}: '~p' ~n", [?MODULE, ?LINE, {Dirs, Mods}]),
    Res = [code:add_pathz(Dir) || Dir <- Dirs],
    analyze_mods("xref_analyze", Mods, [{use_multiple_lines, true}]).

-spec analyze_mods(Modules :: list(module())) -> string().
analyze_mods(Modules) ->
    analyze_code(lists:flatten(get_all_exported(Modules)), Modules).

-spec analyze_mods(OutFileName :: string(), Modules :: list(module())) -> string().
analyze_mods(OutFileName, Modules) ->
    analyze_code(OutFileName, lists:flatten(get_all_exported(Modules)), Modules).

-spec analyze_mods(
    OutFileName :: string(),
    Modules :: list(module()),
    Opts :: options()
) -> string().
analyze_mods(OutFileName, Modules, Opts) ->
    analyze_code(OutFileName, lists:flatten(get_all_exported(Modules)), Opts, Modules).

-spec analyze_code(Entries :: list(mfa()), Mods :: list(atom())) -> string().
analyze_code(Entries, Modules) ->
    analyze(Modules, ?DEFAULT_OUTPUT, Entries, ?DEF_OPTS).

-spec analyze_code(OutFileName :: string(), Entries :: list(mfa()), Modules :: [atom()]) ->
    string().
analyze_code(OutFileName, Entries, Modules) ->
    analyze(Modules, OutFileName, Entries, ?DEF_OPTS).

-spec analyze_code(
    OutFileName :: string(),
    Entries :: list(mfa()),
    Opts :: options(),
    Modules :: list(atom())
) ->  string().
analyze_code(OutFileName, Entries, Opts, Modules) ->
    analyze(Modules, OutFileName, Entries, Opts).

analyze(Modules, OutFileName, Entries) ->
    analyze(Modules, OutFileName, Entries, []).

analyze_all_modules(Modules, OutFileName) ->
    analyze_all_modules(Modules, OutFileName, []).

analyze_all_modules(Modules, OutFileName, Opts) ->
    AllModules = get_deps(Modules),
    Functions = get_all_exported(AllModules),
    analyze(Modules, OutFileName, Functions, Opts).

analyze_all(Modules, OutFileName) ->
    analyze_all(Modules, OutFileName, []).

analyze_all(Modules, OutFileName, Opts) ->
    AllModules = get_deps(Modules),
    Functions = get_all_exported(Modules),
    analyze(AllModules, OutFileName, Functions, Opts).

get_deps(Modules) ->
    xref:start(s),
    xref:set_default(s, [{verbose, false}, {warnings, false}]),
    lists:usort(lists:flatten(get_dep_modules(Modules, []))).

get_dep_modules([], _Path) ->
    [];
get_dep_modules([Module | T], Path) ->
    case lists:member(Module, Path) of
        false ->
            xref:add_module(s, Module),
            {ok, CalledMods} = xref:analyze(s, {module_call, Module}),
            [Module] ++ get_dep_modules(CalledMods, [Module | Path]) ++ get_dep_modules(T, Path);
        true ->
            [Module] ++ get_dep_modules(T, Path)
    end.

analyze(Modules, OutFileName, Entries, Opts) ->
    xref:start(s),
    xref:set_default(s, [{verbose, false}, {warnings, false}]),
    [xref:add_module(s, M) || M <- Modules],
    Highlight = lists:member(highlight_pure_functions, Opts),
    CallGraphs0 = [{highlight(Entry, Highlight, Modules), walk_call_graph(Entry, Modules, Opts, [])} || Entry <- Entries],
    {_, CallGraphs} = delete_duplicates(undefined, CallGraphs0, [], []),
    PureFunctionsRepr = lists:flatten(render_pure_functions(CallGraphs)),
    ClusterRepr = lists:flatten(generate_clusters(CallGraphs, Opts, Modules)),
    DotRepr = lists:flatten(generate_dot_repr(CallGraphs)),
    Output =
        "digraph G {\n" ++ ClusterRepr ++ PureFunctionsRepr ++ DotRepr ++ "\n}",
    generate_dot_output(OutFileName, Output),
    generate_ps_file(OutFileName),
    generate_tree_file(CallGraphs, OutFileName),
    try_show_ps_file(OutFileName).

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------
walk_call_graph({_, module_info, _}, _Modules, _Opts, _CallPath) ->
    [];
walk_call_graph(MFAs, Modules, Opts, CallPath) ->
    {ok, Functions} = xref:analyze(s, {call, MFAs}),
    IncludeSub = lists:member(include_sub, Opts),
    Highlight = lists:member(highlight_pure_functions, Opts),

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
    UseMultipleLines = lists:member(use_multiple_lines, Opts),
    case UseMultipleLines of
        true ->
            Res;
        false ->
            Result = lists:usort(Res),
            Result
    end.

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
    PsName = OutFile ++ ".png",
    Cmd = "dot -x -Tpng  -Gratio=auto -Gdpi=340 " ++ GvName ++ " -o " ++ PsName,
    io:format("~p ~p Cmd: '~p' ~n", [?MODULE, ?LINE, Cmd]),
    Res = os:cmd(Cmd),
    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]),
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


generate_clusters(CallGraphs, Opts, InternalModules) ->
    GenerateClusters = lists:member(generate_clusters, Opts),
    IncludeSub = lists:member(include_sub, Opts),
    case GenerateClusters andalso IncludeSub of
        true ->
            "    subgraph cluster0 {\n" ++
            "    " ++ display_cluster_mfas(generate_cluster_nodes(CallGraphs, InternalModules)) ++
                "    }\n";
        false ->
            []
    end.

display_cluster_mfas(Mfas) ->
    display_cluster_mfas1(lists:usort(Mfas)).

display_cluster_mfas1([]) ->
    [];
display_cluster_mfas1([Mfa | T]) ->
    "    " ++ fmt(Mfa) ++ ";\n" ++ display_cluster_mfas1(T).

generate_cluster_nodes([], _InternalModules) ->
    [];
generate_cluster_nodes([{{{M, _, _} = Mfa, _IsPure}, CalledMfas} | T], InternalModules) ->
    case lists:member(M, InternalModules) of
        false ->
            render_cluster_node(Mfa, CalledMfas, InternalModules, T);
%                generate_cluster_nodes(CalledMfas, InternalModules) ++
%                generate_cluster_nodes(T, InternalModules);
        true ->
            generate_cluster_nodes(CalledMfas, InternalModules) ++
                generate_cluster_nodes(T, InternalModules)
    end.


render_cluster_node(Mfa, CalledMfas, InternalModules, T) ->
    [Mfa] ++
        generate_cluster_nodes(CalledMfas, InternalModules) ++
        generate_cluster_nodes(T, InternalModules).

get_all_exported(Modules) ->
    [lists:flatten(get_mod_functions(M)) || M <- Modules].

get_mod_functions(Mod) ->
    [{Mod, F, A} || {F, A} <- Mod:module_info(exports)].

-spec get_dirs_and_mods(Names :: list(string())) ->
    {Dirs :: list(string()), Mods :: list(atom())}.
get_dirs_and_mods(Names) ->
    get_dirs_and_mods(Names, [], []).

-spec get_dirs_and_mods(Names, DirsAcc, ModsAcc) -> Res when
      Names :: list(string()),
      DirsAcc :: list(string()),
      ModsAcc :: list(atom()),
      Res :: {list(string()), list(atom())}.
get_dirs_and_mods([], DirsAcc, ModsAcc) ->
    {DirsAcc, ModsAcc};
get_dirs_and_mods([Name | T], DirsAcc, ModsAcc) ->
    Dir = filename:dirname(Name),
    [Name2 | _] = lists:reverse(string:tokens(Name, "/")),
    [Name3 | _] = string:tokens(Name2, "."),
    get_dirs_and_mods(T, [Dir | DirsAcc], [list_to_atom(Name3) | ModsAcc]).

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
