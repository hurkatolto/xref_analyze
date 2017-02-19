-module(xref_analyze_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
%%    xref_analyze_sup:start_link().
    {ok, self()}.

stop(_State) ->
    ok.
