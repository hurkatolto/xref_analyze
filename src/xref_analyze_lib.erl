%%% ---------------------------------------------------------------------------
%%% @author Terry Buhl
%%% ---------------------------------------------------------------------------
-module(xref_analyze_lib).

-export([fmt_msg/2]).

fmt_msg(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
