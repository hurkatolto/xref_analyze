-module(xref_gen_cmd).

-callback command_name() ->
    string().

-callback execute(Args :: list(string())) ->
    ok.

-callback usage() ->
    string().
