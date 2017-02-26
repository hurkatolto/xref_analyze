-ifndef(__XREF_ANALYZE__).

-type option_type() :: boolean | integer | string | float | atom.
-type option_key()  :: include_sub | highlight_pure_functions |
                       generate_clusters | separate_entries | temp_dir.
-type option()      :: {option_key(), term()}.
-type options()     :: list(option()).

-define(DEF_TEMP_DIR, "/tmp/xmerl_analyze").


-define(CMD_MODULES, [xref_cmd_callgraphs, xref_cmd_external_appcalls,
                      xref_cmd_internal_appcalls]).

-endif.
