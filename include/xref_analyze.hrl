-ifndef(__XREF_ANALYZE__).

-type option_type() :: boolean | integer | string | float | atom.
-type option_key()  :: include_sub | highlight_pure_functions |
                       generate_clusters | separate_entries | temp_dir.
-type option()      :: {option_key(), term()}.
-type options()     :: list(option()).

-define(DEF_TEMP_DIR, "/tmp/xmerl_analyze").

-define(DEFAULT_OUTPUT, "xref_functions").
-define(DEFAULT_SEPARATE_ENTRIES, true).
-define(DEF_INCLUDE_SUB, false).
-define(DEF_HIGHLIGHT_PURE_FUNCTIONS, true).
-define(DEF_GENERATE_CLUSTERS, true).
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

-endif.
