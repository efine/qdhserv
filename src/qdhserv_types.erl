-module(qdhserv_types).

-export_type([
              action/0,
              bind_address/0,
              config/0,
              httpd_config/0,
              httpd_prop/0,
              ipfamily/0,
              port_range/0,
              qdhserv_config/0,
              qdhserv_prop/0
             ]).

-type action() ::   'action_default'
                  | 'action_help'
                  | 'action_kill'
                  | 'action_list'
                  | 'action_start'
                  | 'action_stop'
                  .

-type bind_address() :: 'any' |
                        inet:ip4_address() |
                        inet:ip6_address() |
                        nonempty_string().

-type port_range() :: 1..16#FFFF.
-type ipfamily() :: 'inet' | 'inet6'.

-type httpd_prop() ::   {'bind_address', bind_address()}
                      | {'directory_index', string()}
                      | {'document_root', string()}
                      | {'ipfamily', ipfamily()}
                      | {'port', port_range()}
                      | {'server_name', string()}
                      | {'server_root', string()}
                      .

-type httpd_config() :: [httpd_prop()].

-type qdhserv_prop() :: {'cookie', string()} | {'verbose', boolean()}.
-type qdhserv_config() :: [qdhserv_prop()].

-type config_prop() :: {qdhserv, qdhserv_config()} |
                       {httpd, httpd_config()}.
-type config() :: [config_prop()].



