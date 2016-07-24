-module(qdhserv_cmdline).

-export([
         option_spec_list/0,
         parse_args/1,
         usage/1
        ]).

-export_type([
              opt_specs/0,
              options/0
             ]).

-include_lib("kernel/include/inet.hrl").

-type opt_specs() :: [getopt:option_spec()].
-type options() :: [getopt:option()].

-spec option_spec_list() -> opt_specs().
option_spec_list() ->
    {ok, CurDir} = file:get_cwd(),

    [
     {help,            $h,        "help",            undefined,               "Show help"},
     {verbose,         $v,        "verbose",         {boolean, false},        "Verbose output"},
     {id,              $i,        "id",              {string, "qdhserv"},     "Server name"},
     {action_start,    undefined, "start",           undefined,               "Start server using --id"},
     {action_stop,     undefined, "stop",            undefined,               "Stop server using --id"},
     {action_kill,     undefined, "kill",            undefined,               "Kill all HTTP servers"},
     {action_list,     undefined, "list",            undefined,               "List all HTTP servers"},
     {cookie,          $c,        "cookie",          {string, "default"},     "Erlang cookie"},
     {port,            $p,        "port",            {integer, 33558},        "Server HTTP port"},
     {server_root,     $r,        "server-root",     {string, "/tmp"},        "Server root for log files"},
     {document_root,   $d,        "document-root",   {string, CurDir},        "Document root"},
     {directory_index, $I,        "directory-index", {string, "index.html"},  "Comma-separated list of directory index files"},
     {bind_address,    undefined, "bind-address",    {string, "*"},           "Bind address (IP address, hostname, or '*')"},
     {inet6,           $6,        "ipv6",            {boolean, false},        "Listen on IPv6 only"},
     {inet,            $4,        "ipv4",            {boolean, true},         "Listen on IPv4 only"}
    ].

-spec parse_args(Args) -> Result when
      Args :: [string()], Result :: {ok, {Action, Config}} |
                                    {error, ErrorText},
      Action :: qdhserv_types:action(), Config :: proplists:proplist(),
      ErrorText :: string().
parse_args(Args) ->
    OptSpecList = option_spec_list(),
    Result = case getopt:parse(OptSpecList, Args) of
                 {ok, {Opts, NonOpts}} ->
                     show_parse_results(Opts, NonOpts),
                     make_action_cfg(OptSpecList, Opts, NonOpts);
                 Error ->
                     Error
             end,
    wrap_result(OptSpecList, Result).

usage(PgmName) ->
    getopt:usage(option_spec_list(), PgmName).

%%%====================================================================
%%% Internal functions
%%%====================================================================
-spec make_action_cfg(Opts, NonOpts) -> Result when
      Opts :: options(), NonOpts :: [string()],
      Result :: {ok, {Action, Config}} | {error, Reason},
      Action :: qdhserv_types:action(), Config :: qdhserv_types:config(), Reason :: term().
make_action_cfg(Opts, []) ->
    try
        Action = get_action(Opts),
        QdhCfg = make_qdhserv_cfg(Opts),
        HttpdCfg = make_httpd_cfg(Opts),
        Cfg = [{qdhserv, QdhCfg}, {httpd, HttpdCfg}],
        {ok, {Action, Cfg}}
    catch
        throw:Error ->
            {error, Error}
    end;
make_action_cfg(_Opts, NonOpts) ->
    {error, {invalid_arg, NonOpts}}.

-spec make_action_cfg(OptSpecList, Opts, NonOpts) -> Result when
      OptSpecList :: opt_specs(), Opts :: options(), NonOpts :: [string()],
      Result :: {ok, {Action, Config}} | {error, Reason},
      Action :: qdhserv_types:action(), Config :: qdhserv_types:config(), Reason :: term().
make_action_cfg(OptSpecList, Opts, NonOpts) ->
    case help_wanted(Opts) of
        true ->
            {ok, {action_help, []}};
        false ->
            make_checked_action_cfg(OptSpecList, Opts, NonOpts)
    end.

-spec make_checked_action_cfg(OptSpecList, Opts, NonOpts) -> Result when
      OptSpecList :: opt_specs(), Opts :: options(), NonOpts :: [string()],
      Result :: {ok, {Action, Config}} | {error, Reason},
      Action :: qdhserv_types:action(), Config :: qdhserv_types:config(), Reason :: term().
make_checked_action_cfg(OptSpecList, Opts, NonOpts) ->
    case getopt:check(OptSpecList, Opts) of
        ok ->
            make_action_cfg(Opts, NonOpts);
        Error ->
            Error
    end.

-spec help_wanted(Opts) -> boolean() when Opts :: options().
help_wanted(Opts) ->
    lists:member(help, Opts).

-spec get_action(Opts) -> Result when
      Opts :: options(), Result :: qdhserv_types:action().
get_action(Opts) ->
    L = lists:foldl(fun(action_start, Acc) -> [action_start | Acc];
                       (action_stop,  Acc) -> [action_stop | Acc];
                       (action_kill,  Acc) -> [action_kill | Acc];
                       (action_list,  Acc) -> [action_list | Acc];
                       (_           , Acc) -> Acc
                    end, [], Opts),
    case L of
        [] ->
            action_default;
        [Action] ->
            Action;
        [_|_] ->
            Actions = action_list(option_spec_list()),
            throw({invalid_option, "only one of " ++ Actions ++ " allowed"})
    end.

make_qdhserv_cfg(Opts) ->
    [cookie(Opts),
     verbose(Opts)].

-spec make_httpd_cfg(Opts) -> Result
    when Opts :: qdhserv_types:config(),
         Result :: qdhserv_types:httpd_config().
make_httpd_cfg(Opts) ->
    Keys = [server_root, document_root],
    [
        server_name(Opts),
        port(Opts),
        directory_index(Opts),
        bind_address(Opts),
        ipfamily(Opts) | qdhserv_util:copy_props(Keys, Opts)
    ].

server_name(Opts) ->
    {_, ServerName} = qdhserv_util:req_prop(id, Opts),
    {server_name, ServerName}.

cookie(Opts) ->
    qdhserv_util:map_prop(fun(X) -> check_cookie(X) end,
                          qdhserv_util:req_prop(cookie, Opts)).

port(Opts) ->
    qdhserv_util:map_prop(fun(X) -> check_port(X) end,
                          qdhserv_util:req_prop(port, Opts)).

directory_index(Opts) ->
    qdhserv_util:map_prop(fun(X) -> parse_directory_index(X) end,
                          qdhserv_util:req_prop(directory_index, Opts)).


bind_address(Opts) ->
    qdhserv_util:map_prop(fun(X) -> parse_bind_address(X) end,
                          qdhserv_util:req_prop(bind_address, Opts)).

verbose(Opts) ->
    lists:foldl(fun({verbose, _} = V, _Acc) -> V;
                   (_, Acc) -> Acc
                end, qdhserv_util:req_prop(verbose, Opts), Opts).

%%--------------------------------------------------------------------
%% @doc
%% Return `{ipfamily, Atom}' where Atom is the last `true' inet property,
%% defaulting to `inet' (ipv4).
%% @end
%%--------------------------------------------------------------------
-spec ipfamily(Opts) -> Result when
      Opts :: options(), Result :: {ipfamily, inet | inet6 }.
ipfamily(Opts) ->
    lists:foldl(fun({inet,     true}, _  ) -> {ipfamily, inet};
                   ({inet6,    true}, _  ) -> {ipfamily, inet6};
                   (_Any            , Acc) -> Acc
                end, {ipfamily, inet}, Opts).

-spec check_port(Port) -> Port when
      Port :: 1..16#FFFF.
check_port(Port) when is_integer(Port), Port > 0, Port < 16#FFFF ->
    Port;
check_port(Port)  ->
    throw({invalid_option_arg, {port, Port}}).

-spec check_cookie(Cookie) -> Cookie when
      Cookie :: nonempty_string().
check_cookie([_|_] = Cookie) ->
    Cookie;
check_cookie(Cookie)  ->
    throw({invalid_option_arg, {cookie, Cookie}}).

-spec parse_directory_index(DirIndex) -> Result when
      DirIndex :: nonempty_string(), Result :: [nonempty_string()].
parse_directory_index(DirIndex) ->
    string:tokens(DirIndex, ",").

-spec parse_bind_address(S) -> Result when
      S :: nonempty_string(),
      Result :: 'any' |
                inet:ip4_address() |
                inet:ip6_address() |
                nonempty_string().
parse_bind_address("*") ->
    any;
parse_bind_address(Str) when is_list(Str) ->
    case inet:parse_address(Str) of
        {ok, IPAddr} ->
            IPAddr;
        {error, _} -> % hostname?
            case inet:gethostbyname(Str) of
                {ok, #hostent{}} ->
                    Str;
                _ ->
                    throw({invalid_option_arg, {bind_address, Str}})
            end
    end.

-spec wrap_result(OptSpecList, Result) -> WrappedResult when
      OptSpecList :: opt_specs(), Result :: OkResult | {error, term()},
      WrappedResult ::  OkResult | {error, nonempty_string()},
      OkResult :: {ok, term()}.
wrap_result(_OptSpecList, {ok, _} = Result) ->
    Result;
wrap_result(OptSpecList, Error) ->
    {error, lists:flatten(getopt:format_error(OptSpecList, Error))}.

-spec action_list(OptSpecList) -> Result when
      OptSpecList :: opt_specs(), Result :: string().
action_list(OptSpecList) ->
    L = lists:foldr(fun({Name,_,_,_,_} = Opt, Acc) ->
                            case atom_to_list(Name) of
                                "action_" ++ _Rest ->
                                    [option_name(Opt) | Acc];
                                _ ->
                                    Acc
                            end
                    end, [], OptSpecList),
    string:join(L, ", ").

option_name({_,ShortName,undefined,_,_}) -> [$-, ShortName];
option_name({_,undefined,LongName,_,_}) -> "--" ++ LongName.

show_parse_results(Opts, NonOpts) ->
    case proplists:get_value(verbose, Opts) of
        true ->
            qdhserv_util:msg("Parse results: Opts: ~p, NonOpts: ~p~n",
                             [Opts, NonOpts]);
        _ ->
            ok
    end.
