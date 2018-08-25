-module(qdhserv_main).

-define(RC_SUCCESS, 0).
-define(RC_ERROR, 1).
-define(RC_FATAL, 2).

-export([
          main/2
    ]).

-import(qdhserv_util, [msg/2, err_msg/2, to_s/1]).

-type return_code() :: ?RC_SUCCESS | ?RC_ERROR | ?RC_FATAL.
-type cleanup_arg() :: 'help' |
                       {error, string()} |
                       {exception, Class :: atom(), Reason :: term()} |
                       {return_code(), string()} |
                       return_code().

-record(cfg, {
          my_cfg :: qdhserv_types:qdhserv_config(),
          my_node :: node(),
          sa_node :: node(),
          httpd_cfg :: qdhserv_types:httpd_config()
         }).

-type cfgrec() :: #cfg{}.
-type cfgfun() :: fun(() -> cfgrec()).

%%====================================================================
%% API
%%====================================================================
-spec main(ScriptName, Args) -> return_code() when
      ScriptName :: string(),
      Args :: [string()].
main(ScriptName, Args) ->
    Result = main(Args),
    cleanup(ScriptName, Result).

%%====================================================================
%% Internal functions
%%====================================================================
-spec main(Args) -> Result when
      Args :: [string()], Result :: cleanup_arg().
main(Args) ->
    try qdhserv_cmdline:parse_args(Args) of
        {ok, {Action, Config}} ->
            try_run(Action, Config);
        {error, _Errmsg} = Err ->
            Err
    catch
        Class:Reason ->
            err_msg("Class: ~p, Reason: ~p~n", [Class, Reason]),
            {exception, Class, Reason}
    end.

-spec try_run(Action, Config) -> Result when
      Action :: qdhserv_types:action(),
      Config :: qdhserv_types:config(),
      Result :: cleanup_arg().
try_run(Action, Config) ->
    try
        Init = fun() ->
                       if_verbose(Config,
                                  fun() ->
                                          msg("Config: ~p\n", [Config])
                                  end),
                       Cfg = mkconfig(Config),
                       qdhserv_util:start_distributed(Cfg#cfg.my_node,
                                                      shortnames),
                       erlang:set_cookie(node(), cookie(Cfg#cfg.my_cfg)),
                       Cfg
               end,
        run(Action, Init)
    catch
        throw:{ping_timeout, Node} ->
            {?RC_FATAL, "Timed out pinging " ++ to_s(Node)}
    end.

-spec run(Action, Init) -> Result when
      Action :: qdhserv_types:action(), Init :: cfgfun(),
      Result :: cleanup_arg().
run(action_help, _Init) ->
    help;
run(action_default, Init) ->
    run(action_help, Init);
run(action_start, Init) ->
    Cfg = Init(),
    start_standalone_node(Cfg#cfg.sa_node, Cfg#cfg.my_cfg),
    start_http_server(Cfg#cfg.sa_node, Cfg#cfg.httpd_cfg);
run(action_stop, Init) ->
    Cfg = Init(),
    stop_http_server(Cfg#cfg.sa_node, Cfg#cfg.httpd_cfg);
run(action_kill, Init) ->
    Cfg = Init(),
    stop_standalone_node(Cfg#cfg.sa_node, Cfg#cfg.my_cfg);
run(action_list, Init) ->
    Cfg = Init(),
    list_http_servers(Cfg#cfg.sa_node).


-spec cleanup(ScriptName, Arg) -> return_code() when
      ScriptName :: nonempty_string(),
      Arg :: cleanup_arg().
cleanup(ScriptName, help) ->
    usage(ScriptName),
    cleanup(ScriptName, ?RC_ERROR);
cleanup(ScriptName, {error, Errmsg}) ->
    usage(ScriptName),
    err_msg("***** ~s~n~n", [qdhserv_util:to_s(Errmsg)]),
    cleanup(ScriptName, ?RC_ERROR);
cleanup(ScriptName, {exception, Class, Reason}) ->
    err_msg("***** ~p:~n~p~n", [Class, Reason]),
    err_msg("~p~n~n", [erlang:get_stacktrace()]),
    cleanup(ScriptName, ?RC_FATAL);
cleanup(ScriptName, {RC, Errmsg}) when is_integer(RC) ->
    err_msg("***** ~s~n~n", [Errmsg]),
    cleanup(ScriptName, RC);
cleanup(_ScriptName, RC) when is_integer(RC) ->
    return_code(RC).

%%% ----------------------------------------------------------------------
%%% @doc Handle return code. Currently does nothing, but handy for adding
%%% logging and other things later on.
%%% @end
%%% ----------------------------------------------------------------------
-spec return_code(RC) -> RC when RC :: return_code().
return_code(RC) when is_integer(RC) ->
    RC.


-spec usage(PgmName) -> ok when PgmName :: string().
usage(PgmName) ->
    qdhserv_cmdline:usage(PgmName),
    ok.

-spec mkconfig(Config) -> Result
    when Config :: qdhserv_types:config(), Result :: cfgrec().
mkconfig(Config) ->
    SANode = short_node_name(qdhserv),
    MyNode = random_node_name(),
    {qdhserv, MyCfg} = qdhserv_util:req_prop(qdhserv, Config),
    {httpd, HttpdCfg} = qdhserv_util:req_prop(httpd, Config),
    #cfg{sa_node = SANode,
         my_node = MyNode,
         httpd_cfg = HttpdCfg,
         my_cfg = MyCfg}.

-spec start_http_server(SANode, HttpdCfg) -> Result when
      SANode :: node(),
      HttpdCfg :: qdhserv_types:httpd_config(),
      Result :: cleanup_arg().
start_http_server(SANode, HttpdCfg) ->
    {_, ServerName} = qdhserv_util:req_prop(server_name, HttpdCfg),
    case get_httpd_info(SANode, ServerName) of
        {ok, _Info} ->
            {?RC_SUCCESS, "Server " ++ ServerName ++ " already running."};
        {error, not_found} ->
            start_httpd(SANode, HttpdCfg);
        Err ->
            Err
    end.

-spec stop_http_server(SANode, HttpdCfg) -> Result when
      SANode :: node(),
      HttpdCfg :: qdhserv_types:httpd_config(),
      Result :: cleanup_arg().
stop_http_server(SANode, HttpdCfg) ->
    {_, ServerName} = qdhserv_util:req_prop(server_name, HttpdCfg),
    case get_httpd_info(SANode, ServerName) of
        {ok, Info} ->
            {_, Pid} = qdhserv_util:req_prop(pid, Info),
            stop_httpd(SANode, Pid);
        {error, not_found} ->
            {?RC_SUCCESS, "Server " ++ ServerName ++ " not found."}
    end.

-spec list_http_servers(SANode) -> Result when
      SANode :: node(), Result :: cleanup_arg().
list_http_servers(SANode) ->
    case get_all_httpd_info(SANode) of
        {ok, Httpds} ->
            S = [io_lib:format("~s\t~s\t~s~n",
                               [server_name_s(PL),
                                address_s(PL),
                                port_s(PL)]) || PL <- Httpds],
            io:put_chars(S),
            ?RC_SUCCESS;
        {error, not_found} ->
            {?RC_SUCCESS, "No servers found.\n"}
    end.

-spec start_httpd(SANode, HttpdCfg) -> Result when
      SANode :: node(), HttpdCfg :: qdhserv_types:httpd_config(),
      Result :: cleanup_arg().
start_httpd(SANode, HttpdCfg) ->
    case do_rpc(SANode, inets, start, [httpd, HttpdCfg]) of
        {ok, _Pid} ->
            ?RC_SUCCESS;
        {error, {already_started, _Pid}} ->
            {?RC_SUCCESS, "Server already started under a different name"};
        Err ->
            Err
    end.

-spec stop_httpd(SANode, Pid) -> Result when
      SANode :: node(), Pid :: pid(), Result :: cleanup_arg().
stop_httpd(SANode, Pid) when is_pid(Pid) ->
    case do_rpc(SANode, inets, stop, [httpd, Pid]) of
        ok ->
            ?RC_SUCCESS;
        Err ->
            Err
    end.

-spec get_all_httpd_info(SANode) -> Result when
      SANode :: node(),
      Result :: {ok, [proplists:proplist()]} | {error, not_found}.
get_all_httpd_info(SANode) ->
    try do_rpc(SANode, inets, services_info, []) of
        Info when is_list(Info) ->
            {ok, [[{pid, Pid} | PL] || {httpd, Pid, PL} <- Info]};
        _ ->
            {error, not_found}
    catch
        {rpcerror, {nodedown, _}} ->
            {error, not_found}
    end.

-spec get_httpd_info(SANode, ServerName) -> Result when
      SANode :: node(), ServerName :: nonempty_string(),
      Result :: cleanup_arg().
get_httpd_info(SANode, ServerName) ->
    try do_rpc(SANode, inets, services_info, []) of
        Info when is_list(Info) ->
            find_server(ServerName, Info);
        {error, _} = Err ->
            Err
    catch
        {rpcerror, {nodedown, _}} ->
            {error, not_found}
    end.

find_server(ServerName, Info) ->
    Httpds = [[{pid, Pid} | PL] || {httpd, Pid, PL} <- Info,
                                   has_server_name(ServerName, PL)],
    case Httpds of
        [Httpd|_] ->
            {ok, Httpd};
        [] ->
            {error, not_found}
    end.

has_server_name(ServerName, PL) ->
    proplists:get_value(server_name, PL) =:= ServerName.

do_rpc(Node, M, F, A) ->
    try
        qdhserv_util:do_rpc(Node, M, F, A, 5000)
    catch
        exception:{rpcerror, {Reason, _}} ->
            Msg = io_lib:format("Remote call to ~p failed: ~p",
                                [Node, Reason]),
            {error, lists:flatten(Msg)}
    end.

-spec short_node_name(Node) -> Result
    when Node :: nonempty_string() | node(), Result :: node().
short_node_name(Node) ->
    SNode = to_s(Node),
    SName = case string:tokens(SNode, "@") of
        [_,_] ->
            SNode;
        [Base] ->
            {ok, Host} = inet:gethostname(),
            Base ++ "@" ++ Host
    end,
    qdhserv_util:to_a(SName).

-spec random_node_name() -> node().
random_node_name() ->
    S = atom_to_list(?MODULE) ++ "_" ++ integer_to_list(rand:uniform(1000)),
    short_node_name(S).

-spec start_standalone_node(Node, Cfg) -> Result
    when Node :: node(), Cfg :: qdhserv_types:qdhserv_config(),
         Result :: any().
start_standalone_node(Node, Cfg) ->
    {_, Verbose} = VerboseProp = qdhserv_util:req_prop(verbose, Cfg),
    Cmd = mkcmdline(["erl_call", maybe_s("-v", Verbose),
                     "-s", "-sname", to_s(Node),
                     "-c", cookie_s(cookie(Cfg)),
                     "-a", "'application ensure_all_started [inets]'"]),
    Verbose andalso mktrue(msg("os:cmd(~s)~n",[Cmd])),
    CmdResult = os:cmd(Cmd),
    Verbose andalso mktrue(msg("~s~n", [CmdResult])),
    qdhserv_util:wait_for_node(Node, 5000, cbfun(VerboseProp)).

stop_standalone_node(Node, Cfg) ->
    {_, Verbose} = qdhserv_util:req_prop(verbose, Cfg),
    Cmd = mkcmdline(["erl_call", maybe_s("-v", Verbose), "-q",
                     "-sname", to_s(Node),
                     "-c", cookie_s(cookie(Cfg))]),
    Verbose andalso mktrue(msg("os:cmd(~s)~n",[Cmd])),
    CmdResult = os:cmd(Cmd),
    Verbose andalso mktrue(msg("~s~n", [CmdResult])),
    ?RC_SUCCESS.

cbfun({verbose, false}) ->
    fun(_Node, _Msg) -> ok end;
cbfun({verbose, true}) ->
    qdhserv_util:default_callback().

maybe_s(S, true) when is_list(S) -> S;
maybe_s(S, false) when is_list(S) -> "".

mkcmdline(Args) ->
    string:join(Args, " ").

mktrue(_) -> true.

server_name_s(PL) -> req_val(server_name, PL).
address_s(PL) ->
    case req_val(bind_address, PL) of
        any ->
            "any";
        IPAddr ->
            inet:ntoa(IPAddr)
    end.
port_s(PL) -> to_s(req_val(port, PL)).

req_val(K, PL) ->
    {_, V} = qdhserv_util:req_prop(K, PL),
    V.

-spec cookie_s(Cookie) -> SCookie
    when Cookie :: atom(), SCookie :: nonempty_string().
cookie_s(Cookie) ->
    to_s(Cookie).

-spec cookie(QdhservCfg) -> Cookie
    when QdhservCfg :: qdhserv_types:qdhserv_config(), Cookie :: atom().
cookie(QdhservCfg) ->
    case qdhserv_util:req_prop(cookie, QdhservCfg) of
        {_, "default"} ->
            erlang:get_cookie();
        {_, Cookie} ->
            qdhserv_util:to_a(Cookie)
    end.

if_verbose(Config, Fun) when is_function(Fun, 0) ->
    QC = proplists:get_value(qdhserv, Config, []),
    Verbose = proplists:get_value(verbose, QC, false),
    Verbose andalso mktrue(Fun()).

