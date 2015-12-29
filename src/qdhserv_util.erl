-module(qdhserv_util).

-export([
        copy_props/2,
        do_rpc/4,
        do_rpc/5,
        err_msg/2,
        map_prop/2,
        msg/2,
        ping_until_timeout/3,
        req_prop/2,
        set_dist_ports/2,
        sleep/1,
        start_distributed/2,
        timestamp_ms/0,
        to_a/1,
        to_s/1,
        wait_for_node/1,
        wait_for_node/2,
        wait_for_node/3
        ]).

%% Internal exports
-export([
        default_callback/0
        ]).

-define(RPC_TIMEOUT, 60000).

-type callback_fun() :: fun((Node :: node(), Act :: string()) -> any()).

-spec set_dist_ports(Min, Max) -> 'ok'
    when Min :: qdhserv_types:port(), Max :: qdhserv_types:port().
set_dist_ports(Min, Max) when is_integer(Min),
                              is_integer(Max),
                              Min >= 1024,
                              Max >= Min ->
	application:set_env(kernel, inet_dist_listen_min, Min),
	application:set_env(kernel, inet_dist_listen_max, Max).

-spec start_distributed(Node, NameType) -> 'ok'
    when Node :: node(), NameType :: 'longnames' | 'shortnames'.
start_distributed(Node, NameType) when NameType == shortnames;
                                       NameType == longnames ->
    case net_kernel:start([Node, NameType]) of
        {ok, _Pid} ->
            ok;
        {error, {{already_started, _Pid}, _}} ->
            ok;
        {error, Reason} ->
            throw(Reason)
    end.

-spec default_callback() -> callback_fun().
default_callback() ->
    fun(Node_, Act_) ->
            msg("~s ~p~n", [Act_, Node_])
    end.

-spec wait_for_node(Node) -> any()
    when Node :: atom().
wait_for_node(Node) ->
    wait_for_node(Node, 30000).

-spec wait_for_node(Node, TimeoutMs) -> any()
    when Node :: node(), TimeoutMs :: non_neg_integer().
wait_for_node(Node, TimeoutMs) ->
    wait_for_node(Node, TimeoutMs, default_callback()).

-spec wait_for_node(Node, TimeoutMs, Callback) -> any()
    when Node :: node(), TimeoutMs :: non_neg_integer(),
         Callback :: callback_fun().
wait_for_node(Node, TimeoutMs, Callback) when is_function(Callback, 2) ->
    Ref = erlang:send_after(TimeoutMs, self(), timeout),
    ping_until_timeout(Node, Ref, Callback).

-spec ping_until_timeout(Node, Ref, Callback) -> any()
    when Node :: node(), Ref :: reference(), Callback :: callback_fun().
ping_until_timeout(Node, Ref, Callback) when is_function(Callback, 2) ->
    Callback(node(), "My node is"),
    Callback(erlang:get_cookie(), "My cookie is"),
    Callback(Node, "Pinging"),
    case net_adm:ping(Node) of
        pong ->
            _ = erlang:cancel_timer(Ref),
            receive after 0 -> ok end, % Flush queue
            Callback(Node, "Connected to");
        pang ->
            Callback(Node, "Ping failed to"),
            receive
                timeout ->
                    Callback(Node, timeout),
                    throw({ping_timeout, Node})
            after
                1000 ->
                    ping_until_timeout(Node, Ref, Callback)
            end
    end.

%%--------------------------------------------------------------------
%%% Perform an RPC call and throw on error
%%%--------------------------------------------------------------------
-spec do_rpc(Node, M, F, A) -> any()
    when Node :: atom(), M :: atom() | tuple(), F :: atom(), A :: [any()].
do_rpc(Node, M, F, A) ->
    do_rpc(Node, M, F, A, ?RPC_TIMEOUT).

-spec do_rpc(Node, M, F, A, Timeout) -> any()
    when Node :: atom(), M :: atom() | tuple(), F :: atom(), A :: [any()],
         Timeout :: 'infinity' | non_neg_integer().
do_rpc(Node, M, F, A, Timeout) ->
    try rpc:call(Node, M, F, A, Timeout) of
        {badrpc, Reason} ->
            throw({rpcerror, {Reason, {Node, M, F, A}}});
        Result ->
            Result
        catch _:Why ->
            throw(Why)
    end.

-spec err_msg(Fmt, Args) -> 'ok'
    when Fmt :: nonempty_string(), Args :: [any()].
err_msg(Fmt, Args) ->
    {TsFmt, TsArgs} = add_ts(Fmt, Args),
    io:format(standard_error, TsFmt, TsArgs).

-spec msg(Fmt, Args) -> 'ok'
    when Fmt :: nonempty_string(), Args :: [any()].
msg(Fmt, Args) ->
    {TsFmt, TsArgs} = add_ts(Fmt, Args),
    io:format(TsFmt, TsArgs).

-spec timestamp_ms() -> PosixTimeMs
    when PosixTimeMs :: non_neg_integer().
timestamp_ms() ->
    {M, S, U} = os:timestamp(),
    (M * 1000000 + S) * 1000 + (U div 1000).

-spec to_a(X) -> atom()
    when X :: term().
to_a(X) when is_atom(X)    -> X;
to_a(X)                    -> list_to_atom(to_s(X)).

-spec to_s(X) -> maybe_improper_list()
    when X :: term().
to_s(X) when is_list(X)    -> X;
to_s(X) when is_atom(X)    -> atom_to_list(X);
to_s(X) when is_integer(X) -> integer_to_list(X);
to_s(X) when is_binary(X)  -> binary_to_list(X);
to_s(X)                    -> lists:flatten(io_lib:format("~p", [X])).

-spec sleep(Ms) -> 'ok'
    when Ms :: 'infinity' | non_neg_integer().
sleep(Ms) ->
    receive after Ms -> ok end.

-spec copy_props(Keys, FromPL) -> Result
    when Keys :: [any()], FromPL :: proplists:proplist(),
         Result :: proplists:proplist().
copy_props(Keys, FromPL) ->
    [req_prop(K, FromPL) || K <- Keys].

-spec map_prop(Fun, {K, V}) -> Result
    when K :: any(), V :: any(), Fun :: fun((V) -> V1),
         Result :: {K, V1}, V1 :: any().
map_prop(Fun, {K, V}) when is_function(Fun, 1) ->
    {K, Fun(V)}.

-spec req_prop(K, PL) -> Result
    when K :: any(), PL :: proplists:proplist(), Result :: tuple().
req_prop(K, PL) ->
    case lists:keysearch(K, 1, PL) of
        {value, KV} ->
            KV;
        false ->
            throw({missing_required_key, K})
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec add_ts(Fmt, Args) -> Result
    when Fmt :: nonempty_string(), Args :: [any()], Result :: {Fmt1, Args1},
         Fmt1 :: nonempty_string(), Args1 :: nonempty_list().
add_ts(Fmt, Args) ->
    {"[~s] " ++ Fmt, [lists:flatten(iso8601_ts()) | Args]}.

-spec iso8601_ts() -> iolist().
iso8601_ts() ->
    {{Yr, Mo, Dy}, {H, M, S}} = calendar:now_to_universal_time(os:timestamp()),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Yr, Mo, Dy, H, M, S]).

