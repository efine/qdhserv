

# Module qdhserv_util #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#copy_props-2">copy_props/2</a></td><td></td></tr><tr><td valign="top"><a href="#default_callback-0">default_callback/0</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpc-4">do_rpc/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpc-5">do_rpc/5</a></td><td></td></tr><tr><td valign="top"><a href="#err_msg-2">err_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#map_prop-2">map_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#msg-2">msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#ping_until_timeout-3">ping_until_timeout/3</a></td><td></td></tr><tr><td valign="top"><a href="#req_prop-2">req_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_dist_ports-2">set_dist_ports/2</a></td><td></td></tr><tr><td valign="top"><a href="#sleep-1">sleep/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_distributed-1">start_distributed/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_distributed-2">start_distributed/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_network-2">start_network/2</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp_ms-0">timestamp_ms/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_a-1">to_a/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_s-1">to_s/1</a></td><td></td></tr><tr><td valign="top"><a href="#wait_for_node-1">wait_for_node/1</a></td><td></td></tr><tr><td valign="top"><a href="#wait_for_node-2">wait_for_node/2</a></td><td></td></tr><tr><td valign="top"><a href="#wait_for_node-3">wait_for_node/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="copy_props-2"></a>

### copy_props/2 ###

`copy_props(Keys, FromPL) -> any()`

<a name="default_callback-0"></a>

### default_callback/0 ###

`default_callback() -> any()`

<a name="do_rpc-4"></a>

### do_rpc/4 ###

`do_rpc(Node, M, F, A) -> any()`

<a name="do_rpc-5"></a>

### do_rpc/5 ###

`do_rpc(Node, M, F, A, Timeout) -> any()`

<a name="err_msg-2"></a>

### err_msg/2 ###

`err_msg(Fmt, Args) -> any()`

<a name="map_prop-2"></a>

### map_prop/2 ###

`map_prop(Fun, X2) -> any()`

<a name="msg-2"></a>

### msg/2 ###

`msg(Fmt, Args) -> any()`

<a name="ping_until_timeout-3"></a>

### ping_until_timeout/3 ###

`ping_until_timeout(Node, Ref, Callback) -> any()`

<a name="req_prop-2"></a>

### req_prop/2 ###

`req_prop(K, PL) -> any()`

<a name="set_dist_ports-2"></a>

### set_dist_ports/2 ###

`set_dist_ports(Min, Max) -> any()`

<a name="sleep-1"></a>

### sleep/1 ###

`sleep(Ms) -> any()`

<a name="start_distributed-1"></a>

### start_distributed/1 ###

`start_distributed(Node) -> any()`

<a name="start_distributed-2"></a>

### start_distributed/2 ###

`start_distributed(Node, NameType) -> any()`

<a name="start_network-2"></a>

### start_network/2 ###

`start_network(Node, Cookie) -> any()`

<a name="timestamp_ms-0"></a>

### timestamp_ms/0 ###

`timestamp_ms() -> any()`

<a name="to_a-1"></a>

### to_a/1 ###

`to_a(X) -> any()`

<a name="to_s-1"></a>

### to_s/1 ###

`to_s(X) -> any()`

<a name="wait_for_node-1"></a>

### wait_for_node/1 ###

`wait_for_node(Node) -> any()`

<a name="wait_for_node-2"></a>

### wait_for_node/2 ###

`wait_for_node(Node, TimeoutMs) -> any()`

<a name="wait_for_node-3"></a>

### wait_for_node/3 ###

`wait_for_node(Node, TimeoutMs, Callback) -> any()`

