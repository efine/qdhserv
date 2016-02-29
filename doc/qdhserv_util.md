

# Module qdhserv_util #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-callback_fun">callback_fun()</a> ###


<pre><code>
callback_fun() = fun((Node::node(), Act::string()) -&gt; any())
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#copy_props-2">copy_props/2</a></td><td></td></tr><tr><td valign="top"><a href="#default_callback-0">default_callback/0</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpc-4">do_rpc/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpc-5">do_rpc/5</a></td><td></td></tr><tr><td valign="top"><a href="#err_msg-2">err_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#map_prop-2">map_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#msg-2">msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#ping_until_timeout-3">ping_until_timeout/3</a></td><td></td></tr><tr><td valign="top"><a href="#req_prop-2">req_prop/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_dist_ports-2">set_dist_ports/2</a></td><td></td></tr><tr><td valign="top"><a href="#sleep-1">sleep/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_distributed-2">start_distributed/2</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp_ms-0">timestamp_ms/0</a></td><td></td></tr><tr><td valign="top"><a href="#to_a-1">to_a/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_s-1">to_s/1</a></td><td></td></tr><tr><td valign="top"><a href="#wait_for_node-1">wait_for_node/1</a></td><td></td></tr><tr><td valign="top"><a href="#wait_for_node-2">wait_for_node/2</a></td><td></td></tr><tr><td valign="top"><a href="#wait_for_node-3">wait_for_node/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="copy_props-2"></a>

### copy_props/2 ###

<pre><code>
copy_props(Keys, FromPL) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Keys = [any()]</code></li><li><code>FromPL = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Result = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li></ul>

<a name="default_callback-0"></a>

### default_callback/0 ###

<pre><code>
default_callback() -&gt; <a href="#type-callback_fun">callback_fun()</a>
</code></pre>
<br />

<a name="do_rpc-4"></a>

### do_rpc/4 ###

<pre><code>
do_rpc(Node, M, F, A) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Node = atom()</code></li><li><code>M = atom() | tuple()</code></li><li><code>F = atom()</code></li><li><code>A = [any()]</code></li></ul>

<a name="do_rpc-5"></a>

### do_rpc/5 ###

<pre><code>
do_rpc(Node, M, F, A, Timeout) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Node = atom()</code></li><li><code>M = atom() | tuple()</code></li><li><code>F = atom()</code></li><li><code>A = [any()]</code></li><li><code>Timeout = infinity | non_neg_integer()</code></li></ul>

<a name="err_msg-2"></a>

### err_msg/2 ###

<pre><code>
err_msg(Fmt, Args) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Fmt = nonempty_string()</code></li><li><code>Args = [any()]</code></li></ul>

<a name="map_prop-2"></a>

### map_prop/2 ###

<pre><code>
map_prop(Fun, X2::{K, V}) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>K = any()</code></li><li><code>V = any()</code></li><li><code>Fun = fun((V) -&gt; V1)</code></li><li><code>Result = {K, V1}</code></li><li><code>V1 = any()</code></li></ul>

<a name="msg-2"></a>

### msg/2 ###

<pre><code>
msg(Fmt, Args) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Fmt = nonempty_string()</code></li><li><code>Args = [any()]</code></li></ul>

<a name="ping_until_timeout-3"></a>

### ping_until_timeout/3 ###

<pre><code>
ping_until_timeout(Node, Ref, Callback) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Node = node()</code></li><li><code>Ref = reference()</code></li><li><code>Callback = <a href="#type-callback_fun">callback_fun()</a></code></li></ul>

<a name="req_prop-2"></a>

### req_prop/2 ###

<pre><code>
req_prop(K, PL) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>K = any()</code></li><li><code>PL = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Result = tuple()</code></li></ul>

<a name="set_dist_ports-2"></a>

### set_dist_ports/2 ###

<pre><code>
set_dist_ports(Min, Max) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Min = <a href="qdhserv_types.md#type-port">qdhserv_types:port()</a></code></li><li><code>Max = <a href="qdhserv_types.md#type-port">qdhserv_types:port()</a></code></li></ul>

<a name="sleep-1"></a>

### sleep/1 ###

<pre><code>
sleep(Ms) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Ms = infinity | non_neg_integer()</code></li></ul>

<a name="start_distributed-2"></a>

### start_distributed/2 ###

<pre><code>
start_distributed(Node, NameType) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Node = node()</code></li><li><code>NameType = longnames | shortnames</code></li></ul>

<a name="timestamp_ms-0"></a>

### timestamp_ms/0 ###

<pre><code>
timestamp_ms() -&gt; PosixTimeMs
</code></pre>

<ul class="definitions"><li><code>PosixTimeMs = non_neg_integer()</code></li></ul>

<a name="to_a-1"></a>

### to_a/1 ###

<pre><code>
to_a(X) -&gt; atom()
</code></pre>

<ul class="definitions"><li><code>X = term()</code></li></ul>

<a name="to_s-1"></a>

### to_s/1 ###

<pre><code>
to_s(X) -&gt; maybe_improper_list()
</code></pre>

<ul class="definitions"><li><code>X = term()</code></li></ul>

<a name="wait_for_node-1"></a>

### wait_for_node/1 ###

<pre><code>
wait_for_node(Node) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Node = atom()</code></li></ul>

<a name="wait_for_node-2"></a>

### wait_for_node/2 ###

<pre><code>
wait_for_node(Node, TimeoutMs) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Node = node()</code></li><li><code>TimeoutMs = non_neg_integer()</code></li></ul>

<a name="wait_for_node-3"></a>

### wait_for_node/3 ###

<pre><code>
wait_for_node(Node, TimeoutMs, Callback) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Node = node()</code></li><li><code>TimeoutMs = non_neg_integer()</code></li><li><code>Callback = <a href="#type-callback_fun">callback_fun()</a></code></li></ul>

