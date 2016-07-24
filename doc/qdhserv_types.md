

# Module qdhserv_types #
* [Data Types](#types)

<a name="types"></a>

## Data Types ##




### <a name="type-action">action()</a> ###


<pre><code>
action() = action_default | action_help | action_kill | action_list | action_start | action_stop
</code></pre>




### <a name="type-bind_address">bind_address()</a> ###


<pre><code>
bind_address() = any | <a href="inet.md#type-ip4_address">inet:ip4_address()</a> | <a href="inet.md#type-ip6_address">inet:ip6_address()</a> | nonempty_string()
</code></pre>




### <a name="type-config">config()</a> ###


<pre><code>
config() = [<a href="#type-config_prop">config_prop()</a>]
</code></pre>




### <a name="type-config_prop">config_prop()</a> ###


<pre><code>
config_prop() = {qdhserv, <a href="#type-qdhserv_config">qdhserv_config()</a>} | {httpd, <a href="#type-httpd_config">httpd_config()</a>}
</code></pre>




### <a name="type-httpd_config">httpd_config()</a> ###


<pre><code>
httpd_config() = [<a href="#type-httpd_prop">httpd_prop()</a>]
</code></pre>




### <a name="type-httpd_prop">httpd_prop()</a> ###


<pre><code>
httpd_prop() = {bind_address, <a href="#type-bind_address">bind_address()</a>} | {directory_index, string()} | {document_root, string()} | {ipfamily, <a href="#type-ipfamily">ipfamily()</a>} | {port, <a href="#type-port_range">port_range()</a>} | {server_name, string()} | {server_root, string()}
</code></pre>




### <a name="type-ipfamily">ipfamily()</a> ###


<pre><code>
ipfamily() = inet | inet6
</code></pre>




### <a name="type-port_range">port_range()</a> ###


<pre><code>
port_range() = 1..65535
</code></pre>




### <a name="type-qdhserv_config">qdhserv_config()</a> ###


<pre><code>
qdhserv_config() = [<a href="#type-qdhserv_prop">qdhserv_prop()</a>]
</code></pre>




### <a name="type-qdhserv_prop">qdhserv_prop()</a> ###


<pre><code>
qdhserv_prop() = {cookie, string()} | {verbose, boolean()}
</code></pre>

