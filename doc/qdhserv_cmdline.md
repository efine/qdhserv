

# Module qdhserv_cmdline #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-action">action()</a> ###


<pre><code>
action() = atom()
</code></pre>




### <a name="type-config">config()</a> ###


<pre><code>
config() = <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>




### <a name="type-opt_specs">opt_specs()</a> ###


<pre><code>
opt_specs() = [<a href="getopt.md#type-option_spec">getopt:option_spec()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#option_spec_list-0">option_spec_list/0</a></td><td></td></tr><tr><td valign="top"><a href="#parse_args-1">parse_args/1</a></td><td></td></tr><tr><td valign="top"><a href="#usage-1">usage/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="option_spec_list-0"></a>

### option_spec_list/0 ###

<pre><code>
option_spec_list() -&gt; <a href="#type-opt_specs">opt_specs()</a>
</code></pre>
<br />

<a name="parse_args-1"></a>

### parse_args/1 ###

<pre><code>
parse_args(Args) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Args = [string()]</code></li><li><code>Result = {ok, {Action, Config}} | {error, ErrorText}</code></li><li><code>Action = atom()</code></li><li><code>Config = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>ErrorText = string()</code></li></ul>

<a name="usage-1"></a>

### usage/1 ###

`usage(PgmName) -> any()`

