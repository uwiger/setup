

#Module setup#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Setup utility for erlang applications.



__Behaviours:__ [`application`](application.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#data_dir-0">data_dir/0</a></td><td>Returns the configured log dir, or a best guess (<code>home()/log.Node</code>).</td></tr><tr><td valign="top"><a href="#find_env_vars-1">find_env_vars/1</a></td><td>Searches all loaded apps for instances of the <code>Env</code> environment variable.</td></tr><tr><td valign="top"><a href="#home-0">home/0</a></td><td>Returns the configured <code>home</code> directory, or a best guess (<code>$CWD</code>).</td></tr><tr><td valign="top"><a href="#log_dir-0">log_dir/0</a></td><td>Returns the configured log dir, or a best guess (<code>home()/log.Node</code>).</td></tr><tr><td valign="top"><a href="#patch_app-1">patch_app/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Application start function.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Application stop function
end.</td></tr><tr><td valign="top"><a href="#verify_dir-1">verify_dir/1</a></td><td>Ensures that the directory Dir exists and is writable.</td></tr><tr><td valign="top"><a href="#verify_directories-0">verify_directories/0</a></td><td>Ensures that essential directories exist and are writable.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="data_dir-0"></a>

###data_dir/0##




<pre>data_dir() -&gt; Directory</pre>
<br></br>




Returns the configured log dir, or a best guess (`home()/log.Node`)<a name="find_env_vars-1"></a>

###find_env_vars/1##




<pre>find_env_vars(Env) -&gt; [{AppName, Value}]</pre>
<br></br>






Searches all loaded apps for instances of the `Env` environment variable.

The environment variables may contain instances of
`$APP`, `$PRIV_DIR`, `$LIB_DIR`, `$DATA_DIR`, `$LOG_DIR`, `$HOME`,
inside strings or binaries, and these will be replaced with actual values
for the current system (`$APP` simply expands to the name of the current
application).<a name="home-0"></a>

###home/0##




<pre>home() -&gt; Directory</pre>
<br></br>




Returns the configured `home` directory, or a best guess (`$CWD`)<a name="log_dir-0"></a>

###log_dir/0##




<pre>log_dir() -&gt; Directory</pre>
<br></br>




Returns the configured log dir, or a best guess (`home()/log.Node`)<a name="patch_app-1"></a>

###patch_app/1##




`patch_app(A) -> any()`

<a name="start-2"></a>

###start/2##




<pre>start(X1::Type, Args) -&gt; {ok, pid()}</pre>
<br></br>




Application start function.<a name="stop-1"></a>

###stop/1##




<pre>stop(X1::State) -&gt; ok</pre>
<br></br>




Application stop function
end
<a name="verify_dir-1"></a>

###verify_dir/1##




<pre>verify_dir(Directory::Dir) -&gt; Dir</pre>
<br></br>




Ensures that the directory Dir exists and is writable.<a name="verify_directories-0"></a>

###verify_directories/0##




<pre>verify_directories() -&gt; ok</pre>
<br></br>




Ensures that essential directories exist and are writable.
Currently, only the log directory is verified.