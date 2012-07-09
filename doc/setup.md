

#Module setup#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Setup utility for erlang applications.



__Behaviours:__ [`application`](application.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#data_dir-0">data_dir/0</a></td><td>Returns the configured log dir, or a best guess (<code>home()/log.Node</code>).</td></tr><tr><td valign="top"><a href="#find_app-1">find_app/1</a></td><td>Locates application <code>A</code> along $ERL_LIBS or under the OTP root.</td></tr><tr><td valign="top"><a href="#find_env_vars-1">find_env_vars/1</a></td><td>Searches all loaded apps for instances of the <code>Env</code> environment variable.</td></tr><tr><td valign="top"><a href="#home-0">home/0</a></td><td>Returns the configured <code>home</code> directory, or a best guess (<code>$CWD</code>).</td></tr><tr><td valign="top"><a href="#log_dir-0">log_dir/0</a></td><td>Returns the configured log dir, or a best guess (<code>home()/log.Node</code>).</td></tr><tr><td valign="top"><a href="#patch_app-1">patch_app/1</a></td><td>Adds an application's "development" path to a target system.</td></tr><tr><td valign="top"><a href="#read_config_script-3">read_config_script/3</a></td><td></td></tr><tr><td valign="top"><a href="#reload_app-1">reload_app/1</a></td><td>Equivalent to <a href="#reload_app-2"><tt>reload_app(AppName, latest)</tt></a>.</td></tr><tr><td valign="top"><a href="#reload_app-2">reload_app/2</a></td><td>Loads or upgrades an application to the specified version.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Application start function.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Application stop function
end.</td></tr><tr><td valign="top"><a href="#verify_dir-1">verify_dir/1</a></td><td>Ensures that the directory Dir exists and is writable.</td></tr><tr><td valign="top"><a href="#verify_directories-0">verify_directories/0</a></td><td>Ensures that essential directories exist and are writable.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="data_dir-0"></a>

###data_dir/0##




<pre>data_dir() -&gt; Directory</pre>
<br></br>




Returns the configured log dir, or a best guess (`home()/log.Node`)<a name="find_app-1"></a>

###find_app/1##




<pre>find_app(A::atom()) -&gt; [{Vsn, Dir}]</pre>
<br></br>




Locates application `A` along $ERL_LIBS or under the OTP root<a name="find_env_vars-1"></a>

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




<pre>patch_app(AppName::atom()) -&gt; true | {error, Reason}</pre>
<br></br>






Adds an application's "development" path to a target system



This function locates the given application (`AppName`) along the `$ERL_LIBS`  
path, and prepends it to the code path of the existing system. This is useful  
not least when one wants to add e.g. a debugging or trace application to a  
target system.

The function will not add the same path again, if the new path is already
the 'first' path entry for the application `A`.<a name="read_config_script-3"></a>

###read_config_script/3##




`read_config_script(F, Name, Opts) -> any()`

<a name="reload_app-1"></a>

###reload_app/1##




<pre>reload_app(AppName::atom()) -&gt; {ok, NotPurged} | {error, Reason}</pre>
<br></br>




Equivalent to [`reload_app(AppName, latest)`](#reload_app-2).<a name="reload_app-2"></a>

###reload_app/2##




<pre>reload_app(AppName::atom(), ToVsn0::ToVsn) -&gt; {ok, Unpurged} | {error, Reason}</pre>
<ul class="definitions"><li><pre>ToVsn = latest | next | Vsn</pre></li><li><pre>Vsn = string()</pre></li></ul>





Loads or upgrades an application to the specified version



This function is a convenient function for 'upgrading' an application.
It locates the given version (using [`find_app/1`](#find_app-1) and [`pick_vsn/3`](#pick_vsn-3))  
and loads it in the most appropriate way:



* If the application isn't already loaded, it loads the application and    
all its modules.



* If the application is loaded, it generates an appup script and performs
a soft upgrade. If the new version of the application has an `.appup` script    
on-disk, that script is used instead.



The generated appup script is of the form:



* add modules not present in the previous version of the application



* do a soft upgrade on pre-existing modules, using suspend-code_change-resume



* delete modules that existed in the old version, but not in the new.



The purge method used is `brutal_purge` - see [`//sasl/appup`](/Users/uwiger/FL/git/sasl/doc/appup.md).

For details on how the new version is chosen, see [`find_app/1`](#find_app-1) and
[`pick_vsn/3`](#pick_vsn-3).<a name="start-2"></a>

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