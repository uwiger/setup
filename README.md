Setup - Generic setup utility for Erlang-based systems
======================================================

Setup is a simple utility meant to support a structured setup
of Erlang-based applications. It addresses the problem that there is no
standard way to perform common tasks like initialize mnesia, create tables,
populate the database, etc.

My recommendation is to run `setup` from a "load-only" script, such as 
can be created using the `setup_gen` module. This means that all applications
are loaded, all paths are set, and the sys.config file has been read.
In short, all things are prepared for running setup commands.

The `setup` application, when started, searches for a special environment
variable, `'$setup_hooks'` in each loaded application. Each setup hook must
specify a "setup phase number", and an `{M,F,A}` tuple.

More precisely, this is the structure of the `'$setup_hooks'` variable:

<pre>
{'$setup_hooks', [{PhaseNo, [ {M,F,A} ]}]}
</pre>

The `setup` application doesn't care what type PhaseNo has, but I suggest 
sticking to integers. All found setup hooks will be sorted by PhaseNo,
and all callbacks related to each given phase will be called in sequence
before moving on to the next phase.

To begin establishing a convention, I suggest the following:

<table>
     <th>PhaseNo</th> <th>Action</th>
<tr> <td>100</td>     <td>Create database</td> </tr>
<tr> <td>200</td>     <td>Create tables / Configure schema</td> </tr>
<tr> <td>300</td>     <td>Populate database</td> </tr>
</table>

That is, if you have an application that needs to create some mnesia
tables, it can specify a setup hook for phase no 200, knowing that the
mnesia schema will have been created (by someone) by that time.

<pre>
{'$setup_hooks', [{200, {my_setup_mod, create_tables, []}}]}
</pre>

If it also needs to populate the database with some initial data,
it can add another hook for phase no 300, knowing that *all* tables 
will be available at that time:

<pre>
{'$setup_hooks', [{200,
                    [{my_setup_mod, create_tables, []}]
                  }
                  {300,
                    [{my_setup_mod, create_db_entries, []},
                     {my_setup_mod, verify_consistency, []}]
                  }
                 ]}
</pre>

You can add multiple hooks for each phase, and they will be executed
in the order you specify. You shouldn't make any assumptions about whether
your hooks are run before or after other applications' hooks, except what
is dictated by the phase number.

There are some other features as well. Read the man page for `setup`.

Comments and extension proposals are welcome.

Building Edoc
=============
By default, `./rebar doc` generates Markdown files for Github online perusal.
If you want to change this, remove this line from `rebar.config`.

`{edoc_opts, [{doclet, edown_doclet}]}.`
