Riak Core Metadata
==================

.. note::

    The first 3 sections are taken from here
    https://gist.github.com/jrwest/d290c14e1c472e562548

1. Overview
-----------

Cluster Metadata is intended to be used by `riak_core` applications
wishing to work with information stored cluster-wide. It is useful for
storing application metadata or any information that needs to be
read without blocking on communication over the network.

1.1 Data Model
..............

Cluster Metadata is a key-value store. It treats values as opaque
Erlang terms that are fully addressed by their "Full Prefix" and
"Key". A Full Prefix is a `{atom() | binary(), atom() | binary()}`,
while a Key is any Erlang term. The first element of the Full Prefix
is referred to as the "Prefix" and the second as the "Sub-Prefix".

1.2 Storage
...........

Values are stored on-disk and a full copy is also maintained
in-memory. This allows reads to be performed only from memory, while
writes are affected in both mediums.

1.3 Consistency
...............

Updates in Cluster Metadata are eventually consistent. Writing a value
only requires acknowledgment from a single node and as previously
mentioned, reads return values from the local node, only.

1.4 Replication
...............

Updates are replicated to every node in the cluster, including nodes
that join the cluster after the update has already reached all nodes
in the previous set of members.

2. API
------

The interface to Cluster Metadata is provided by the
`riak_core_metadata <https://github.com/basho/riak_core/blob/develop/src/riak_core_metadata.erl>`_
module. The module's documentation is the official source for
information about the API, but some details are re-iterated here.

2.1 Reading and Writing
.......................

Reading the local value for a key can be done with the `get/2,3`
functions. Like most `riak_core_metadata` functions, the higher arity
version takes a set of possible options, while the lower arity
function uses the defaults.

Updating a key is done using `put/3.4`. Performing a put only blocks
until the write is affected locally. The broadcast of the update is
done asynchronously.

2.1.1 Deleting Keys
:::::::::::::::::::

Deletion of keys is logical and tombstones are not
reaped. `delete/2,3` act the same as `put/3,4` with respect to
blocking and broadcast.

2.2 Iterators
.............

Along with reading individual keys, the API also allows Full Prefixes
to be iterated over. Iterators can visit both keys and values. They
are not ordered, nor are they read-isolated. However, they do
guarantee that each key is seen *at most once* for the lifetime of an
iterator.

See `iterator/2` and the `itr_*` functions.

2.3 Conflict Resolution
.......................

Conflict resolution can be done on read or write.

On read, if the conflict is resolved, an option, `allow_put`, passed
to `get/3` or `iterator/2` controls whether or not the resolved value
will be written back to local storage and broadcast asynchronously.

On write, conflicts are resolved by passing a function instead of a
new value to `put/3,4`. The function is passed the list of existing
values and can use this and values captured within the closure to
produce a new value to store.

2.4 Detecting Changes in Groups of Keys
.......................................

The `prefix_hash/1` function can be polled to determined when groups
of keys, by Prefix or Full Prefix, have changed.

3. Common Pitfalls & Other Notes
--------------------------------

The following is by no means a complete list of things to keep in mind
when developing on top of Cluster Metadata.

3.1 Storage Limitations
.......................

Cluster Metadata use `dets` for on-disk storage. There is a `dets`
table per Full Prefix, which limits the amount of data stored under
each Full Prefix to 2GB. This size includes the overhead of
information stored alongside values, such as the logical clock and
key.

Since a full-copy of the data is kept in-memory, its usage must also
be considered.

3.2 Replication Limitations
...........................

Cluster Metadata uses disterl for message delivery, like most Erlang
applications. Standard caveats and issues with large and/or too
frequent messages still apply.

3.3 Last-Write Wins
...................

The default conflict resolution strategy on read is
last-write-wins. The usual caveats about the dangers of this method
apply.

3.4 "Pathological Eventual Consistency"
.......................................

The extremely frequent writing back of resolved values after read in
an eventually consistent store where acknowledgment is only required
from one node for both types of operations can lead to an interesting
pathological case where siblings continue to be produce (although the
set does not grow unbounded). A very rough exploration of this can be
found `here <https://gist.github.com/jrwest/f8c0d49174f4db1c4c88>`_).

If a `riak_core` application is likely to have concurrent writes and
wishes to read extremely frequently, e.g. in the Riak request path, it
may be advisable to use `{allow_put, false}` with `get/3`.

4. Playing in the REPL
----------------------

we start by building and running our app:

.. code-block:: shell

    rebar3 release
    rebar3 run

First let's setup some variables, FullPrefix is like an identifier for the
place where we are going to store related values, there can be many, some of
them are used by other components of riak_core as you will see in the next
sections.

.. code-block:: erlang

    (tanodb@127.0.0.1)1> FullPrefix = {<<"tanodb">>, <<"config">>}.
    {<<"tanodb">>,<<"config">>}

Let's start by trying to get a value that is not set, by default we get undefined.

.. code-block:: erlang

    (tanodb@127.0.0.1)2> riak_core_metadata:get(FullPrefix, max_users).
    undefined

We can change that by calling the get function that supports options, one of
them is **default**, so we set it to a value that makes sense for use in case
max_users is not set.

.. code-block:: erlang

    (tanodb@127.0.0.1)3> riak_core_metadata:get(FullPrefix, max_users, [{default, 100}]).
    100

Now let's put the value in the store.

.. code-block:: erlang

    (tanodb@127.0.0.1)4> riak_core_metadata:put(FullPrefix, max_users, 150).
    ok

And try getting it.

.. code-block:: erlang

    (tanodb@127.0.0.1)5> riak_core_metadata:get(FullPrefix, max_users).
    150

Let's put another value.

.. code-block:: erlang

    (tanodb@127.0.0.1)6> riak_core_metadata:put(FullPrefix, max_connections, 100).
    ok

Get all the values in this prefix as a list, the "d" there is because [100] looks
like a string to erlang, don't worry, your value is safe.

.. code-block:: erlang

    (tanodb@127.0.0.1)7> riak_core_metadata:to_list(FullPrefix).
    [{max_connections,"d"},{max_users,[150]}]

Now let's delete a value.

.. code-block:: erlang

    (tanodb@127.0.0.1)8> riak_core_metadata:delete(FullPrefix, max_users).
    ok

And put another one.

.. code-block:: erlang

    (tanodb@127.0.0.1)9> riak_core_metadata:put(FullPrefix, hostname, "tanodb1").
    ok

Now let's list them again, you will see that deleted values are still there but
**marked** with a "thombstone" value (the atom '$deleted'), this means we have
to handle them in our functions if we want to avoid trouble.

.. code-block:: erlang

    (tanodb@127.0.0.1)11> riak_core_metadata:to_list(FullPrefix).
    [{max_connections,"d"},
     {max_users,['$deleted']},
     {hostname,["tanodb1"]}]

Now let's do something more complex, let's iterate over all the values in the
prefix, count the amount of deleted values and accumulate the "alive" ones.

Notice I use a function clause to match the thombstone first and then one to
handle "alive" values.

.. code-block:: erlang

    (tanodb@127.0.0.1)11> riak_core_metadata:fold(fun
    (tanodb@127.0.0.1)11>     ({Key, ['$deleted']}, {Deleted, Alive}) ->
    (tanodb@127.0.0.1)11>         {Deleted + 1, Alive};
    (tanodb@127.0.0.1)11>     ({Key, [Value]}, {Deleted, Alive}) ->
    (tanodb@127.0.0.1)11>         {Deleted, [{Key, Value}|Alive]}
    (tanodb@127.0.0.1)11> end, {0, []}, FullPrefix).

    {1,[{max_connections,100},{hostname,"tanodb1"}]}

There are more functions I didn't show here since this ones are the main ones
you will uses, you can look at the riak_core_metadata module for the other ones,
the module has good documentation for each function.
