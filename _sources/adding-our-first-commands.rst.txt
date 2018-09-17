Adding our First Commands
=========================

.. note::

    While the content of this book is still valid, the code may not run with
    latest versions of the tools and libraries, for an updated version of the
    code check the `Riak Core Tutorial <https://marianoguerra.github.io/riak-core-tutorial/>`_

Now that we have everything set up and we know how commands are implemented it's
time to implement our own.

To start we are going to implement a simple in memory key value store, the
first two commands we are going to implement are the basic ones we need to see
if it works: put and get

To hold the values we are going to use the `Erlang Term Storage (ETS) <http://www.erlang.org/doc/man/ets.html>`_.

Riak Core API
-------------

First we start by `creating the two new metrics <https://github.com/marianoguerra/tanodb/commit/398e3ae0a6ede7529aa1fee3930640c9598a45df#diff-afa3f67ec87f742d64ee9ed311455777R4>`_ for our new commands.

Then we add the commands to tanodb.erl, `get <https://github.com/marianoguerra/tanodb/commit/398e3ae0a6ede7529aa1fee3930640c9598a45df#diff-6f7251bf9e224ebabd766f0331b848adR15>`_ and
`put <https://github.com/marianoguerra/tanodb/commit/398e3ae0a6ede7529aa1fee3930640c9598a45df#diff-6f7251bf9e224ebabd766f0331b848adR19>`_ we extract the common code to hash
the key to a vnode to a private function called `send_to_one <https://github.com/marianoguerra/tanodb/commit/398e3ae0a6ede7529aa1fee3930640c9598a45df#diff-6f7251bf9e224ebabd766f0331b848adR25>`_.

On the riak_core side, that is, in the tanodb_vnode module, on init `we create
our ETS table <https://github.com/marianoguerra/tanodb/commit/398e3ae0a6ede7529aa1fee3930640c9598a45df#diff-942e4ef944df628266f096d2fbcd4348R30>`_, the name is tanodb_<partition> where <partition> is the partition id.

Then we `add two new function clauses to handle_command <https://github.com/marianoguerra/tanodb/commit/398e3ae0a6ede7529aa1fee3930640c9598a45df#diff-942e4ef944df628266f096d2fbcd4348R41>`_, one for put and one for get. The logic is quite simple.

The code from tanodb.erl:

.. code-block:: erlang

    get(Key) ->
        tanodb_metrics:core_get(),
        send_to_one(Key, {get, Key}).

    delete(Key) ->
        tanodb_metrics:core_delete(),
        send_to_one(Key, {delete, Key}).

    put(Key, Value) ->
        tanodb_metrics:core_put(),
        send_to_one(Key, {put, Key, Value}).

    % private functions

    send_to_one(Key, Cmd) ->
        DocIdx = riak_core_util:chash_key(Key),
        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, tanodb),
        [{IndexNode, _Type}] = PrefList,
        riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd,
             tanodb_vnode_master).

The relevant code from tanodb_vnode.erl:

.. code-block:: erlang

    handle_command({put, Key, Value}, _Sender,
                   State=#state{table_name=TableName, partition=Partition}) ->
        ets:insert(TableName, {Key, Value}),
        {reply, {ok, Partition}, State};

    handle_command({get, Key}, _Sender,
                   State=#state{table_name=TableName, partition=Partition}) ->
        case ets:lookup(TableName, Key) of
            [] ->
                {reply, {not_found, Partition, Key}, State};
            [Value] ->
                {reply, {found, Partition, {Key, Value}}, State}
        end;

    handle_command({delete, Key}, _Sender,
                   State=#state{table_name=TableName, partition=Partition}) ->
        case ets:lookup(TableName, Key) of
            [] ->
                {reply, {not_found, Partition, Key}, State};
            [Value] ->
                true = ets:delete(TableName, Key),
                {reply, {found, Partition, {Key, Value}}, State}
        end;

Test it
.......

Stop, build, start and in the console we run some commands.

First try getting the key "k1" from bucket "mybucket", which doesn't exist:

.. code-block:: erlang

    (tanodb@127.0.0.1)2> tanodb:get({<<"mybucket">>, <<"k1">>}).

    {not_found,228359630832953580969325755111919221821239459840,
               {<<"mybucket">>,<<"k1">>}}

We get not_found back with the partition that handled the command and the bucket
and key that wasn't found.

Now let's put that key:

.. code-block:: erlang

    (tanodb@127.0.0.1)3> tanodb:put({<<"mybucket">>, <<"k1">>}, 42).

    {ok,228359630832953580969325755111919221821239459840}

We just get ok back, let's try to get it again:

.. code-block:: erlang

    (tanodb@127.0.0.1)4> tanodb:get({<<"mybucket">>, <<"k1">>}).

    {found,228359630832953580969325755111919221821239459840,
           {{<<"mybucket">>,<<"k1">>},{{<<"mybucket">>,<<"k1">>},42}}}

Now we get the value back.

Let's try the same with another key:

.. code-block:: erlang

    (tanodb@127.0.0.1)5> tanodb:get({<<"mybucket">>, <<"k2">>}).

    {not_found,1210306043414653979137426502093171875652569137152,
               {<<"mybucket">>,<<"k2">>}}

Notice that the partition id changed, this is because the key hashed to a different
vnode.

.. code-block:: erlang

    (tanodb@127.0.0.1)6> tanodb:put({<<"mybucket">>, <<"k2">>}, 42).

    {ok,1210306043414653979137426502093171875652569137152}

.. code-block:: erlang

    (tanodb@127.0.0.1)7> tanodb:get({<<"mybucket">>, <<"k2">>}).

    {found,1210306043414653979137426502093171875652569137152,
           {{<<"mybucket">>,<<"k2">>},{{<<"mybucket">>,<<"k2">>},42}}}


REST API
--------

Let's expose our new functions as a REST API, first we `add a new route to
cowboy for our store <https://github.com/marianoguerra/tanodb/commit/88afaad754db69b1c8967e2fe6e4625aab6fe6aa#diff-4477d4dd0aa2db0e274a56c9158207bdR74>`_, the API will be like this:

* POST /store/:bucket/:key <json-body>: stores <json-body> under {:bucket, :key}

  + returns 204 No Content on success

* GET /store/:bucket/:key: 

  + returns 404 if :key doesn't exist on :bucket
  + returns 200 and the value stored under {:bucket, :key} if found

The implementation of the store api is quite simple if you know cowboy, it's
in the `tanodb_http_store.erl file <https://github.com/marianoguerra/tanodb/blob/88afaad754db69b1c8967e2fe6e4625aab6fe6aa/apps/tanodb/src/tanodb_http_store.erl>`_.

Test it
.......

Do the usual stop, build, run and then from another shell:

.. code-block:: sh

    $ http localhost:8080/store/mybucket/bob

Returns

.. code-block:: http

    HTTP/1.1 404 Not Found
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:16:16 GMT
    server: Cowboy

Let's put something on that bucket/key:

.. code-block:: sh

    $ http post localhost:8080/store/mybucket/bob name=bob color=yellow

.. code-block:: http

    HTTP/1.1 204 No Content
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:17:25 GMT
    server: Cowboy

And try to get it again:

.. code-block:: sh

    $ http localhost:8080/store/mybucket/bob

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 31
    content-type: application/json
    date: Fri, 30 Oct 2015 17:18:06 GMT
    server: Cowboy

    {
        "color": "yellow",
        "name": "bob"
    }

Implementing Delete
-------------------

Let's implement the delete command and REST API so our API is complete.

We start as usual `adding the metrics for the delete command <https://github.com/marianoguerra/tanodb/commit/735058ec0c00f1045682982f527cfe0a70a21537#diff-afa3f67ec87f742d64ee9ed311455777R4>`_, then `add the delete function on the tanodb module <https://github.com/marianoguerra/tanodb/commit/735058ec0c00f1045682982f527cfe0a70a21537#diff-6f7251bf9e224ebabd766f0331b848adR19>`_ which is really similar to get.

After that we `add the new function clause in handle_command in our vnode <https://github.com/marianoguerra/tanodb/commit/735058ec0c00f1045682982f527cfe0a70a21537#diff-942e4ef944df628266f096d2fbcd4348R53>`_, notice that it returns the same values as get, this is to get
back the last value in case it was found or inform us that there wasn't a value
with that bucket and key.

Finally we `handle the DELETE HTTP method in our cowboy handler <https://github.com/marianoguerra/tanodb/commit/735058ec0c00f1045682982f527cfe0a70a21537#diff-49cafd1f97d6013b2a41319db4c7961fR36>`_.

Test it
.......

Let's start by testing the core API, we get a key that is not there:

.. code-block:: erlang

    (tanodb@127.0.0.1)1> tanodb:get({<<"mybucket">>, <<"k1">>}).

    {not_found,228359630832953580969325755111919221821239459840,
               {<<"mybucket">>,<<"k1">>}}

Then set it to the value 42:

.. code-block:: erlang

    (tanodb@127.0.0.1)2> tanodb:put({<<"mybucket">>, <<"k1">>}, 42).

    {ok,228359630832953580969325755111919221821239459840}

Get it to make sure it's there:

.. code-block:: erlang

    (tanodb@127.0.0.1)3> tanodb:get({<<"mybucket">>, <<"k1">>}).

    {found,228359630832953580969325755111919221821239459840,
           {{<<"mybucket">>,<<"k1">>},{{<<"mybucket">>,<<"k1">>},42}}}

Proceed to delete it, notice that it returns the last seen value and the
result has the same shape as a get call:

.. code-block:: erlang

    (tanodb@127.0.0.1)4> tanodb:delete({<<"mybucket">>, <<"k1">>}).

    {found,228359630832953580969325755111919221821239459840,
           {{<<"mybucket">>,<<"k1">>},{{<<"mybucket">>,<<"k1">>},42}}}

We get it again to make sure it was deleted:

.. code-block:: erlang

    (tanodb@127.0.0.1)5> tanodb:get({<<"mybucket">>, <<"k1">>}).

    {not_found,228359630832953580969325755111919221821239459840,
               {<<"mybucket">>,<<"k1">>}}

And try to delete it again to see how it handles trying to delete a key that
is not there:

.. code-block:: erlang

    (tanodb@127.0.0.1)6> tanodb:delete({<<"mybucket">>, <<"k1">>}).

    {not_found,228359630832953580969325755111919221821239459840,
               {<<"mybucket">>,<<"k1">>}}

Now that we checked it works on the Erlang shell, let's try the REST API, we
will do the same as before, first get and expect not found:

.. code-block:: sh

    $ http localhost:8080/store/mybucket/bob

    HTTP/1.1 404 Not Found
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:32:17 GMT
    server: Cowboy

Then POST a value:

.. code-block:: sh

    $ http post localhost:8080/store/mybucket/bob name=bob color=yellow

.. code-block:: http

    HTTP/1.1 204 No Content
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:32:21 GMT
    server: Cowboy

GET it to make sure it's there:

.. code-block:: sh

    $ http localhost:8080/store/mybucket/bob

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 31
    content-type: application/json
    date: Fri, 30 Oct 2015 17:32:23 GMT
    server: Cowboy

    {
        "color": "yellow",
        "name": "bob"
    }

DELETE it:

.. code-block:: sh

    $ http delete localhost:8080/store/mybucket/bob

.. code-block:: http

    HTTP/1.1 204 No Content
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:32:27 GMT
    server: Cowboy

GET it back to make sure it's actually deleted:

.. code-block:: sh

    $ http localhost:8080/store/mybucket/bob

.. code-block:: http

    HTTP/1.1 404 Not Found
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:32:28 GMT
    server: Cowboy

DELETE it again to see how it handles a missing delete:

.. code-block:: sh

    $ http delete localhost:8080/store/mybucket/bob

.. code-block:: http

    HTTP/1.1 404 Not Found
    content-length: 0
    content-type: application/json
    date: Fri, 30 Oct 2015 17:43:03 GMT
    server: Cowboy


