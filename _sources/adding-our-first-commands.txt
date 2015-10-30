Adding our First Commands
=========================

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

:)
