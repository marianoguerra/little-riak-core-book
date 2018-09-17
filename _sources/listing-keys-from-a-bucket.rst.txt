Listing Keys from a Bucket
==========================

.. note::

    While the content of this book is still valid, the code may not run with
    latest versions of the tools and libraries, for an updated version of the
    code check the `Riak Core Tutorial <https://marianoguerra.github.io/riak-core-tutorial/>`_

Since we already implemented some commands you may be asking yourself, why do
we need a full chapter for another command? well, think again...

Since bucket and key are hashed together to decide to which vnode a request
will go it means that the keys for a given bucket may be distributed in
multiple vnodes, and in case you are running in a cluster this means your keys
are distributed in multiple physical nodes.

This means that to list all the keys from a bucket we have to ask all the
vnodes for the keys on a given bucket and then put the responses together and
return the set of all responses.

For this Riak Core provides something called coverage calls, which are a way
to handle this process of running a command on all vnodes and gathering the
responses.

In this chapter we are going to implement the `tanodb:keys(Bucket)` function
using coverage calls.

Implementing the CORE API
-------------------------

We start as usual by `adding the metric for the keys function <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-afa3f67ec87f742d64ee9ed311455777R4>`_.

Then implement `tanodb:keys/1 <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-6f7251bf9e224ebabd766f0331b848adR27>`_, but as you
may notice it's not similar to the previous ones because of what we talked about
in the introduction.

In this case we call `tanodb_coverage_fsm:start({keys, Bucket}, Timeout)`, which
is a new module, it implements a behavior called `riak_core_coverage_fsm`, short
for riak_core_coverage `finite state machine <https://en.wikipedia.org/wiki/Finite-state_machine>`_, it implements some predefined callbacks that are called on different
states of a finite state machine.

The start function calls `tanodb_coverage_fsm_sup:start_fsm([ReqId, self(), Request, Timeout]) <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-7ccdace934891188d9d1055533cb81b8R20>`_ which starts a supervisor for this new process.

We also need to `register the supervisor in the supervisor tree <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-8ca11d5e05a10f28aec8ac9694b1c14fR27>`_.

As a side note, tanodb_coverage_fsm uses a module called time_compat to avoid
problems with deprecated uses of time in Erlang, for that we need to `add the
module as a dependency <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-31d7a50c99c265ca2793c20961b60979R10>`_.

When we start the fsm with a command ({keys, Bucket}) and a timeout in milliseconds,
it starts a supervisor that starts the finite state machine process, it first
`calls the init function <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-7ccdace934891188d9d1055533cb81b8R27>`_ which initializes
the state of the process and returns some information to riak_core so it knows
what kind of coverage call we want to do, then riak_core calls the
`handle_coverage <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-942e4ef944df628266f096d2fbcd4348R90>`_ function on each vnode and
with each response it `calls process_results <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-7ccdace934891188d9d1055533cb81b8R31>`_
in our process, when all the results are received or if an error happens
(such as a timeout) it will call the `finish callback <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-7ccdace934891188d9d1055533cb81b8R40>`_ 
there we `send the results <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-7ccdace934891188d9d1055533cb81b8R41>`_ to the calling
process which `is waiting for it <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-7ccdace934891188d9d1055533cb81b8R21>`_.

The `handle_coverage implementation <https://github.com/marianoguerra/tanodb/commit/3fba49431c68f14f35d088b5e98839d81ea468ab#diff-942e4ef944df628266f096d2fbcd4348R92>`_ is
really simple, it uses the `ets:match/2 function <http://www.erlang.org/doc/man/ets.html#match-2>`_ to match against all the entries with the given bucket and returns the key
from the matched results.

You can read more about ets match specs in the `match spec chapter on the Erlang documentation <http://www.erlang.org/doc/apps/erts/match_spec.html>`_.

Relevant code from tanodb.erl:

.. code-block:: erlang

    keys(Bucket) ->
        tanodb_metrics:core_keys(),
        Timeout = 5000,
        tanodb_coverage_fsm:start({keys, Bucket}, Timeout).

Relevant code from tanodb_vnode.erl:

.. code-block:: erlang

    handle_coverage({keys, Bucket}, _KeySpaces, {_, RefId, _},
                    State=#state{table_name=TableName}) ->
        Keys0 = ets:match(TableName, {{Bucket, '$1'}, '_'}),
        Keys = lists:map(fun first/1, Keys0),
        {reply, {RefId, Keys}, State};

Test It
.......

Let's start by checking keys on an empty bucket.

.. code-block:: erlang

    (tanodb@127.0.0.1)1> tanodb:keys(<<"mybucket">>).

    {ok,[{1347321821914426127719021955160323408745312813056,
          'tanodb@127.0.0.1',[]},

         ...

         {959110449498405040071168171470060731649205731328,
          'tanodb@127.0.0.1',...},
         {411047335499316445744786359201454599278231027712,...},
         {...}|...]}

The output is quite verbose, here is redacted for clarity, but we get back:

.. code-block:: erlang

    {ok, [{Partition, Node, ListOfKeys}*64]}

That means 64 3-item tuples (one for each vnode) with the partition id, the
node where the partition is and the list of keys for that vnode, in this
case all of them are empty and in the following cases most of them will be empty
so we will filter them to clean the output.

Now let's put a value:

.. code-block:: erlang

    (tanodb@127.0.0.1)2> tanodb:put({<<"mybucket">>, <<"k1">>}, 42).

    {ok,228359630832953580969325755111919221821239459840}

And try again listing keys but this time filtering the empty results:

.. code-block:: erlang

    (tanodb@127.0.0.1)3> lists:filter(fun ({_, _, []}) -> false;
                                          (_) -> true
                                      end,
                                      element(2, tanodb:keys(<<"mybucket">>))).

    [{228359630832953580969325755111919221821239459840,
      'tanodb@127.0.0.1', [<<"k1">>]}]

We get one partition that returns the key that we just inserted, you can also
check that the partition id is the same as the result from the put call before.

Now let's insert another value:

.. code-block:: erlang

    (tanodb@127.0.0.1)4> tanodb:put({<<"mybucket">>, <<"k2">>}, 43).

    {ok,1210306043414653979137426502093171875652569137152}

And list again, now we get two partitions with keys:

.. code-block:: erlang

    (tanodb@127.0.0.1)5> lists:filter(fun ({_, _, []}) -> false;
                                          (_) -> true
                                      end,
                                      element(2, tanodb:keys(<<"mybucket">>))).

    [{1210306043414653979137426502093171875652569137152,
      'tanodb@127.0.0.1', [<<"k2">>]},
     {228359630832953580969325755111919221821239459840,
      'tanodb@127.0.0.1', [<<"k1">>]}]

Yet another value:

.. code-block:: erlang

    (tanodb@127.0.0.1)6> tanodb:put({<<"mybucket">>, <<"k3">>}, 44).

    {ok,1073290264914881830555831049026020342559825461248}

And the list again:

.. code-block:: erlang

    (tanodb@127.0.0.1)7> lists:filter(fun ({_, _, []}) -> false;
                                          (_) -> true
                                      end,
                                      element(2, tanodb:keys(<<"mybucket">>))).

    [{1210306043414653979137426502093171875652569137152,
      'tanodb@127.0.0.1', [<<"k2">>]},
     {1073290264914881830555831049026020342559825461248,
      'tanodb@127.0.0.1', [<<"k3">>]},
     {228359630832953580969325755111919221821239459840,
      'tanodb@127.0.0.1', [<<"k1">>]}]

Implementing the REST API
-------------------------

The REST API is quite straight forward, we `add a new route to cowboy <https://github.com/marianoguerra/tanodb/commit/2e5fb43e44f8240132b2f4a37d3da9c2e07caa34#diff-4477d4dd0aa2db0e274a56c9158207bdR74>`_  allowing to do `GET /store/:bucket` without specifying the key,
we will interpret this as a request to "get the bucket" which for us means to
return the keys.

Then when doing a GET and key is undefined we assume it's a request to list
the bucket's keys so we `request the keys <https://github.com/marianoguerra/tanodb/commit/2e5fb43e44f8240132b2f4a37d3da9c2e07caa34#diff-49cafd1f97d6013b2a41319db4c7961fR44>`_
and deduplicate them by `using them as keys in a map with the values set to
true <https://github.com/marianoguerra/tanodb/commit/2e5fb43e44f8240132b2f4a37d3da9c2e07caa34#diff-49cafd1f97d6013b2a41319db4c7961fR39>`_ and then `collecting the keys of the map <https://github.com/marianoguerra/tanodb/commit/2e5fb43e44f8240132b2f4a37d3da9c2e07caa34#diff-49cafd1f97d6013b2a41319db4c7961fR49>`_.

Test It
.......

Like in the previous test, let's start listing an empty bucket:

.. code-block:: sh

    $ http localhost:8080/store/mybucket

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 2
    content-type: application/json
    date: Sat, 31 Oct 2015 14:12:52 GMT
    server: Cowboy

    []

Let's put a value in that bucket:

.. code-block:: sh

    $ http post localhost:8080/store/mybucket/bob name=bob color=yellow

.. code-block:: http

    HTTP/1.1 204 No Content
    content-length: 0
    content-type: application/json
    date: Sat, 31 Oct 2015 14:12:58 GMT
    server: Cowboy

And list it again:

.. code-block:: sh

    $ http localhost:8080/store/mybucket

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 7
    content-type: application/json
    date: Sat, 31 Oct 2015 14:13:00 GMT
    server: Cowboy

    [
        "bob"
    ]

Yet another one:

.. code-block:: sh

    $ http post localhost:8080/store/mybucket/patrick name=patrick color=pink

.. code-block:: http

    HTTP/1.1 204 No Content
    content-length: 0
    content-type: application/json
    date: Sat, 31 Oct 2015 14:13:18 GMT
    server: Cowboy

List again:

.. code-block:: sh

    $ http localhost:8080/store/mybucket

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 17
    content-type: application/json
    date: Sat, 31 Oct 2015 14:13:20 GMT
    server: Cowboy

    [
        "bob",
        "patrick"
    ]

