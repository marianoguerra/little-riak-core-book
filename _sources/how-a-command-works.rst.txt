How a Command Works
===================

Enough with the setup, let's see how ping works under the covers.

Its entry point and public API is the tanodb module, that means we have to
look into tanodb.erl:

.. code-block:: erlang

    -module(tanodb).
    -include_lib("riak_core/include/riak_core_vnode.hrl").

    -export([ping/0]).

    -ignore_xref([ping/0]).

    %% Public API

    %% @doc Pings a random vnode to make sure communication is functional
    ping() ->
        tanodb_metrics:core_ping(),
        DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, tanodb),
        [{IndexNode, _Type}] = PrefList,
        riak_core_vnode_master:sync_spawn_command(IndexNode, ping, tanodb_vnode_master).

We see we have our ping function there as the only public API and it does some
funny stuff.

I won't go into much riak_core details that are described elsewhere since here
we cover the practical aspects, there are many useful talks about riak_core
internals and theory around, you can watch them:

* `Rusty Klophaus - Masterless Distributed Computing with Riak Core <https://vimeo.com/18758206>`_
* `Andy Gross - Riak Core - An Erlang Distributed Systems Toolkit <https://vimeo.com/21772889>`_

There are also some detailed articles about it:

* `Ryan Zezeski's "working" blog <https://github.com/rzezeski/try-try-try>`_
* `Riak Core Wiki <https://github.com/basho/riak_core/wiki>`_
* `Where To Start With Riak Core <http://basho.com/posts/technical/where-to-start-with-riak-core/>`_

But let's look at what it does line by line:

.. code-block:: erlang

        tanodb_metrics:core_ping(),

First we register the operation in our metrics, we covered this in previous chapters.

.. code-block:: erlang

        DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),

The line above hashes a key to decide to which vnode the call should go, a
riak_core app has a fixed number of vnodes that are distributed across all the
instances of your app's physical nodes, vnodes move from instance to instance
when the number of instances change to balance the load and provide fault
tolerance and scalability.

The call above will allow us to ask for vnodes that can handle that hashed key,
let's run it in the app console to see what it does:

.. code-block:: erlang

    (tanodb@127.0.0.1)1> DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}).

    <<126,9,218,77,97,108,38,92,0,155,160,26,161,3,200,87,134,213,167,168>>

We seem to get a binary back, in the next line we ask for a list of vnodes that
can handle that hashed key:

.. code-block:: erlang

        PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, tanodb),

let's run it to see what it does:

.. code-block:: erlang

    (tanodb@127.0.0.1)2> PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, tanodb).

    [{{730750818665451459101842416358141509827966271488, 'tanodb@127.0.0.1'},
         primary}]

We get a list with one tuple that has 3 items, a long number, something that looks like a host
and an atom, let's try changing the number 1:

.. code-block:: erlang

    (tanodb@127.0.0.1)3> PrefList2 = riak_core_apl:get_primary_apl(DocIdx, 2, tanodb).

    [{{730750818665451459101842416358141509827966271488,
       'tanodb@127.0.0.1'}, primary},
     {{753586781748746817198774991869333432010090217472,
       'tanodb@127.0.0.1'}, primary}]

Now we get two tuples, the first one is the same, so what this does is to return
the number of vnodes that can handle the request from the hashed key by priority.

Btw, the first number is the vnode id, it's what we get on the ping response :)

Next line just unpacks the pref list to get the vnode id and ignore the other part:

.. code-block:: erlang

        [{IndexNode, _Type}] = PrefList,

And finally we ask riak_core to call the ping command on the IndexNode we got back:

.. code-block:: erlang

        riak_core_vnode_master:sync_spawn_command(IndexNode, ping, tanodb_vnode_master).

Let's try it on the console:

.. code-block:: erlang

    (tanodb@127.0.0.1)5> [{IndexNode, _Type}] = PrefList.

    [{{730750818665451459101842416358141509827966271488,
       'tanodb@127.0.0.1'}, primary}]

    (tanodb@127.0.0.1)6> riak_core_vnode_master:sync_spawn_command(IndexNode, ping, tanodb_vnode_master).

    {pong,730750818665451459101842416358141509827966271488}

You can see we get IndexNode back in the pong response, now let's try passing the second IndexNode:

.. code-block:: erlang

    (tanodb@127.0.0.1)7> [{IndexNode1, _Type1}, {IndexNode2, _Type2}] = PrefList2.

    [{{730750818665451459101842416358141509827966271488,
       'tanodb@127.0.0.1'}, primary},
     {{753586781748746817198774991869333432010090217472,
       'tanodb@127.0.0.1'}, primary}]


    (tanodb@127.0.0.1)9> riak_core_vnode_master:sync_spawn_command(IndexNode2, ping, tanodb_vnode_master).

    {pong,753586781748746817198774991869333432010090217472}


We get the IndexNode2 back, that means that the request was sent to the second
vnode instead of the first one.

But where does the command go? the road is explained in this scientific chart::

    tano.erl -> riak_core magic -> tano_vnode.erl

let's see the content of tanodb_vnode.erl (just the useful parts):

.. code-block:: erlang

    -module(tanodb_vnode).
    -behaviour(riak_core_vnode).

    -export([start_vnode/1,
             init/1,
             terminate/2,
             handle_command/3,
             is_empty/1,
             delete/1,
             handle_handoff_command/3,
             handoff_starting/2,
             handoff_cancelled/1,
             handoff_finished/2,
             handle_handoff_data/2,
             encode_handoff_item/2,
             handle_coverage/4,
             handle_exit/3]).

    -record(state, {partition}).

    %% API
    start_vnode(I) ->
        riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

    init([Partition]) ->
        {ok, #state { partition=Partition }}.

    %% Sample command: respond to a ping
    handle_command(ping, _Sender, State) ->
        {reply, {pong, State#state.partition}, State};
    handle_command(Message, _Sender, State) ->
        lager:warning("unhandled_command ~p", [Message]),
        {noreply, State}.

OK, let's go by parts, first we declare our module:

.. code-block:: erlang

    -module(tanodb_vnode).

Then we specify that we want to implement the riak_core_vnode behavior:

.. code-block:: erlang

    -behaviour(riak_core_vnode).

Behaviors in Erlang are like interfaces, a set of functions that a module must
implement to satisfy the behaviour specification, you can read more in the
`Erlang documentation <http://www.erlang.org/doc/design_principles/des_princ.html>`_.

In this case riak_core defines a behavior with a set of functions we must
implement to be a valid riak_core vnode, you can get an idea of the kind of
functionality we need by looking at the exported functions:

.. raw:: latex

    \newpage

.. code-block:: erlang

    -export([start_vnode/1,
             init/1,
             terminate/2,
             handle_command/3,
             is_empty/1,
             delete/1,
             handle_handoff_command/3,
             handoff_starting/2,
             handoff_cancelled/1,
             handoff_finished/2,
             handle_handoff_data/2,
             encode_handoff_item/2,
             handle_coverage/4,
             handle_exit/3]).

For the moment most of them have a "dummy" implementation where they just to
the minimal amount of work to satisfy the behavior and not more, it's our job
to change the default implementation to fit our needs.

We will have a record called state to keep info between callbacks, this is
typical Erlang way of managing state so I won't cover it here:

.. code-block:: erlang

    -record(state, {partition}).

Then we implement the api to start the vnode, nothing fancy:

.. code-block:: erlang

    %% API
    start_vnode(I) ->
        riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

Note that on init we store the Partition value on state so we can use it later,
this is what I referred above as vnode id, it's the big number you saw before:

.. code-block:: erlang

    init([Partition]) ->
        {ok, #state { partition=Partition }}.

And now for the interesting part, here we have our ping command implementation,
we match for ping in the Message position (the first argument):

.. code-block:: erlang

    handle_command(ping, _Sender, State) ->

And return a reply response with the second item in the tuple being the actual
response that the caller will get where we reply with the atom pong and the
partition number of this vnode, the last item in the tuple is the new state we
want to have for this vnode, since we didn't change anything we pass the
current value:

.. code-block:: erlang

        {reply, {pong, State#state.partition}, State};

And then we implement a catch all that will just log the unknown command and
give no reply back:

.. code-block:: erlang

    handle_command(Message, _Sender, State) ->
        lager:warning("unhandled_command ~p", [Message]),
        {noreply, State}.

So, this is the roundtrip of the ping call, our task to add more commands will
be:

* Add a function on tanodb.erl that hides the internal work done to distribute the work
* Add a new match on handle_command to match the command we added on tanodb.erl and provide a reply
