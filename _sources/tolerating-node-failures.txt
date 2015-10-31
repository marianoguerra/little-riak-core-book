Tolerating Node Failures
========================

You know computers cannot be trusted, so we may want to run our commands in
more than one vnode and wait for a subset (or all of them) to finish before
considering the operation to be successful, for this when a command is ran we
will send the command to a number of vnodes, let's call it W and wait for a
number of them to succeed, let's call it N.

To do this we will need to do something similar than what we did with coverage
calls, we will need to setup a process that will send the command to a number
of vnodes and accumulate the responses or timeout if it takes to long, then
send the result back to the caller. We will also need a supervisor for it and
to register this supervisor in our main supervisor tree.

Here is a diagram of how it works::

    +------+    +---------+    +---------+    +---------+              +------+
    |      |    |         |    |         |    |         |remaining = 0 |      |
    | Init +--->| Prepare +--->| Execute +--->| Waiting +------------->| Stop |
    |      |    |         |    |         |    |         |              |      |
    +------+    +---------+    +---------+    +-------+-+              +------+
                                                  ^   | |                    
                                                  |   | |        +---------+ 
                                                  +---+ +------->|         | 
                                                                 | Timeout | 
                                          remaining > 0  timeout |         | 
                                                                 +---------+ 

.. raw:: latex

    \newpage

Quorum Based Writes and Deletes
-------------------------------

To implement quorum based writes and deletes we will introduce two new modules,
a gen_fsm implementation called `tanodb_write_fsm <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R1>`_
and its supervisor, `tanodb_write_fsm_sup <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-6e269238efb19bfc83c7ee43415545f3R1>`_. The supervisor is a simple supervisor behavior so
I won't go into details here other than observing that we `add it to the
supervisor hierarchy <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-8ca11d5e05a10f28aec8ac9694b1c14fR31>`_ as we did with the coverage supervisor, the gen_fsm is the one that is
interesting.

On `tanodb_write_fsm:write/6 <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R41>`_ and `tanodb_write_fsm:delete/4 <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R45>`_ we start a supervisor that calls `tanodb_write_fsm:start_link <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R37>`_ which in turn calls `tanodb_write_fsm:init/1 <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R54>`_, this function initialize the state and moves the state machine to the `prepare state <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R60>`_.

The state from the fsm contains the following fields:

req_id
    Identifier for this request
from
    Process Id of the process that did the request
n
    Number of vnodes to send the request to
w
    Minimum number of responses to consider the request successful
key
    The key ({Bucket, Key}) that will be used for the operation
action
    An atom identifying the operation type, it can be `write` or `delete`
data
    If `action` is `write` then `data` is the value to write, if it's `delete`
    then it's not used
preflist
    A riak_core preflist
num_w
    Counter for current amount of responses
accum
    List of current response values, this field was introduced in the `next commit <https://github.com/marianoguerra/tanodb/commit/8e564ba444ab8b4e8205cce1ec21f9b8cf4d1c5a>`_

When we move to the `prepare` state we `build the list of nodes we are going to send the request to <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R61>`_ using
the value of `n`, we `store the list of nodes on the preflist field <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R63>`_ and `move to the execute state <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R64>`_.

On the `execute` state we `build the command we want to send <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R70>`_ depending on the value of the
`action` field and we `execute it <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R74>`_, then we `move to the waiting state <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R76>`_.

On the `waiting state <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R79>`_ when
we receive a result we `increment num_w <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-57ab62e123b72ea62fa59f06abdc9520R80>`_ and `add the new response to accum <https://github.com/marianoguerra/tanodb/commit/8e564ba444ab8b4e8205cce1ec21f9b8cf4d1c5a#diff-57ab62e123b72ea62fa59f06abdc9520R82>`_, if `num_w` is equal to `w` we `send the accumulated results to the requester <https://github.com/marianoguerra/tanodb/commit/8e564ba444ab8b4e8205cce1ec21f9b8cf4d1c5a#diff-57ab62e123b72ea62fa59f06abdc9520R85>`_ with the req_id so it can distinguis it from others `doing a selective receive <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-6f7251bf9e224ebabd766f0331b848adR51>`_.

On the `tanodb` module the changes are to call the `delete <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-6f7251bf9e224ebabd766f0331b848adR26>`_ and `write <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-6f7251bf9e224ebabd766f0331b848adR33>`_ functions in the write_fsm module and then do the `selective
receive waiting for the req_id we sent <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-6f7251bf9e224ebabd766f0331b848adR49>`_ if the response doesn't come after `Timeout` milliseconds we
return an error.

The changes on the `tanodb_vnode` module are that the `put` and `delete`
commands now receive an extra argument, `ReqId` that is `returned in the reply <https://github.com/marianoguerra/tanodb/commit/47d1e713c1a0977147d8a6f822977409063ef331#diff-942e4ef944df628266f096d2fbcd4348R44>`_.

Test it
.......

We start by listing the bucket's keys to make sure it's empty:

.. code-block:: erlang

    (tanodb@127.0.0.1)1> lists:filter(fun ({_, _, []}) -> false;
                                          (_) -> true
                                      end,
                                      element(2, tanodb:keys(<<"mybucket">>))).

    []

Then we put a value on that bucket:

.. code-block:: erlang

    (tanodb@127.0.0.1)2> tanodb:put({<<"mybucket">>, <<"k1">>}, 42).

    {ok,[{ok,274031556999544297163190906134303066185487351808},
         {ok,251195593916248939066258330623111144003363405824},
         {ok,228359630832953580969325755111919221821239459840}]}

Now we list the keys again, but this time there's something different, 3
vnodes returned that they have the key `k1`, this means that our put wrote
to 3 vnodes instead of 1 as before.

.. code-block:: erlang

    (tanodb@127.0.0.1)3> lists:filter(fun ({_, _, []}) -> false;
                                          (_) -> true
                                      end,
                                      element(2, tanodb:keys(<<"mybucket">>))).

    [{251195593916248939066258330623111144003363405824,
      'tanodb@127.0.0.1', [<<"k1">>]},
     {274031556999544297163190906134303066185487351808,
      'tanodb@127.0.0.1', [<<"k1">>]},
     {228359630832953580969325755111919221821239459840,
      'tanodb@127.0.0.1', [<<"k1">>]}]

Let's delete that key to see if it deletes in the 3 vnodes:

.. code-block:: erlang

    (tanodb@127.0.0.1)4> tanodb:delete({<<"mybucket">>, <<"k1">>}).

    {ok,[{found,274031556999544297163190906134303066185487351808,
                {{<<"mybucket">>,<<"k1">>},{{<<"mybucket">>,<<"k1">>},42}}},
         {found,228359630832953580969325755111919221821239459840,
                {{<<"mybucket">>,<<"k1">>},{{<<"mybucket">>,<<"k1">>},42}}},
         {found,251195593916248939066258330623111144003363405824,
                {{<<"mybucket">>,<<"k1">>}, {{<<"mybucket">>,<<"k1">>},42}}}]}

Listing the keys from the bucket shows that the key went away from all vnodes:

.. code-block:: erlang

    (tanodb@127.0.0.1)5> lists:filter(fun ({_, _, []}) -> false;
                                          (_) -> true
                                      end,
                                      element(2, tanodb:keys(<<"mybucket">>))).

    []

.. raw:: latex

    \newpage

Handoffs
--------

With quorum based writes we are half there, our values are written to more than
one vnode but if a node dies and another takes his work or if we add a new node
and the vnodes must be rebalanced we need to handle `handoff <https://github.com/basho/riak_core/wiki/Handoffs>`_.

The reasons to start a handoff are:

* A ring update event for a ring that all other nodes have already seen.
* A secondary vnode is idle for a period of time and the primary, original
  owner of the partition is up again.

When this happen riak_core will inform the vnode that handoff is starting,
`calling handoff_starting <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R81>`_, if it returns false it's cancelled, if it returns
true it `calls is_empty <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R103>`_, that must return false to inform that the vnode has
something to handoff (it's not empty) or true to inform that the vnode is
empty, in our case `we ask for the first element of the ets table <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R104>`_ and if it's
the special value '$end_of_table' we know it's empty, if it returns true the
handoff is considered finished, if false then a call is done to
`handle_handoff_command
<https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R68>`_
passing as first parameter an opaque structure that contains two fields we are
insterested in, foldfun and acc0, they can be unpacked with a macro like this:

.. code:: erlang

    handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->

The `FOLD_REQ` macro is defined in the `riak_core_vnode.hrl header file <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R4>`_ which we include.

This function must `iterate through all the keys it stores
<https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R71>`_
and for each of them `call foldfun
<https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R73>`_
with the key as first argument, the value as second argument and the latest
acc0 value as third.

The result of the function call is the new `Acc0` you must pass to the next
call to foldfun, the last `Acc0` must be `returned by the handle_handoff_command <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R75>`_.

For each call to Fun(Key, Entry, AccIn0) riak_core will send it to the new
vnode, to do that it must encode the data before sending, it does this by
`calling encode_handoff_item(Key, Value) <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R100>`_, where you must encode the data before sending it.

When the value is received by the new vnode it must decode it and do something
with it, this is done by the function `handle_handoff_data <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R93>`_, where we decode the received data and do the appropriate thing with it.

When we sent all the key/values `handoff_finished will be called <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R89>`_ and then `delete so we cleanup the data on the old vnode <https://github.com/marianoguerra/tanodb/commit/c9f944c5448d672f1b6923cdbd8fb06bd4862239#diff-942e4ef944df628266f096d2fbcd4348R108>`_.

You can decide to handle other commands sent to the vnode while the handoff is
running, you can choose to do one of the followings:

* Handle it in the current vnode
* Forward it to the vnode we are handing off
* Drop it

What to do depends on the design of you app, all of them have tradeoffs.

The signature of all the responses is:

.. code:: erlang

    -callback handle_handoff_command(Request::term(), Sender::sender(), ModState::term()) ->
    {reply, Reply::term(), NewModState::term()} |
    {noreply, NewModState::term()} |
    {async, Work::function(), From::sender(), NewModState::term()} |
    {forward, NewModState::term()} |
    {drop, NewModState::term()} |
    {stop, Reason::term(), NewModState::term()}.

.. raw:: latex

    \newpage

A diagram of the flow is as follows::

     +-----------+      +----------+        +----------+                
     |           | true |          | false  |          |                
     | Starting  +------> is_empty +--------> fold_req |                
     |           |      |          |        |          |                
     +-----+-----+      +----+-----+        +----+-----+                
           |                 |                   |                      
           | false           | true              | ok                   
           |                 |                   |                      
     +-----v-----+           |              +----v-----+     +--------+ 
     |           |           |              |          |     |        | 
     | Cancelled |           +--------------> finished +-----> delete | 
     |           |                          |          |     |        | 
     +-----------+                          +----------+     +--------+ 

Test it
.......

To test it we will first start a devrel node, put some values and then join
two other nodes and see on the console the handoff happening.

To make sure the nodes don't know about each other in case you played with
clustering already we will start by removing the devrel builds:

.. code-block:: sh

    rm -rf _build/dev*

And build the nodes again:

.. code-block:: sh

    make devrel

Now we will start the first node and connect to its console:

.. code-block:: sh

    make dev1-console

We generate a list of some numbers:

.. code-block:: erlang

    (tanodb1@127.0.0.1)1> Nums = lists:seq(1, 10).

    [1,2,3,4,5,6,7,8,9,10]

And with it create some bucket names:

.. code-block:: erlang

    (tanodb1@127.0.0.1)2> Buckets = lists:map(fun (N) ->
    (tanodb1@127.0.0.1)2>             list_to_binary("bucket-" ++ integer_to_list(N))
    (tanodb1@127.0.0.1)2>           end, Nums).

    [<<"bucket-1">>,<<"bucket-2">>,<<"bucket-3">>,
     <<"bucket-4">>,<<"bucket-5">>,<<"bucket-6">>,<<"bucket-7">>,
     <<"bucket-8">>,<<"bucket-9">>,<<"bucket-10">>]

And some key names:

.. code-block:: erlang

    (tanodb1@127.0.0.1)3> Keys = lists:map(fun (N) ->
    (tanodb1@127.0.0.1)3>          list_to_binary("key-" ++ integer_to_list(N))
    (tanodb1@127.0.0.1)3>        end, Nums).

    [<<"key-1">>,<<"key-2">>,<<"key-3">>,<<"key-4">>,
     <<"key-5">>,<<"key-6">>,<<"key-7">>,<<"key-8">>,<<"key-9">>,
     <<"key-10">>]

We create a function to generate a value from a bucket and a key:

.. code-block:: erlang

    (tanodb1@127.0.0.1)4> GenValue = fun (Bucket, Key) ->
    (tanodb1@127.0.0.1)4>                    [{bucket, Bucket}, {key, Key}]
    (tanodb1@127.0.0.1)4>            end.

    #Fun<erl_eval.12.54118792>

And then put some values to the buckets and keys we created:

.. code-block:: erlang

    (tanodb1@127.0.0.1)5> lists:foreach(fun (Bucket) ->
    (tanodb1@127.0.0.1)5>   lists:foreach(fun (Key) ->
    (tanodb1@127.0.0.1)5>     Val = GenValue(Bucket, Key),
    (tanodb1@127.0.0.1)5>       tanodb:put({Bucket, Key}, Val)
    (tanodb1@127.0.0.1)5>     end, Keys)
    (tanodb1@127.0.0.1)5> end, Buckets).

    ok

Now that we have some data let's start the other two nodes:

.. code-block:: sh

    make dev2-console

In yet another shell:

.. code-block:: sh

    make dev3-console

This part should remind you of the first chapter:

.. code-block:: sh

    make devrel-join

::

    Success: staged join request for 'tanodb2@127.0.0.1' to 'tanodb1@127.0.0.1'
    Success: staged join request for 'tanodb3@127.0.0.1' to 'tanodb1@127.0.0.1'

.. code-block:: sh

    make devrel-cluster-plan

::

    =============================== Staged Changes =========================
    Action         Details(s)
    ------------------------------------------------------------------------
    join           'tanodb2@127.0.0.1'
    join           'tanodb3@127.0.0.1'
    ------------------------------------------------------------------------


    NOTE: Applying these changes will result in 1 cluster transition

    ########################################################################
                             After cluster transition 1/1
    ########################################################################

    ================================= Membership ===========================
    Status     Ring    Pending    Node
    ------------------------------------------------------------------------
    valid     100.0%     34.4%    'tanodb1@127.0.0.1'
    valid       0.0%     32.8%    'tanodb2@127.0.0.1'
    valid       0.0%     32.8%    'tanodb3@127.0.0.1'
    ------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

    WARNING: Not all replicas will be on distinct nodes

    Transfers resulting from cluster changes: 42
      21 transfers from 'tanodb1@127.0.0.1' to 'tanodb3@127.0.0.1'
      21 transfers from 'tanodb1@127.0.0.1' to 'tanodb2@127.0.0.1'

.. code-block:: sh

    make devrel-cluster-commit

::

    Cluster changes committed

On the consoles from the nodes you should see some logs like the following, I
will just paste some as example.

On the sending side::

    00:17:24.240 [info] Starting ownership transfer of tanodb_vnode from
    'tanodb1@127.0.0.1' 1118962191081472546749696200048404186924073353216 to
    'tanodb2@127.0.0.1' 1118962191081472546749696200048404186924073353216

    00:17:24.240 [info] fold req 1118962191081472546749696200048404186924073353216
    00:17:24.240 [info] fold fun {<<"bucket-1">>,<<"key-1">>}:
        [{bucket,<<"bucket-1">>},{key,<<"key-1">>}]

    ...

    00:17:24.241 [info] fold fun {<<"bucket-7">>,<<"key-8">>}:
        [{bucket,<<"bucket-7">>},{key,<<"key-8">>}]

    00:17:24.281 [info] ownership transfer of tanodb_vnode from
    'tanodb1@127.0.0.1' 1118962191081472546749696200048404186924073353216 to
    'tanodb2@127.0.0.1' 1118962191081472546749696200048404186924073353216
        completed: sent 575.00 B bytes in 7 of 7 objects in 0.04 seconds
        (13.67 KB/second)

    00:17:24.280 [info] handoff finished
        1141798154164767904846628775559596109106197299200:
        {1141798154164767904846628775559596109106197299200,
            'tanodb3@127.0.0.1'}

    00:17:24.285 [info] delete
        1141798154164767904846628775559596109106197299200

On the receiving side::

    00:13:59.641 [info] handoff starting
        1050454301831586472458898473514828420377701515264:
        {hinted,{1050454301831586472458898473514828420377701515264,
            'tanodb1@127.0.0.1'}}

    00:13:59.641 [info] is_empty
        182687704666362864775460604089535377456991567872: true

    00:14:34.259 [info] Receiving handoff data for partition
        tanodb_vnode:68507889249886074290797726533575766546371837952 from
        {"127.0.0.1",47440}

    00:14:34.296 [info] handoff data received
        {{<<"bucket-8">>,<<"key-1">>},
            [{bucket,<<"bucket-8">>},{key,<<"key-1">>}]}

    ...

    00:14:34.297 [info] handoff data received
        {{<<"bucket-3">>,<<"key-7">>},
            [{bucket,<<"bucket-3">>},{key,<<"key-7">>}]}

    00:14:34.298 [info] Handoff receiver for partition
        68507889249886074290797726533575766546371837952 exited after
        processing 5 objects from {"127.0.0.1",47440}


