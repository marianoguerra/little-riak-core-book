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

Again I won't go into details on the fsm and supervisor implementations, maybe
I will add an annex later or comment the code heavily in case you want to
understand how it works, but just for you to know, I tend to copy those fsms
from other projects and adapt them to my needs, just don't tell anybody ;)

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

Implementing it
...............

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
