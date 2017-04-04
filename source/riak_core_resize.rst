Ring Resize Operations
======================

.. hint::

   Additional information can be found in the `official documentation <https://github.com/basho/riak_core/blob/develop/docs/ring-resizing.md>`_.

Riak core has a method for resizing the ring. This allows a installation that was started with a certain `ring_size` (say 64) to either grow or shrink.

As a word of warning ahead of time, this operation is risky, it requires large part of the data to be re-written and deleted so it is a risky operation.

The basic procedure is performed in a few steps, some of which are fully handled by `riak_core` others need to be implemented.

Preparation
```````````
The first part is handled by `riak_core` itself, when a resize request is given `riak_core` will compute a second ring, reflecting the new size and work out what vnode goes to which node.

Handoffs
````````

Next up are the handoffs, `riak_core` will use handoffs to move data from the old vnode to the new one. The handoffs will potentially be performed **multiple times** using filters to shard the data over new vnodes.

For this to work the ``handle_command`` function needs to implement a case that accepts a ``?FOLD_REQ``. This case needs to fold over **all** bucket and key combinations!

The fold function that is passed in needs to be called with 3 arguments:

1) The bucket and key, the formate here is important! ``{Bucket, Key}``
2) The object.
3) The accumulator.


Additional functions
````````````````````

There need to be some additional functions implemented:

``nval_map(Ring)`` This function should return a list of buckets and their nval if it differs from the standard. The following is a sample implementation taken from `riak_kv`. It works well if either there are no bucket specific n-values or the ``riak_core_bucket`` module is used to manage them.

.. code:: erlang

   nval_map(Ring) ->
      riak_core_bucket:bucket_nval_map(Ring).


``object_info({Bucket, Key}`` This function computes the hash for a bucket / key combination to determine it's new placing. The following is sample implementation, taken from `riak_kv`. It works for most cases where no custom hashing or bucket naming is used.

.. code:: erlang

   object_info({Bucket, Key}=BKey) ->
       Hash = riak_core_util:chash_key(BKey),
       {Bucket, Hash}.

``request_hash(Request)`` This function is called for every incoming request for a vnode that is handing off data as part of a resize operation. For operations that should be forwarded it should return the `hash` as returned by ``riak_core_util:chash_key`` and ``undefined`` for the rest. The goal is to determine what vnode it should be forwarded to (if any). This forwarding happens **in addition** to executing the query locally! The normal approach is to only forward modifying operations such as writes or deletes and to return ``undefined`` for all others.
