Metrics
=======

API Metrics
-----------

Since this is meant to be a production system we can't be far along until we
add metrics, for this we will use `exometer <https://github.com/Feuerlabs/exometer>`_ which is already a dependency of riak_core so we don't need to add it.

We start by defining a `module named tanodb_metrics <https://github.com/marianoguerra/tanodb/blob/0ea3595aefce0f9098cb651eb33263933ce9d6e7/apps/tanodb/src/tanodb_metrics.erl>`_.

The main functions we care about are :

`init/0 <https://github.com/marianoguerra/tanodb/blob/0ea3595aefce0f9098cb651eb33263933ce9d6e7/apps/tanodb/src/tanodb_metrics.erl#L16>`_
    which will initialize all the metrics when the app starts, we will add more metrics here as we add more features.

`core_ping/0 <https://github.com/marianoguerra/tanodb/blob/0ea3595aefce0f9098cb651eb33263933ce9d6e7/apps/tanodb/src/tanodb_metrics.erl#L14>`_
    should be called to register metrics about calls to `tanodb:ping/0 <https://github.com/marianoguerra/tanodb/blob/0ea3595aefce0f9098cb651eb33263933ce9d6e7/apps/tanodb/src/tanodb.erl#L15>`_

`all/0 <https://github.com/marianoguerra/tanodb/commit/0ea3595aefce0f9098cb651eb33263933ce9d6e7#diff-afa3f67ec87f742d64ee9ed311455777R8>`_
    returns the current status of all metrics.

To make the metrics actually work we need to call `tanodb_metrics:init/0 <https://github.com/marianoguerra/tanodb/commit/0ea3595aefce0f9098cb651eb33263933ce9d6e7#diff-4477d4dd0aa2db0e274a56c9158207bdR13>`_ when we start the application and `tanodb_metrics:core_ping/0 <https://github.com/marianoguerra/tanodb/commit/0ea3595aefce0f9098cb651eb33263933ce9d6e7#diff-6f7251bf9e224ebabd766f0331b848adR16>`_ each time tanodb:ping/0 is called.

Test It
.......

Stop, build a release and run the server (I won't tell you how from now on, check previous chapters to see how).

On the server shell run:

.. code-block:: erl

    (tanodb@127.0.0.1)1> tanodb_metrics:all().
    [{tanodb,[
    
     ...

     {core,[{ping,[{count,0},{one,0}]}]}]

    (tanodb@127.0.0.1)2> tanodb:ping().
    {pong,593735040165679310520246963290989976735222595584}

    (tanodb@127.0.0.1)3> tanodb_metrics:all().
    [{tanodb,[

     ...

     {core,[{ping,[{count,1},{one,1}]}]}]

    (tanodb@127.0.0.1)4>

The `...` are there to skip a lot of metrics about riak_core itself that
are quite useful but not important at this point.

Let's see the shell session step by step, first we call tanodb_metrics:all()
and get the core ping metrics, in this case count and one are 0 since we
didn't call ping yet.

.. code-block:: erl

    (tanodb@127.0.0.1)1> tanodb_metrics:all().
    [{tanodb,[
    
     ...

     {core,[{ping,[{count,0},{one,0}]}]}]

Then we call ping once.

.. code-block:: erl

    (tanodb@127.0.0.1)2> tanodb:ping().
    {pong,593735040165679310520246963290989976735222595584}

And ask for the metrics again, we can see that now it registered our call.

.. code-block:: erl

    (tanodb@127.0.0.1)3> tanodb_metrics:all().
    [{tanodb,[

     ...

     {core,[{ping,[{count,1},{one,1}]}]}]

Erlang Runtime Metrics
----------------------

Until now we have metrics for riak_core and for our API, it would be useful to
have some metrics about the Erlang Runtime, like memory, GC, processes,
schedulers etc. For that we will use a really nice library called `recon <https://github.com/ferd/recon>`_ which unified all the information gathering behind
a nice API.

We start by `adding recon as a dependency <https://github.com/marianoguerra/tanodb/commit/8d6535f360d24a1486bd7b1ed14d7fcde8c465bb#diff-31d7a50c99c265ca2793c20961b60979R6>`_,  then we `create the function tanodb_metrics:node_stats/0 <https://github.com/marianoguerra/tanodb/commit/8d6535f360d24a1486bd7b1ed14d7fcde8c465bb#diff-afa3f67ec87f742d64ee9ed311455777R24>`_ and add it to `tanodb_metrics:all/0 <https://github.com/marianoguerra/tanodb/commit/8d6535f360d24a1486bd7b1ed14d7fcde8c465bb#diff-afa3f67ec87f742d64ee9ed311455777R10>`_.

Testing it
..........

Stop, build a release and run. In the shell run:

.. code-block:: erl

    (tanodb@127.0.0.1)1> tanodb_metrics:all().
    [{tanodb,[

        ...

     {node,[{abs,[{process_count,377},
                  {run_queue,0},
                  {error_logger_queue_len,0},
                  {memory_total,30418240},
                  {memory_procs,11745496},
                  {memory_atoms,458994},
                  {memory_bin,232112},
                  {memory_ets,1470872}]},
            {inc,[{bytes_in,11737},
                  {bytes_out,2470},
                  {gc_count,7},
                  {gc_words_reclaimed,29948},
                  {reductions,2601390},
                  {scheduler_usage,[{1,0.9291112866248371},
                                    {2,0.04754016011809648},
                                    {3,0.04615958261183974},
                                    {4,0.03682005933534583}]}]}]},
     {core,[{ping,[{count,0},{one,0}]}]}]

The metrics should be self explanatory, check `the recon documentation <http://ferd.github.io/recon/>`_ for details.
