Metrics
=======

.. note::

    While the content of this book is still valid, the code may not run with
    latest versions of the tools and libraries, for an updated version of the
    code check the `Riak Core Tutorial <https://marianoguerra.github.io/riak-core-tutorial/>`_

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

.. code-block:: erlang

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

.. code-block:: erlang

    (tanodb@127.0.0.1)1> tanodb_metrics:all().
    [{tanodb,[
    
     ...

     {core,[{ping,[{count,0},{one,0}]}]}]

Then we call ping once.

.. code-block:: erlang

    (tanodb@127.0.0.1)2> tanodb:ping().
    {pong,593735040165679310520246963290989976735222595584}

And ask for the metrics again, we can see that now it registered our call.

.. code-block:: erlang

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

Test it
.......

Stop, build a release and run. In the shell run:

.. code-block:: erlang

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

Web Server Metrics (Cowboy)
---------------------------

We will start with some generic web server metrics, you can add specific ones
with what you have learned in this chapter and by reading `the exometer docs <https://github.com/Feuerlabs/exometer/tree/master/doc>`_.

For the generic metrics we will use `cowboy_exometer <https://github.com/marianoguerra/cowboy_exometer>`_ which is a module I just wrote since it was quite generic :)

We start by adding the `cowboy_exometer dependency <https://github.com/marianoguerra/tanodb/commit/8fb792bc01ac58fbdc709a0c9d2f960605255e54#diff-31d7a50c99c265ca2793c20961b60979R7>`_, this module exposes a middleware and a response hook
to register metrics on all requests, for that we need to `initialize it providing the endpoints we care about <https://github.com/marianoguerra/tanodb/commit/8fb792bc01ac58fbdc709a0c9d2f960605255e54#diff-afa3f67ec87f742d64ee9ed311455777R20>`_ and when we want to collect the metrics we `call cowboy_exometer:stats/1 passing the same endpoints we passed on init <https://github.com/marianoguerra/tanodb/commit/8fb792bc01ac58fbdc709a0c9d2f960605255e54#diff-afa3f67ec87f742d64ee9ed311455777R11>`_.

Finally we need to tell cowboy that we will `add a middleware and a response hook <https://github.com/marianoguerra/tanodb/commit/8fb792bc01ac58fbdc709a0c9d2f960605255e54#diff-4477d4dd0aa2db0e274a56c9158207bdR38>`_.

Test it
.......

After all of this, stop, build, run and make some requests:

.. code-block:: sh

    http localhost:8080/ping

.. raw:: latex

    \newpage

and then on the node shell ask for the metrics:

.. code-block:: erlang

    (tanodb@127.0.0.1)1> tanodb_metrics:all().
    [{tanodb,[

        ...

     {http,[{resp,[{by_code,[{200,[{count,1},{one,1}]},
                             {201,[{count,0},{one,0}]},
                             {202,[{count,0},{one,0}]},
                             {203,[{count,0},{one,0}]},
                             {204,[{count,0},{one,0}]},
                             {205,[{count,0},{one,0}]},
                             {206,[{count,0},{one,0}]},
                             {300,[{count,0},{one,0}]},
                             {301,[{count,0},{one,0}]},
                             {302,[{count,0},{one,0}]},
                             {303,[{count,0},{one,0}]},
                             {304,[{count,0},{one,0}]},
                             {305,[{count,0},{one,0}]},
                             {306,[{count,0},{one,...}]},
                             {307,[{count,...},{...}]},
                             {308,[{...}|...]},
                             {400,[...]},
                             {401,...},
                             {...}|...]}]},
            {req,[{time,[{<<"ping">>,
                          [{n,3},
                           {mean,44126},
                           {min,44126},
                           {max,44126},
                           {median,44126},
                           {50,0},
                           {75,44126},
                           {90,44126},
                           {95,44126},
                           {99,44126},
                           {999,44126}]}]},
                  {active,[{value,0},{ms_since_reset,11546}]},
                  {count,[{<<"ping">>,[{count,1},{one,1}]}]}]}]},
     {node,[{abs,[{process_count,428},
                  {run_queue,0},
                  {error_logger_queue_len,0},
                  {memory_total,50301760},
                  {memory_procs,30854096},
                  {memory_atoms,471201},
                  {memory_bin,222648},
                  {memory_ets,1574728}]},
            {inc,[{bytes_in,11737},
                  {bytes_out,2470},
                  {gc_count,6},
                  {gc_words_reclaimed,29747},
                  {reductions,2848780},
                  {scheduler_usage,[{1,0.05329944038387727},
                                    {2,0.8991375098414373},
                                    {3,0.03932163131802264},
                                    {4,0.05719991628720056}]}]}]},
     {core,[{ping,[{count,1},{one,1}]}]}]

You can see on this line that I made one request to ping and it returned 200:

.. code-block:: erlang

     {http,[{resp,[{by_code,[{200,[{count,1},{one,1}]},

You can also see request time stats per endpoint:

.. code-block:: erlang

            {req,[{time,[{<<"ping">>,
                          [{n,3},
                           {mean,44126},
                           {min,44126},
                           {max,44126},
                           {median,44126},
                           {50,0},
                           {75,44126},
                           {90,44126},
                           {95,44126},
                           {99,44126},
                           {999,44126}]}]},

And request count by endpoint:

.. code-block:: erlang

                  {count,[{<<"ping">>,[{count,1},{one,1}]}]}]}]},

Exposing Metrics as a REST resource
...................................

This one will be simple, first we `add the route to cowboy <https://github.com/marianoguerra/tanodb/commit/de3dde8187ceefdeb787eb835a6e36e80528de6f#diff-4477d4dd0aa2db0e274a56c9158207bdR33>`_ then `add the metrics endpoint to the list of endpoints we want to collect metrics <https://github.com/marianoguerra/tanodb/commit/de3dde8187ceefdeb787eb835a6e36e80528de6f#diff-afa3f67ec87f742d64ee9ed311455777R6>`_ (metricception) and finally `we implement the cowboy handler to return the json <https://github.com/marianoguerra/tanodb/blob/de3dde8187ceefdeb787eb835a6e36e80528de6f/apps/tanodb/src/tanodb_http_metrics.erl>`_.

Test it
:::::::

Stop, build, start and make some requests:

.. code-block:: sh

    http localhost:8080/ping

And then make a request for the metrics (result edited since it's quite big):

.. code-block:: sh

    $ http localhost:8080/metrics

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 8079
    content-type: application/json
    date: Fri, 30 Oct 2015 10:39:27 GMT
    server: Cowboy

    {
        "core": {
            "ping": { "count": 2, "one": 1 }
        },
        "http": {
            "req": {
                "active": { "ms_since_reset": 279958, "value": 1 },
                "count": {
                    "metrics": { "count": 1, "one": 0 },
                    "ping": { "count": 2, "one": 1 }
                },
                "time": {
                    "metrics": {
                        "50": 0,
                        "75": 0,
                        "90": 0,
                        "95": 0,
                        "99": 0,
                        "999": 0,
                        "max": 0,
                        "mean": 0,
                        "median": 0,
                        "min": 0,
                        "n": 0
                    },
                    "ping": {
                        "50": 0,
                        "75": 349,
                        "90": 349,
                        "95": 349,
                        "99": 349,
                        "999": 349,
                        "max": 349,
                        "mean": 349,
                        "median": 349,
                        "min": 349,
                        "n": 3
                    }
                }
            },
            "resp": {
                "by_code": {
                    "200": { "count": 3, "one": 1 },
                    "201": { "count": 0, "one": 0 },
                    ...
                    "400": { "count": 0, "one": 0 },
                    "401": { "count": 0, "one": 0 },
                    ...
                    "404": { "count": 0, "one": 0 },
                    ...
                    "500": { "count": 0, "one": 0 },
                    ...
                }
            }
        },
        "node": {
            "abs": {
                "error_logger_queue_len": 0,
                "memory_atoms": 471362,
                "memory_bin": 224392,
                "memory_ets": 1579592,
                "memory_procs": 31886248,
                "memory_total": 51342840,
                "process_count": 432,
                "run_queue": 0
            },
            "inc": {
                "bytes_in": 0,
                "bytes_out": 0,
                "gc_count": 2,
                "gc_words_reclaimed": 6624,
                "reductions": 695770,
                "scheduler_usage": {
                    "1": 0.16108125753314584,
                    "2": 0.5187896583972728,
                    "3": 0.18046079477682214,
                    "4": 0.15292436095407036
                }
            }
        },
        "tanodb": {
            ...
        }
    }

