Ping as a Service (PaaS)
========================

.. note::

    This chapter and the following ones will reference a real project in a
    github repository, follow the links to see the details of what is written
    here.

Setting Up
..........

After setting up our project we will now expose ping as a REST API, for that
we will use `The Cowboy Web Server <http://ninenines.eu/docs/en/cowboy/1.0/>`_.

For JSON parsing we will use `jsx <https://github.com/talentdeficit/jsx>`_.

Here is the `commit to add the dependencies <https://github.com/marianoguerra/tanodb/commit/b86718c1b8e8689ca8adb15627f59ce44c486bfc>`_.

We also add the `rebar.lock <http://www.rebar3.org/docs/dependencies#dependency-lock-management>`_
file to make our builds reproducible.

Just in case we want to use another json library later and to simplify the calls
we `wrap the json library in our own module <https://github.com/marianoguerra/tanodb/commit/fdccd5e2863c8c71599bcd38a26e8b8b5fcd5219>`_.

Finally we create a `cowboy rest handler <http://ninenines.eu/docs/en/cowboy/1.0/manual/cowboy_rest/>`_ for our ping resource, `tanodb_http_ping.erl <https://github.com/marianoguerra/tanodb/blob/220bcade820538aec05993065ac4edf19f3ebcde/apps/tanodb/src/tanodb_http_ping.erl>`_ and `initialize cowboy in tanodb_app <https://github.com/marianoguerra/tanodb/commit/220bcade820538aec05993065ac4edf19f3ebcde>`_.

Testing it
..........

To interact with the REST API we will use httpie since it's simpler to read
(and write) than curl, check how to install it on
`the httpie website <http://httpie.org>`_

First we build it and run it as usual (this may be the last time I show you explicitly how to do it, so learn it :):

.. code-block:: sh

    rebar3 release
    rebar3 run

Now on another shell we will make an HTTP request to our ping resource:

.. code-block:: sh

    http localhost:8080/ping

And this is what I get:

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 59
    content-type: application/json
    date: Thu, 29 Oct 2015 19:07:23 GMT
    server: Cowboy

    {
    "pong": "981946412581700398168100746981252653831329677312"
    }

If you run it more times the value of the `pong` attribute should change, since
the vnode that handles the request `is defined by the time that the request is
made <https://github.com/marianoguerra/tanodb/blob/220bcade820538aec05993065ac4edf19f3ebcde/apps/tanodb/src/tanodb.erl#L16>`_.

Changing Some Configuration
...........................

Let's say we would like to run the server on another port, for that we need
to change the configuration, we can do this by editing the file::

    _build/default/rel/tanodb/etc/tanodb.conf

Search for 8080 and change it for 8081, save and close and stop the server if you are running it.

Now we will run it again but manually to avoid rebar3 from overriding our
change:

.. code-block:: sh

    ./_build/default/rel/tanodb/bin/tanodb console

And try a request to see if the port is actually changed:

.. code-block:: sh

    http localhost:8081/ping

And this is what I get:

.. code-block:: http

    HTTP/1.1 200 OK
    content-length: 60
    content-type: application/json
    date: Thu, 29 Oct 2015 19:18:03 GMT
    server: Cowboy

    {
        "pong": "1187470080331358621040493926581979953470445191168"
    }

Read tanodb.config to see all the available options, this file is generated
using `cuttlefish <https://github.com/basho/cuttlefish>`_ which takes a
`schema we define <https://github.com/marianoguerra/tanodb/blob/220bcade820538aec05993065ac4edf19f3ebcde/config/config.schema>`_ and uses it to generate
the default config file and later to validate the config file on startup
and generate configuration files that the Erlang runtime understands.

If you are curious you can see the generated config files after running the
server at least once under `_build/default/rel/tanodb/generated.configs/`
