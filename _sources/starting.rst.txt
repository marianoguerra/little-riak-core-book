Starting
========

.. note::

    While the content of this book is still valid, the code may not run with
    latest versions of the tools and libraries, for an updated version of the
    code check the `Riak Core Tutorial <https://marianoguerra.github.io/riak-core-tutorial/>`_

We are going to build a system on top of riak_core, for that we will use some
tools and to avoid copy paste and boilerplate we will use a template to get
started.

Tools
-----

* `rebar3 <http://www.rebar3.org/docs/getting-started>`_: The build tool, click on the link to see how to install it.
* `erlang <http://www.erlang.org/>`_: Our programming language, we assume Erlang version to be at least 17.0

I also assume a unix-like environment with a shell similar to bash or zsh.

Installing the Template
-----------------------

At this point you should have `erlang <http://www.erlang.org/>`_ and `rebar3
<http://www.rebar3.org/docs/getting-started>`_ installed, now let's install the
template we are going to use.

.. code-block:: sh

    mkdir -p ~/.config/rebar3/templates
    git clone https://github.com/marianoguerra/rebar3_template_riak_core/ ~/.config/rebar3/templates/rebar3_template_riak_core

We just created the folder `~/.config/rebar3/templates` for the templates in
case it wasn't there and cloned our template inside of it.

You can read more about `rebar3 templates here <http://www.rebar3.org/docs/using-templates>`_.

Creating our Project
--------------------

Now that we have our tools and our template installed we can start by asking
rebar3 to create a new project we will call tanodb using the `riak_core <https://github.com/basho/riak_core>`_ template
we just installed:

.. code-block:: sh

    rebar3 new rebar3_riak_core name=tanodb

If it fails saying it can't find rebar3 check that it's in your `$PATH`
environment variable.

.. raw:: latex

    \newpage

The output should be something like this::

    ===> Writing tanodb/apps/tanodb/src/tanodb.app.src
    ===> Writing tanodb/apps/tanodb/src/tanodb.erl
    ===> Writing tanodb/apps/tanodb/src/tanodb_app.erl
    ===> Writing tanodb/apps/tanodb/src/tanodb_sup.erl
    ===> Writing tanodb/apps/tanodb/src/tanodb_console.erl
    ===> Writing tanodb/apps/tanodb/src/tanodb_vnode.erl
    ===> Writing tanodb/rebar.config
    ===> Writing tanodb/.editorconfig
    ===> Writing tanodb/.gitignore
    ===> Writing tanodb/README.rst
    ===> Writing tanodb/Makefile
    ===> Writing tanodb/config/nodetool
    ===> Writing tanodb/config/extended_bin
    ===> Writing tanodb/config/admin_bin
    ===> Writing tanodb/config/config.schema
    ===> Writing tanodb/config/advanced.config
    ===> Writing tanodb/config/sys.config
    ===> Writing tanodb/config/vars.config
    ===> Writing tanodb/config/vars_dev1.config
    ===> Writing tanodb/config/vars_dev2.config
    ===> Writing tanodb/config/vars_dev3.config
    ===> Writing tanodb/config/vm.args
    ===> Writing tanodb/config/dev1_vm.args
    ===> Writing tanodb/config/dev2_vm.args
    ===> Writing tanodb/config/dev3_vm.args

Building and Running
--------------------

Before explaining what the files mean so you get an idea what just happened
let's run it!


.. code-block:: sh

    cd tanodb
    rebar3 release
    rebar3 run


`rebar3 release` asks rebar3 to build a release of our project, for that it uses a tool called `relx <https://github.com/erlware/relx>`_.

The initial build may take a while since it has to fetch all the dependencies
and build them.

After the release is built (you can check the result by inspecting the folder
`_build/default/rel/tanodb/`) we can run it, for this we use a rebar3 plugin
called `rebar3_run <https://github.com/tsloughter/rebar3_run>`_

When we run `rebar3 run` we get some noisy output that should end with something like this:

.. code-block:: erl

    Eshell V7.0  (abort with ^G)
    (tanodb@127.0.0.1)1>

This is the Erlang shell, something like a REPL connected to our system,
we now can test our system by calling `tanodb:ping()` on it.

.. code-block:: erl

    (tanodb@127.0.0.1)1> tanodb:ping().
    {pong,1347321821914426127719021955160323408745312813056}

The response is the atom `pong` and a huge number that we will explain later,
but to make it short, it's the id of the process that replied to us.

.. raw:: latex

    \newpage

Exploring the Template Files
----------------------------

The template created a lot of files and you are like me, you don't like things
that make magic and don't explain what's going on, that's why we will get a
brief overview of the files created here.

First this files are created::

    apps/tanodb/src/tanodb.app.src
    apps/tanodb/src/tanodb.erl
    apps/tanodb/src/tanodb_app.erl
    apps/tanodb/src/tanodb_sup.erl
    apps/tanodb/src/tanodb_console.erl
    apps/tanodb/src/tanodb_vnode.erl

Those are the meat of this project, the source code we start with, if you
know a little of erlang you will recognice many of them, let's explain them briefly,
if you think you need more information I recommend you this awesome book which
you can read online: `Learn You Some Erlang for great good!  <http://learnyousomeerlang.com/>`_

tanodb.app.src
    This file is "The Application Resource File", you can read it, it's quite self descriptive.
    You can read more about it in the 
    `Building OTP Applications Section of Learn You Some Erlang <http://learnyousomeerlang.com/building-otp-applications>`_
    or in the `man page for app in the Erlang documentation <http://www.erlang.org/doc/man/app.html>`_.

tanodb.erl
    This file is the main API of our application, here we expose all the things
    you can ask our application to do, for now it can only handle the `ping()`
    command but we will add some more in the future.

tanodb_app.erl
    This file implements the `application behavior <http://www.erlang.org/doc/design_principles/applications.html>`_ it's a set of callbacks
    that the Erlang runtime calls to start and stop our application.

tanodb_sup.erl
    This file implements the `supervisor behavior <http://www.erlang.org/doc/design_principles/sup_princ.html>`_ it's a set of callbacks
    that the Erlang runtime calls to build the supervisor hierarchy.

tanodb_console.erl
    This file is specific to riak_core, it's a set of callbacks that will be
    called by the `tanodb-admin` command.

tanodb_vnode.erl
    This file is specific to riak_core, it implements the riak_code_vnode
    behavior, which is a set of callbacks that riak_core will call to
    accomplish different tasks, it's the main file we will edit to add new
    features.

Those were the source code files, but the template also created other files,
let's review them

rebar.config
    This is the file that rebar3 reads to get information about our project
    like dependencies and build configuration, you can read more about it
    on the `rebar3 documentation <http://www.rebar3.org/docs/basic-usage>`_

.editorconfig
    This file describes the coding style for this project, if your text editor
    understands editorconfig files then it will change it's setting for this
    project to the ones described in this file, read more about editor config
    on the `editorconfig website <http://editorconfig.org/>`_

.gitignore
    A file to tell git which files to ignore from the repository.

README.rst
    The README of the project

Makefile
    A make file with some targets that will make it easier to achieve some
    complex tasks without copying and pasting too much.

config/nodetool
    An `escript <http://www.erlang.org/doc/man/escript.html>`_ that makes it
    easier to interact with an erlang node from the command line, it will be
    used by the `tanodb` and `tanodb-admin` commands.

config/extended_bin
    A template for the `tanodb` command with some changes to support `cuttlefish <https://github.com/basho/cuttlefish>`_
    which is the library we use to load and validate our configuration

config/admin_bin
    A template for the `tanodb-admin` command.

config/config.schema
    The `cuttlefish schema <https://github.com/basho/cuttlefish/wiki>`_ file
    that describes what configuration our application supports, it starts with
    some example configuration fields that we will
    use as the application grows.

config/advanced.config
    This file is where we configure some advanced things of our application
    that don't go on our `tanodb.config` file, here we configure riak_core and
    our `logging library <https://github.com/basho/lager/>`_

config/sys.config
    This is a standard Erlang application file, you can read more about it
    in the `Erlang documentation for sys.config <http://www.erlang.org/doc/man/config.html>`_

config/vars.config
    This file contains variables used by relx to build a release, you can
    read more about it in the `rebar3 release documentation <http://www.rebar3.org/docs/releases>`_

The following files are like vars.config but with slight differences to allow
running more than one node on the same machine::

    config/vars_dev1.config
    config/vars_dev2.config
    config/vars_dev3.config

Normally when you have a cluster for your application one operating system
instance runs one instance of your application and you have many operating
system instances, but to test the clustering features of riak_core we will
build 3 releases of our application using offsets for ports and changing the
application name to avoid collisions.

config/vm.args
    A file used to pass options to the Erlang VM when starting our application.

The following files are like vars_dev*.config but for vm.args::

    config/dev1_vm.args
    config/dev2_vm.args
    config/dev3_vm.args

Those are all the files, follow the links to know more about them.

Playing with Clustering
-----------------------

Before starting to add features, let's first play with clustering so we understand
all those config files above work.

Build 3 releases that can run on the same machine::

    make devrel

This will build 3 releases of the application using different parameters (the
dev1, dev2 and dev3 files we saw earlier) and will place them under::

    _build/dev1
    _build/dev2
    _build/dev3

This is achived by using the `profiles feature from rebar3 <http://www.rebar3.org/docs/profiles>`_.

Now open 3 consoles and run the following commands one on each console::

    make dev1-console
    make dev2-console
    make dev3-console

This will start the 3 nodes but they won't know about eachother, for them
to know about eachother we need to "join" them, that is to tell one of them
about the other two, this is achieved using the tanodb-admin command, here is
how you should run it manually (don't run them)::

    _build/dev2/rel/tanodb/bin/tanodb-admin cluster join tanodb1@127.0.0.1
    _build/dev3/rel/tanodb/bin/tanodb-admin cluster join tanodb1@127.0.0.1

We tell dev2 and dev3 to join tanodb1 (dev1), to make this easier and less
error prone run the following command::

    make devrel-join

Now let's check the status of the cluster::

    make devrel-status

You can read the Makefile to get an idea of what those commands do, in this case
devrel-status does the following::

    _build/dev1/rel/tanodb/bin/tanodb-admin member-status

You should see something like this::

    ================================= Membership ===============
    Status     Ring    Pending    Node
    ------------------------------------------------------------
    joining     0.0%      --      'tanodb2@127.0.0.1'
    joining     0.0%      --      'tanodb3@127.0.0.1'
    valid     100.0%      --      'tanodb1@127.0.0.1'
    ------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:2 / Down:0

It should say that 3 nodes are joining, now check the cluster plan::

    make devrel-cluster-plan

The output should be something like this::

    =============================== Staged Changes ==============
    Action         Details(s)
    -------------------------------------------------------------
    join           'tanodb2@127.0.0.1'
    join           'tanodb3@127.0.0.1'
    -------------------------------------------------------------


    NOTE: Applying these changes will result in 1 cluster transition

    #############################################################
                             After cluster transition 1/1
    #############################################################

    ================================= Membership ================
    Status     Ring    Pending    Node
    -------------------------------------------------------------
    valid     100.0%     34.4%    'tanodb1@127.0.0.1'
    valid       0.0%     32.8%    'tanodb2@127.0.0.1'
    valid       0.0%     32.8%    'tanodb3@127.0.0.1'
    -------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

    WARNING: Not all replicas will be on distinct nodes

    Transfers resulting from cluster changes: 42
      21 transfers from 'tanodb1@127.0.0.1' to 'tanodb3@127.0.0.1'
      21 transfers from 'tanodb1@127.0.0.1' to 'tanodb2@127.0.0.1'

Now we can commit the plan::

    make devrel-cluster-commit

Which should say something like::

    Cluster changes committed

Now riak_core started an internal process to join the nodes to the cluster,
this involve some complex processes that we will explore in the following
chapters.

You should see on the consoles where the nodes are running that some logging
is happening describing the process.

Check the status of the cluster again::

    make devrel-status

You can see the vnodes transfering, this means the content of some virtual
nodes on one tanodb node are being transferred to another tanodb node::

    ================================= Membership =============
    Status     Ring    Pending    Node
    ----------------------------------------------------------
    valid      75.0%     34.4%    'tanodb1@127.0.0.1'
    valid       9.4%     32.8%    'tanodb2@127.0.0.1'
    valid       7.8%     32.8%    'tanodb3@127.0.0.1'
    ----------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

At some point you should see something like this, which means that the nodes
are joined and balanced::

    ================================= Membership ==============
    Status     Ring    Pending    Node
    -----------------------------------------------------------
    valid      34.4%      --      'tanodb1@127.0.0.1'
    valid      32.8%      --      'tanodb2@127.0.0.1'
    valid      32.8%      --      'tanodb3@127.0.0.1'
    -----------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

When you are bored you can stop them::

    make devrel-stop

Building a Production Release
-----------------------------

Even when our application doesn't have the features to merit a production
release we are going to learn how to do it here since you can later do it at
any step and get a full release of the app::

    rebar3 as prod release

In that command we as rebar3 to run the release task using the prod profile,
which has some configuration differences with the dev profiles we use so that
it builds something we can unpack and run on another operating system without
installing anything.

In my case I'm developing this on ubuntu, to show you that it works I will
copy the release to a clean ubuntu 15.04 Virtualbox and run it there::

    mkdir vm-ubuntu-1504
    cd vm-ubuntu-1504

Inside I will create a file called `Vagrantfile` with the following
content::

    Vagrant.configure(2) do |config|
      config.vm.box = "ubuntu/vivid64"
      config.vm.provider "virtualbox" do |vb|
        vb.memory = "1024"
      end
    end

And then run::

    vagrant up

To start the virtual machine.

Now let's package our release and copy it to a place where the VM can see it::

    cd _build/prod/rel
    tar -czf tanodb.tgz tanodb
    cd -
    mv _build/prod/rel/tanodb.tgz vm-ubuntu-1504

Let's ssh into the virtual machine::

    export TERM=xterm
    vagrant ssh

Inside the virtual machine run::

    cp /vagrant/tanodb.tgz .
    tar -xzf tanodb.tgz
    ./tanodb/bin/tanodb console
             
And it runs!

.. note::

    You should build the production release on the same operating system
    version you are intending to run it to avoid version problems, the
    main source of headaches are C extensions disagreeing on libc versions
    and similar.

    So, even when you could build it on a version that is close and test
    it it's better to build releases on the same version to avoid
    problems. More so if you are packaging the Erlang runtime with the
    release as we are doing here.

Wrapping Up
-----------

Now you know how to create a riak_core app from a template, how to build a
release and run it, how to build releases for a development cluster, run
the nodes, join them and inspect the cluster status and how to build a
production release and run it on a fresh server.

Quite a lot for the first chapter I would say...
