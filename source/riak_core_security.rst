Riak Core Security
==================

riak_core_security is a module in riak_core that provides facilities to
implement user/group management, authentication and authorization.

Here we will see an overview of it.

Implementation
--------------

riak_core_security is implemented on top of riak_core_metadata, it uses the
following keys to store its information:

.. code-block:: erlang

    {<<"security">>, <<"users">>}
    {<<"security">>, <<"groups">>}
    {<<"security">>, <<"sources">>}
    {<<"security">>, <<"usergrants">>}
    {<<"security">>, <<"groupgrants">>}
    {<<"security">>, <<"status">>} -> enabled
    {<<"security">>, <<"config">>} -> ciphers

How they are stored should be an implementation detail but sometimes you may
need to fold over values to get information if it's not supported by
riak_core_security's API.

Vocabulary
----------

Context
.......

Opaque information you get back from authentication, you have to pass it back
in to other operations.

At the moment it's a record with three fields:

* username
* grants
* epoch

But notice that this is an implementation detail and you should handle it
as an opaque value.

Contexts are only valid until the GRANT epoch changes, and it will change
whenever a GRANT or a REVOKE is performed. This rule may change in the future.

Permission
..........

A string that represents some action in a given application, for example
tanodb.get, tanodb.put.

A permission muy be listed as valid in the environment variable {riak_core, permissions}::

    (tanodb@127.0.0.1)1> application:get_env(riak_core, permissions).
    {ok,[{riak_core,[get_bucket,set_bucket,get_bucket_type, set_bucket_type]}]}

You can list your permissions in config/advanced.config uncommenting the line::

   % {permissions, [{ tanodb, [put, get, list, grant, delete]}]}

And changing the permissions inside the list.

.. note::

    tanodb is the name of your app

Role
....

Something you assign permissions to, it can be a user or a group, there are
some reserved roles:

* all
* on
* to
* from
* any

Source
......

The source where the user is authenticating, it can be an ip or something else,
you can allow a user to authenticate from a source but not another.

Extra Features
--------------

* Certificate Authentication
* Plugable Authentication

API Overview
------------

check_permission
................

.. code-block:: erlang

    % Check a Global permission, one that is not tied to a bucket
    check_permission({Permission}, Context)

    % Check a permission for a specific bucket
    check_permission({Permission, Bucket}, Context)

check_permissions
.................

.. code-block:: erlang

    % Check that all permissions are valid
    check_permissions(List, Ctx)

get_username
............

.. code-block:: erlang

    % return username from context
    get_username(Context)

authenticate
............

If successful it will return {ok, Context}

A username can be tied to specific sources from which he can login, if you
don't need this feature specify a generic source for all your users.

.. code-block:: erlang

    authenticate(Username, Password, ConnInfo)

add_user
........

Valid options:

* password
* groups

.. code-block:: erlang

    add_user(Username, Options)

add_group
.........

Valid options:

* password

.. code-block:: erlang

    add_group(Groupname, Options)

alter_user
..........

Options passed will override options already in user's details, this means if
you pass a password it will be changed, if you pass groups the new groups will
be set and the old removed.

.. code-block:: erlang

    alter_user(Username, Options)

alter_group
...........

Options passed will override options already in groups's details, if you pass
groups the new groups will be set and the old removed.

.. code-block:: erlang

    alter_group(Groupname, Options)

del_user
........

Deletes user and associated grants

.. code-block:: erlang

    del_user(Username)

del_group
.........

Deletes group and associated grants

.. code-block:: erlang

    del_group(Groupname)

add_grant
.........

Add Grants to RoleList on Bucket, RoleList can be the atom **all** to asign
Grants to all roles in that Bucket.

Bucket can be a binary to assign to the whole bucket or {binary(), binary()},
to assign to a key in the bucket.

The call will merge previous grants with the new ones.

.. code-block:: erlang

    add_grant(RoleList, Bucket, Grants)

add_revoke
..........

Revoke Grants to RoleList on Bucket, RoleList can be the atom **all** to revoke
Grants to all roles in that Bucket.

.. code-block:: erlang

    add_revoke(RoleList, Bucket, Revokes)

add_source
..........

Users is a list of users or the atom **all** to apply to all users.
CIDR is a tuple with an IP address and a mask in bits.
Source is an atom:

* trust: no password required
* password: password authentication
* certificate: certificate authentication
* Atom: Atom will be used as a custom authentication module, on auth Atom will
  be looked up on the env key {riak_core, auth_mods} if found the returned
  value will be used as a module to call
  AuthMod:auth(Username, Password, UserData, SourceOptions)

Options are options for the source that will be passed during auth

.. code-block:: erlang

    add_source(Users, CIDR, Source, Options)

Example calls:

.. code-block:: erlang

    riak_core_security:add_source(all, {{127, 0, 0, 1}, 32}, trust, [])
    riak_core_security:add_source(all, {{127, 0, 0, 1}, 32}, password, [])

del_source
..........

Delete source identified by CIDR for Users, Users can be the atom **all** to
remove the source from all users. This won't apply to sources added for each
users, only if the source was added explicitly for the **all** atom.

.. code-block:: erlang

    del_source(Users, CIDR)

is_enabled
..........

Returns **true** if riak_core_security is enabled, **false** otherwise.

.. code-block:: erlang

    is_enabled()

enable
......

Enables riak_core_security

.. code-block:: erlang

    enable()

disable
.......

Disabled riak_core_security

.. code-block:: erlang

    disable()

status
......

Returns an atom representing the  status of riak_core_security:

* enabled
* enabled_but_no_capability
* disabled

.. code-block:: erlang

    status()

Playing in the REPL
-------------------

First we will need to uncomment the permissions for our app in config/advanced.config

TODO: commit hash here

Then we build again and run it:

.. code-block:: shell

    rebar3 release
    rebar3 run

First let's setup some variables

.. code-block:: erlang

    (tanodb@127.0.0.1)1> User1 = <<"sandy">>.
    <<"sandy">>

    (tanodb@127.0.0.1)2> Pass1 = <<"secret">>.
    <<"secret">>

    (tanodb@127.0.0.1)3> ConnInfo = [{ip, {127, 0, 0, 1}}].
    [{ip,{127,0,0,1}}]

    (tanodb@127.0.0.1)4> Source1 = {{127, 0, 0, 1}, 32}.
    {{127,0,0,1},32}

    (tanodb@127.0.0.1)5> Bucket1 = <<"bucket_sandy">>.
    <<"bucket_sandy">>

    (tanodb@127.0.0.1)6> PermGet = "tanodb.get".
    "tanodb.get"

    (tanodb@127.0.0.1)7> PermPut = "tanodb.put".
    "tanodb.put"

    (tanodb@127.0.0.1)8> PermList = "tanodb.list".
    "tanodb.list"

    (tanodb@127.0.0.1)9> GroupWriter = <<"writers">>.
    <<"writers">>

    (tanodb@127.0.0.1)10> GroupReader = <<"readers">>.
    <<"readers">>

We didn't add the user yet, so the following should fail

.. code-block:: erlang

    (tanodb@127.0.0.1)11> riak_core_security:authenticate(User1, Pass1, ConnInfo).
    {error,unknown_user}

Let's add the user

.. code-block:: erlang

    (tanodb@127.0.0.1)12> riak_core_security:add_user(User1, [{"password", binary_to_list(Pass1)}]).
    ok

Adding it twice should fail

.. code-block:: erlang

    (tanodb@127.0.0.1)13> riak_core_security:add_user(User1, [{"password", binary_to_list(Pass1)}]).
    {error,role_exists}

We didn't add the source for the user so the following should fail

.. code-block:: erlang

    (tanodb@127.0.0.1)14> riak_core_security:authenticate(User1, Pass1, ConnInfo).
    {error,no_matching_sources}

Add a local source that requires password for all users

.. code-block:: erlang

    (tanodb@127.0.0.1)15> riak_core_security:add_source(all, Source1, password, []).
    ok

Now it should work

.. code-block:: erlang

    (tanodb@127.0.0.1)16> {ok, Ctx1} = riak_core_security:authenticate(User1, Pass1, ConnInfo).
    {ok,{context,<<"sandy">>,[],{1444,659568,765253}}}

Checking permissions should fail, since we didn't granted any permissions yet

.. code-block:: erlang

    (tanodb@127.0.0.1)17> riak_core_security:check_permission({PermGet, Bucket1}, Ctx1).
    {false,<<"Permission denied: User 'sandy' does not have 'tanodb.get' on bucket_sandy">>,
       {context,<<"sandy">>,[],{1444,659568,765253}}}

Let's grant PermGet to User1

.. code-block:: erlang

    (tanodb@127.0.0.1)18> riak_core_security:add_grant([User1], Bucket1, [PermGet]).
    ok

And try again

.. code-block:: erlang

    (tanodb@127.0.0.1)19> riak_core_security:check_permission({PermGet, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,["tanodb.get"]}],
               {1444,659568,779759}}}

cReate some groups, each group belongs to the previous one

.. code-block:: erlang

    (tanodb@127.0.0.1)20> riak_core_security:add_group(GroupReader, []).
    ok

    (tanodb@127.0.0.1)21> riak_core_security:add_group(GroupWriter, [{"groups", [GroupReader]}]).
    ok

Let's grant permissions to each group

.. code-block:: erlang

    (tanodb@127.0.0.1)22> riak_core_security:add_grant([GroupReader], Bucket1, [PermGet]).
    ok

    (tanodb@127.0.0.1)23> riak_core_security:add_grant([GroupWriter], Bucket1, [PermPut]).
    ok

Now let's join User1 to some groups and try permissions

.. code-block:: erlang

    (tanodb@127.0.0.1)24> riak_core_security:alter_user(User1, [{"groups", [GroupReader]}]).
    ok

We can see User1 is a member of the group

.. code-block:: erlang

    (tanodb@127.0.0.1)25> riak_core_security:print_user(User1).
    ok

::

    +----------+---------------+----------------------------------------+------------------------------+

    | username |   member of   |                password                |           options            |
    +----------+---------------+----------------------------------------+------------------------------+
    |  sandy   |    readers    |9c8984b176e07eb7ba9ff1e3ada5a43ecb8a812e|              []              |
    +----------+---------------+----------------------------------------+------------------------------+

She can do PermGet on Bucket1, but she could before since she has the
permission explicitly set

.. code-block:: erlang

    (tanodb@127.0.0.1)26> riak_core_security:check_permission({PermGet, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,["tanodb.get"]}],
               {1444,659568,837358}}}

Let's revoke it

.. code-block:: erlang

    (tanodb@127.0.0.1)27> riak_core_security:add_revoke([User1], Bucket1, [PermGet]).
    ok

Still can

.. code-block:: erlang

    (tanodb@127.0.0.1)28> riak_core_security:check_permission({PermGet, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,["tanodb.get"]}],
               {1444,659568,847161}}}

But can't put on that bucket

.. code-block:: erlang

    (tanodb@127.0.0.1)29> riak_core_security:check_permission({PermPut, Bucket1}, Ctx1).
    {false,<<"Permission denied: User 'sandy' does not have 'tanodb.put' on bucket_sandy">>,
       {context,<<"sandy">>,
                [{<<"bucket_sandy">>,["tanodb.get"]}],
                {1444,659568,848204}}}

Now let's join User1 to some groups and try permissions

.. code-block:: erlang

    (tanodb@127.0.0.1)30> riak_core_security:alter_user(User1, [{"groups", [GroupWriter]}]).
    ok

We can see User1 is a member of the group, but no more of GroupReader

.. code-block:: erlang

    (tanodb@127.0.0.1)31> riak_core_security:print_user(User1).
    ok

::

    +----------+---------------+----------------------------------------+------------------------------+
    | username |   member of   |                password                |           options            |
    +----------+---------------+----------------------------------------+------------------------------+
    |  sandy   |    writers    |9c8984b176e07eb7ba9ff1e3ada5a43ecb8a812e|              []              |
    +----------+---------------+----------------------------------------+------------------------------+

User1 can now put on that bucket

.. code-block:: erlang

    (tanodb@127.0.0.1)32> riak_core_security:check_permission({PermPut, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,["tanodb.get","tanodb.put"]}],
               {1444,659568,859448}}}

Still can get since GroupWriter is member of the group GroupReader

.. code-block:: erlang

    (tanodb@127.0.0.1)33> riak_core_security:check_permission({PermGet, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,["tanodb.get","tanodb.put"]}],
               {1444,659568,860961}}}

Now let's add a new grant to GroupReader so they can list the bucket

.. code-block:: erlang

    (tanodb@127.0.0.1)34> riak_core_security:add_grant([GroupReader], Bucket1, [PermList]).
    ok

Now User1 has the list permission since she is a member of GroupWriter
which is a member of GroupReader who has permissions to list Bucket1

.. code-block:: erlang

    (tanodb@127.0.0.1)35> riak_core_security:check_permission({PermList, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,
                 ["tanodb.get","tanodb.list","tanodb.put"]}],
               {1444,659568,872565}}}

Let's remove GroupReader membership from GroupWriter

.. code-block:: erlang

    (tanodb@127.0.0.1)36> riak_core_security:alter_group(GroupWriter, [{"groups", []}]).
    ok

Now User1 can't list on Bucket1 anymore

.. code-block:: erlang

    (tanodb@127.0.0.1)37> riak_core_security:check_permission({PermList, Bucket1}, Ctx1).
    {false,<<"Permission denied: User 'sandy' does not have 'tanodb.list' on bucket_sandy">>,
       {context,<<"sandy">>,
                [{<<"bucket_sandy">>,["tanodb.put"]}],
                {1444,659568,881585}}}

Let's try one more thing, add GroupWriter to GroupReader

.. code-block:: erlang

    (tanodb@127.0.0.1)38> riak_core_security:alter_group(GroupWriter, [{"groups", [GroupReader]}]).
    ok

This works again

.. code-block:: erlang

    (tanodb@127.0.0.1)39> riak_core_security:check_permission({PermList, Bucket1}, Ctx1).
    {true,{context,<<"sandy">>,
               [{<<"bucket_sandy">>,
                 ["tanodb.get","tanodb.list","tanodb.put"]}],
               {1444,659568,890698}}}

Let's now remove GroupReader completely

.. code-block:: erlang

    (tanodb@127.0.0.1)40> riak_core_security:del_group(GroupReader).
    ok

This should fail again

.. code-block:: erlang

    (tanodb@127.0.0.1)41> riak_core_security:check_permission({PermList, Bucket1}, Ctx1).
    {false,<<"Permission denied: User 'sandy' does not have 'tanodb.list' on bucket_sandy">>,
       {context,<<"sandy">>,
                [{<<"bucket_sandy">>,["tanodb.put"]}],
                {1444,659568,914573}}}

Let's clean everything up

.. code-block:: erlang

    (tanodb@127.0.0.1)42> riak_core_security:del_group(GroupWriter).
    ok

    (tanodb@127.0.0.1)43> riak_core_security:del_user(User1).
    ok

    (tanodb@127.0.0.1)44> riak_core_security:del_source(all, Source1).
    ok

If you want to retry from scratch removing all state you can do the following::

    rm -rf _build/default/rel
    rebar3 release
    rebar3 run
