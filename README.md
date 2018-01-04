# epolymorph
Easily and clearly separate interface from implementation in Erlang

# Theory
## The problem

There is a concept of separating interface from the implementation in OOP. To be more specific, let's define the problem in terms of Erlang. A project has a module `clientmodule` and `servermodule`. The `clientmodule` has the following piece of code:

    func(Server) ->
      servermodule:callme(Server).

So far so good, but one day your want to use as a server another module `servermodule2`. Moreover, you want to decide, which module to use somewhere outside of module `clientmodule`. And the `clientmodule` should sometimes call `servermodule:callme/1` and sometimes `servermodule2:callme/1`.

This is what one calls __"polymorphism"__.

Quick goooogling for _"erlang polymorphism"_ gives the impression that the question is raised sometimes, but there is no well-defined and clear solution.
- [erlang polymorphism: multiple implementations of the same contract](https://stackoverflow.com/questions/22313076/erlang-polymorphism-multiple-implementations-of-the-same-contract)
- [Polymorphism in Erlang](http://weblog.plexobject.com/?p=1572)

From the links above one could quickly figure out two possible solutions:

__Solution 1__: `servermodule:callme(Server)` should send a message to Erlang's process Server.

_servermodule.erl_:

    callme(Server) ->
      Server ! {callme, self()},
      receive
        {response, Response} ->
          Response
      end.

_Advantage(s)_: 
* The client should NOT be changed in any way

_Disadvantages_:

* The implementation of the server is now limited to a process with handles those messages. 
  So you can't implement `servermodule:callme/1` which just quickly fetches something from ETS table without message passing.

* You can't benefit from the server being a `gen_server` and implement callme as a `gen_server:call/2,3`.
  Of course you could rewrite it to:

        callme(Server) -> .
            gen_server:call(Server, callme).

  But now the implementation of the server must be a `gen_server` and not a `gen_statem`.

* If one day you are to add another function `servermodule:callme2` the compiler will not help you to find all the implementation modules.
  This is very important feature of strictly typed and OOP languages like C++ or Java: if you add new method to the interface, any incomplete implementation will not compile.

__Solution 2__: Replace Server with a tuple {Module,Context}

    func(Server = {Module,Context}) ->
      Module:callme(Context).

_Advantage(s)_: 
1. The implementation is now not limited to a process
2. The _Behaviors_ feature of Erlang could be introduced to specify the `servermodule`'s interface, so all the implementations could be checked through the specification.

_Disadvantage(s)_: 

1. The client should be changed and re-compiled. 
   This one can be easily avoided by introducing the tuple-way approach from the very beginning of the project.

2. A reader of your code HAS NO CLUE what kind of Module is that. If you have several such "interfaces" in your project and operate on _{Module,Context}_ pairs, that would be very hard to figure out looking through the code, which "interface" the _Module_ implements in any particular case. Code comments could help (if you write them everywhere and keep them up-to-date).

The second Solution could be improved by introducing dedicated interface module.

__Solution 2.1__: Introduce delegating interface module.

_servermodule.erl_:

    -export([callme/1]).
    -callback callme() -> ok.
    callme({Module,Context}) ->
        Module:callme(Context).

_servermodule_imp_1.erl_

    -behavior(servermodule).
    -export([callme/1]).
    callme(Context) ->
        ok.

_clientmodule.erl_:

    func(Server) ->
        servermodule:callme(Server).

_somewhere.erl_:

    {ok, Context} = servermodule_imp_1:create(),
    clientmodule:func({servermodule_imp_1,Context}).
    
This one is far better, but still has subtle disadvantages:

* For each method in your interface module `servermodule` you should duplicate callbacks and delegating methods;
* The way of instantiating of the implementations in yet not well defined so it is easy to make an error for a new comer;
* The instantiating code in _somewhere.erl_ should be aware of the way the interface will later invoke the methods.

Having all those thoughts in mind let's try to formalize the requirements for the library.
    
## Requirements

1. Unify the process of creating _Interfaces_ separated from their _Implementations_ in Erlang
2. Don't limit the way the _Implementation_ could be impelemented
3. Employ _Behaviours_ for compile-time check

## Approach

1. Define interface as a _behavior_ with a number of _callback functions_ (as in Solution 2.1);
2. Validate an _Implementation_ against the interface via `-behaviour(interface).` directive (as in Solution 2.1);
3. Use `parse_transform` feature of Erlang's compiler for generating delegating functions (as in Solution 2.1).
   Obviously such functions could be easily generated from _callback functions_.
4. In order to unify the creating of instances each implementation must follow special convention: it must implement special _Behaviour_ with factory method.
5. Use `parse_transform` to generate abstract factory method `interface::create(Module, Args)` which creates instance of  particular implementation and wraps it in _{Module, Instance}_ pair. As a result the knowledge about the way the callable object is represented is localized well.

# Practice (step-by-step)

Suppose you have two implementations of key-value storage.
The storage's interface has two operations: `get(Storage, Key)` and `set(Storage, Key, Value)`.

You want to use the storage abstractly without knowing the exact implementation module.

_ets_storage.erl_:

    -export([create/0, set/3, get/2]).
    
    create() ->
        {ok, ets:new(?MODULE, [public])}.
    
    get(Tab, Key) ->
        case ets:lookup(Tab, Key) of
            [{_,Value}] ->
                Value;
            [] ->
                undefined
        end.

    set(Tab, Key, Value) ->
        ets:insert(Tab, {Key, Value}).

_redis_storage.erl_:

    -export([create/1, set/3, get/2]).
    
    create({RedisHost, RedisPort}) ->
        {ok, open_redis_connection(RedisHost, RedisPort)}.
        
    get(Conn, Key) -> 
        redis_command(Conn, ["GET", Key]).
        
    set(Conn, Key, Value) ->
        redis_command(Conn, ["SET", Key, Value]).

## Step 1: Prepare your modules to be an instance

Each instance should implement behaviour `epolymorph_instance_spec` which defines factory method `epolymorph_create/1`.

_ets_storage.erl_:

    -behaviour(epolymorph_instance_spec).
    -export([create/0, set/3, get/2, epolymorph_create/1]).
    
    epolymorph_create(_) ->
        create().
        
    create() ->
        {ok, ets:new(?MODULE, [public])}.
    
    get(Tab, Key) ->
        case ets:lookup(Tab, Key) of
            [{_,Value}] ->
                Value;
            [] ->
                undefined
        end.

    set(Tab, Key, Value) ->
        ets:insert(Tab, {Key, Value}).

_redis_storage.erl_:

    -behaviour(epolymorph_instance_spec).
    -export([create/1, set/3, get/2, epolymorph_create/1]).

    epolymorph_create({RedisHost, RedisPort}) ->
        create({RedisHost, RedisPort}).
        
    create({RedisHost, RedisPort}) ->
        {ok, open_redis_connection(RedisHost, RedisPort)}.
        
    get(Conn, Key) -> 
        redis_command(Conn, ["GET", Key]).
        
    set(Conn, Key, Value) ->
        redis_command(Conn, ["SET", Key, Value]).

## Step 2: Declare the interface

_storage.erl_:

    -compile({parse_transform, epolymorph_interface_pt}).
    -callback get(term(), term()) -> term().
    -callback set(term(), term(), term()) -> ok|error.

The `epolymorph_interface_pt` transformation walk through callbacks and generates exported delegating methods to `storage` in the form of:

    get({Module,Instance}, Key, Value) ->
        Module:get(Instance, Key, Value).

Also it generates exported function `create/2` which:
* expects the name of the module implementing `epolymorph_instance_spec` as a first parameter and
* arbitrary data as a second parameter:


    create(Module, Arg) ->
        % That is why we need epolymorph_create in each implementation
        case Module:epolymorph_create(Arg) of
            {ok, Instance} ->
                {ok, {Module, Instance}};
            {error, Reason} ->
                {error, Reason}
        end.

## Step 3: Build your project with epolymorph (rebar)

1. Add polymorph as a dependency to your rebar.config:

        {deps, [
            {epolymorph, ".*", {git, "https://github.com/eldarko/epolymorph.git", {branch, "master"}}}
        ]}.
        
2. ./rebar get-deps compile

## Step 3: Build your project with epolymorph (custom)

Just take the following files to your project:

* epolymorph.erl
* epolymorph_instance_spec.erl
* epolymorph_interface_pt.erl

## Step 4: Use your new storage interface

    set_single_value(Storage) ->
        storage:set(Storage, "key1", "value1").

    {ok, Storage1} = storage:create(ets_storage, ignored),
    {ok, Storage2} = storage:create(redis_storage, {"127.0.0.1", 6379}),
    
    set_single_value(Storage1),
    set_single_value(Storage2).

# Example: connection_example

## Task

Define abstract connection with send/2 and close/1 methods. Add two implementations of the connection - one using UDP and one using TCP. The client shouldn't be aware which implementation is used under the hood.

## Quick check

    # wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
    # ./rebar3 as examples compile
    # ./rebar3 as examples shell
    1> connection_example:main().
    ok
    2>

