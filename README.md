## epolymorph
Easily and clearly separate interface from implementation in Erlang

- [Theory](#theory)
  * [The problem](#the-problem)
  * [Requirements](#requirements)
  * [Approach](#approach)
- [Practice](#practice)
  * [Step 1: Declare the interface](#step-1-declare-the-interface)
  * [Step 2: Prepare your modules](#step-2-prepare-your-modules)
  * [Step 3: Add epolymorph to your project (rebar)](#step-3-add-epolymorph-to-your-project-rebar)
  * [Step 3: Add epolymorph to your project (home-made)](#step-3-add-epolymorph-to-your-project-home-made)
  * [Step 4: Use new interface in your code](#step-4--use-new-interface-in-your-code)
- [Example: connection_example](#example-connection-example)
  * [Task](#task)
  * [Quick check](#quick-check)
  * [Code](#code)

# Theory
## The problem

In OOP there is a concept of separating interface from the implementation.
To be more precise, let's define the problem in terms of Erlang.

A project has a module `clientmodule` and `servermodule`. The `clientmodule` has the following piece of code:

    func(Server) ->
      servermodule:callme(Server).

So far so good, but one day your want to use another module `servermodule2` as a server. Moreover, you want to decide, which module to use somewhere outside of the  `clientmodule` scope. And the `clientmodule` should be aware which one implementation it calls.

This is what one calls __"polymorphism"__.

Quick goooogling for _"erlang polymorphism"_ gives the impression that the question is raised from time to time, but there is no well-defined and clear solution yet.
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

The instances of Server from `servermodule` and `servermodule2` could be realized as a process, which handles _{callme, From}_ message and react accordingly.

You definitely nailed it! The client should not be changed.
But there are also **downsides**.

* The implementation of the server is now limited to a process with handles those messages. 
  So you can't write `servermodule:callme/1` which just quickly fetches something from ETS table without message passing.

* You can't benefit from the server being a `gen_server` and implement callme as a `gen_server:call/2,3`.
  Of course you could rewrite it to:

        callme(Server) -> .
            gen_server:call(Server, callme).

  But now the implementation of the server must be a `gen_server` and not a `gen_statem`.

* If one day you are to add another function `servermodule:callme2` the compiler will not help you to find all the implementation modules for adjusting to the change. The same is true if you want to change your `callme` method, for example, to accept another parameter.
  This is an important feature of strictly typed and OOP languages like C++ or Java: as you add new method to the interface or change signature of existing method, any non-conforming implementation don't compile.

__Solution 2__: Replace Server with a tuple {Module,Context}

    func(_Server = {Module,Context}) ->
      Module:callme(Context).

Much better but still smells.

_Advantage(s)_: 
1. The implementation is now not limited to a process
2. The _Behaviors_ feature of Erlang could be introduced to specify the `servermodule`'s interface, so all the implementations could be checked through the specification.

_Disadvantage(s)_: 

1. The client should be changed and re-compiled. 
   This one can be avoided by introducing the concept from the very beginning.

2. A reader of your code has no clue what kind of Module is that. If you have a dozen of  such _{Module,Context}_ "interfaces" scattered over the project, it is impossible to figure out, which one of them the _Module_ implements in any particular case. Code comments could help (if you write them everywhere and keep them up-to-date). But there is no native language tools to overcome.

The second Solution could be improved by introducing dedicated interface module with a set of delegating functions plus one factory method unifying the creating process.

__Solution 2.1__: Introduce delegating interface module.

_servermodule.erl_:

    -export([callme/1]).
  
    % servermodule could be used as a behaviour thus
    % compile-time check of an implementation could be used
    -callback callme() -> ok.
  
    % Next one unifies the process of creating instances of 'servermodule'
    create(Module, Arguments) ->
        Context = Module:create(Arguments),
        {?MODULE, Module, Context}.
  
    callme({?MODULE, Module,Context}) ->
        Module:callme(Context).

_servermodule_imp_1.erl_

    % Compile-time check if the implementation conforms interface
    -behavior(servermodule).

    -export([callme/1]).

    callme(Context) ->
        ok.

_clientmodule.erl_:

    func(Server) ->
        servermodule:callme(Server).

_somewhere.erl_:

    Server = servermodule:create(servermodule_imp_1, no_arguments),
    clientmodule:func(Server).

This one is far better from a code's reader point of view. Everything is clear now.

But there is still a subtle disadvantage exists:

* For each interface module in your project you should duplicate callbacks and delegating methods. Moreover, you should write pretty boring delegating functions with  risk of typos.

* For each interface module in your project you should write the same `create` function delegating the instance creating to the passed `Module` and then wrapping them together into `{?MODULE, Module, Instance}` tuple.

Those disadvantages definitely could be overcome with some kind of code-generating.

Having all those thoughts in mind let's try to establish our requirements of an assumed solution.
    
## Requirements

1. Unify the process of creating _Interfaces_ separated from their _Implementations_ in Erlang
2. Don't limit the way the _Implementation_ could be embodied.
3. Support any kind of compile-time check
4. Minimize duplication of code.

## Approach

1. An interface is defined as a _behavior_ with a number of _callback functions_ (as in Solution 2.1);
2. Each _Implementation_ is validated via `-behaviour(interface).` (as in Solution 2.1);
3. Having defined _callbacks_ the delegating functions are generated by [parse_transform](http://erlang.org/doc/man/erl_id_trans.html) feature of the Erlang compiler;
4. Each _Implementation_ follows special _Behaviour_ with factory method callback. This one unifies creating of instances.
5. Factory method `interface:create/2` is generated by [parse_transform](http://erlang.org/doc/man/erl_id_trans.html)

# Practice

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

## Step 1: Declare the interface

_storage.erl_:

    -compile({parse_transform, epolymorph_interface_pt}). % (1)
    
    -callback get(term(), term()) -> term(). % (2)
    -callback set(term(), term(), term()) -> ok|error.  % (3)

The `epolymorph_interface_pt` transformation `(1)` walk through the callbacks `(2), (3)` and generates exported delegating methods to `storage` in the form of:

    get({?MODULE, Module,Instance}, Key, Value) ->
        Module:get(Instance, Key, Value).

Also it generates exported function `create/2` which expects the name of the module implementing `epolymorph_instance_spec` as a first parameter and arbitrary data passed to the implementation factory method as a second parameter.

     create(Module, Arg) ->
        case Module:epolymorph_create(Arg) of
          {ok, Instance} ->
             {ok, {?MODULE, Module, Instance}};
          {error, Reason} ->
            {error, Reason}
        end.

## Step 2: Prepare your modules

Prepare your modules to be instances of abstract storage.

Each instance is to follow `epolymorph_instance_spec` behaviour which defines factory method `epolymorph_create/1` callback.

_ets_storage.erl_:

    -behaviour(epolymorph_instance_spec).
    -export([epolymorph_create/1]).
    
    -behaviour(storage).
    -export([set/3, get/2]).
  
    epolymorph_create(_) ->
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
    -export([epolymorph_create/1]).
    
    -behaviour(storage).
    -export([set/3, get/2]).
  
    epolymorph_create({RedisHost, RedisPort}) ->
        {ok, open_redis_connection(RedisHost, RedisPort)}.
                
    get(Conn, Key) -> 
        redis_command(Conn, ["GET", Key]).
        
    set(Conn, Key, Value) ->
        redis_command(Conn, ["SET", Key, Value]).

Going on!

## Step 3: Add epolymorph to your project (rebar)

1. Add polymorph as a dependency to your rebar.config:

    {deps, [
        {epolymorph, ".*", {git, "https://github.com/eldarko/epolymorph.git", {branch, "master"}}}
    ]}.
        
2. ./rebar get-deps compile

## Step 3: Add epolymorph to your project (home-made)

    # git clone https://github.com/eldarko/epolymorph.git
    # cp src/*.erl <your_project>/src

## Step 4: Use new interface in your code

    set_single_value(Storage) ->
        storage:set(Storage, "key1", "value1").

    {ok, Storage1} = storage:create(ets_storage, ignored),
    {ok, Storage2} = storage:create(redis_storage, {"127.0.0.1", 6379}),
    
    set_single_value(Storage1),
    set_single_value(Storage2).

# Example: connection_example

## Task

Define abstract connection with send/2 and close/1 methods. Add two implementations of the connection - one using UDP and one using TCP. The client shouldn't be aware which implementation is used.

## Quick check

    # git clone https://github.com/eldarko/epolymorph.git
    # cd epolymorph
    # wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
    # ./rebar3 as examples compile
    # ./rebar3 as examples shell
    1> connection_example:main().
    ok
    2>

## Code

Look at `examples/connection_example.erl` :)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>
