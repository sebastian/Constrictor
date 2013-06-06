Constrictor
-------

# Stability?

This is untested alpha software at best.
We are not ourselves relying on this in production.
User discretion advised.
> # Build status

> [![Build Status](https://travis-ci.org/Aircloak/constrictor.png)](https://travis-ci.org/Aircloak/constrictor)

# Why?

Erlang is fantastic in that it allows you to spawn (almost) arbitrary number
of processes. But sometimes, what you want to deal with is a resource that
can only handle a certain amount of concurrency. For example a file handle.

You give the constrictor a function to execute, and it will execute it when 
the resource becomes available. Basically you can think of it as a work queue 
that you can give a function, and where the number of concurrent workers are 
predefined and limited.

Usage:

    1> {ok, Pid} = constrictor:start_link(hello_resource, 2).
    {ok,<0.33.0>}

    2> constrictor:execute(hello_resource, fun() -> hello_constricted_resource end).
    %% ... when other pending tasks have executed...
    hello_constricted_resource

    3> constrictor:stop(hello_resource).
    ok

# License

[Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)
