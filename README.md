Constrictor
-------

# Build status

[![Build Status](https://travis-ci.org/Aircloak/constrictor.png)](https://travis-ci.org/Aircloak/constrictor)

# Why?

Erlang is fantastic in that it allows you to spawn (almost) arbitrary number
of threads. But sometimes, what you deal with is a set of restricted resource.
For example a set of file handles.

If you want to strictly limit the number of threads that can access a resource
concurrently, then the constrictor comes can help you. You give the constrictor
a function to execute, and it will execute it when the resource becomes
available. Basically you can think of it as a work queue that you can give
a function, and where the number of concurrent workers are predefined and
limited.

Usage:

    1> {ok, Pid} = constrictor:start_link(2).
    {ok,<0.33.0>}

    2> constrictor:execute(Pid, fun() -> hello_constricted_resource end).
    %% ... when other pending tasks have executed...
    hello_constricted_resource

    3> constrictor:stop(Pid).
    stop

# License

AppenDb is licensed under the BSD 2-Clause License.

    Copyright (c) 2012, Aircloak
    All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
