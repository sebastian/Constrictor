%% Copyright (c) 2012, Aircloak
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, 
%% with or without modification, are permitted provided 
%% that the following conditions are met:
%% 
%% - Redistributions of source code must retain the above 
%%   copyright notice, this list of conditions and 
%%   the following disclaimer.
%%
%% - Redistributions in binary form must reproduce the above 
%%   copyright notice, this list of conditions and the 
%%   following disclaimer in the documentation and/or other 
%%   materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
%% OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% @author Sebastian Probst Eide <sebastian@aircloak.com>
%% @copyright 2012 Aircloak.com

%% @doc Behaviour that ensures that no more than a certain
%%      number of processes run concurrently.
-module(constrictor).

%% API
-export([
  start_link/1,
  stop/1,
  execute/2
]).

%% Private API
-export([receiver/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the constrictor server. It takes one
%%      parameter which is the number of available
%%      slots.
-spec start_link(pos_integer()) -> {ok, pid()}.
start_link(Slots) when Slots > 0 ->
  Pid = spawn_link(?MODULE, receiver, [Slots]),
  true = register(?MODULE, Pid),
  {ok, Pid};
start_link(Num) ->
  erlang:error({at_least_one_slot_is_required, Num}).

%% @doc Termiantes the server
stop(Pid) ->
  Pid ! stop.

%% @doc Takes a function to execute,
%%      and executes it when the sufficient resources
%%      are available.
%%      Jobs are executed in first come first
%%      serve basis.
-spec execute(pid(), fun(() -> any())) -> any().
execute(Pid, Job) ->
  Pid ! {job, self(), Job, now()},
  receive 
    {result, R} ->
      R;
    Other ->
      lager:error("Received other result: ~p", [Other])
  end.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

%% @hidden
receiver(0) ->
  receive 
    stop ->
      ok;
    slot_available ->
      receiver(1)
  end;
receiver(Slots) ->
  receive
    {job, From, Fun, CreatedTime} ->
      _Waited = timer:now_diff(now(), CreatedTime) / 1000, %% ms
      execute_fun(From, Fun),
      receiver(Slots-1);
    slot_available ->
      receiver(Slots+1);
    stop ->
      ok;
    Other ->
      lager:error("Unknown message received in priority handler: ~p", [Other]),
      receiver(Slots)
  end.

execute_fun(From, Fun) ->
  Controller = self(),
  spawn(fun() ->
    Res = try Fun()
        catch Error:Reason -> {error, {Error, Reason}}
    end,
    Controller ! slot_available,
    From ! {result, Res}
  end).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

wait_for(0) -> ok;
wait_for(N) -> receive _ -> wait_for(N-1) end.

constrictor_test_() ->
  {
    setup,
    fun() ->
      {go_strong}
    end,
    fun({go_strong}) ->

      [{"constrictor:start_link requires at least one slot",
        [
            ?_assertException(error, {at_least_one_slot_is_required, _}, start_link(0))
        ]
      },

     {"constrictor:execute should restrict the execution of events",
        fun() ->
            {ok, Pid} = start_link(1),

            Num = 4,
            WaitMs = 100,
            ExpectedMinWaitTime = WaitMs * Num * 1000,

            This = self(),
            F = fun() ->
                    spawn(fun() ->
                          execute(Pid, fun() ->
                                timer:sleep(WaitMs),
                                This ! done
                            end)
                      end)
                end,

            StartTime = now(),
            [F() || _ <- lists:seq(1,Num)],
            wait_for(Num),
            EndTime = now(),
            ActualWaitTime = timer:now_diff(EndTime, StartTime),
            ?assert(ActualWaitTime >= ExpectedMinWaitTime),
            stop(Pid)
        end
      },

     {"constrictor:execute should execute tasks in parallel if possible",
        fun() ->
            {ok, Pid} = start_link(4),
            Num = 4,
            WaitMs = 100,
            ExpectedMinWaitTime = WaitMs * 1000,
            ExpectedMaxTime = WaitMs * 1000 * 2,

            This = self(),
            F = fun() ->
                    spawn(fun() ->
                          execute(Pid, fun() ->
                                timer:sleep(WaitMs),
                                This ! done
                            end)
                      end)
                end,

            StartTime = now(),
            [F() || _ <- lists:seq(1,Num)],
            wait_for(Num),
            EndTime = now(),
            ActualWaitTime = timer:now_diff(EndTime, StartTime),
            io:format("ExpectedMinWaitTime: ~p~n", [ExpectedMinWaitTime]),
            io:format("ActualTime: ~p~n", [ActualWaitTime]),
            ?assert(ActualWaitTime >= ExpectedMinWaitTime),
            ?assert(ActualWaitTime =< ExpectedMaxTime),
            stop(Pid)
        end
      }]

    end
  }.
    
-endif.
