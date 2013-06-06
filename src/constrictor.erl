% License: Apache License, Version 2.0
%
% Copyright 2013 Aircloak
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

%% @author Sebastian Probst Eide <sebastian@aircloak.com>
%% @copyright Copyright 2013 Aircloak
%%
%% @doc Mini server that ensures that no more than N processes
%%      concurrently access a resource.
%% @end

-module(constrictor).

%% API
-export([
  start_link/2,
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
-spec start_link(atom(), pos_integer()) -> {ok, pid()}.
start_link(Name, Slots) when Slots > 0 ->
  Pid = spawn_link(?MODULE, receiver, [Slots]),
  true = register(Name, Pid),
  {ok, Pid};
start_link(_Name, Num) ->
  erlang:error({at_least_one_slot_is_required, Num}).

%% @doc Termiantes the server
stop(Pid) when is_pid(Pid) ->
  erlang:error(please_stop_using_registered_name);
stop(Name) when is_atom(Name) ->
  Pid = whereis(Name),
  unregister(Name),
  Pid ! stop,
  ok.

%% @doc Takes a function to execute,
%%      and executes it when sufficient resources
%%      are available.
%%      Jobs are executed in first come first
%%      serve basis.
-spec execute(pid(), fun(() -> any())) -> any().
execute(Name, Job) when is_atom(Name) ->
  execute(whereis(Name), Job);
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
            ?_assertException(error, {at_least_one_slot_is_required, _}, start_link(test_server, 0))
        ]
      },

      {"constrictor:stop requires registered name",
        fun() ->
          {ok, Pid} = start_link(test_server, 1),
          ?assertException(error, please_stop_using_registered_name, stop(Pid)),
          ?assertEqual(ok, stop(test_server))
        end
      },

     {"constrictor:execute should restrict the execution of events",
        fun() ->
            {ok, _Pid} = start_link(test_server, 1),

            Num = 4,
            WaitMs = 100,
            ExpectedMinWaitTime = WaitMs * Num * 1000,

            This = self(),
            F = fun() ->
                    spawn(fun() ->
                          execute(test_server, fun() ->
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
            stop(test_server)
        end
      },

     {"constrictor:execute should execute tasks in parallel if possible",
        fun() ->
            {ok, Pid} = start_link(test_server, 4),
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
            stop(test_server)
        end
      }]

    end
  }.
    
-endif.
