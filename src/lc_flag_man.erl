%%--------------------------------------------------------------------
%% Copyright (c) 2021-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(lc_flag_man).

-callback init(#{ callback := module()
                , _ => any()
                }) ->
  #{ callback := module()
   , flag_name := atom()
   , alarm_name := atom()
   , is_flagged := boolean()
   , current_credit => integer()
   , sample => any()
   , last_ts => integer()
   , _ =>  _
   }.

-callback check(#{ callback := module()
                 , _ => any() }) ->
  {integer(), #{ callback := module()
               , flag_name := atom()
               , alarm_name := atom()
               , alarm_info := map()
               , is_flagged := boolean()
               , current_credit => integer()
               , sample => any()
               , last_ts => integer()
               }}.

-export([ start_link/1
        , init/1
        , flag_man_loop/1
        ]).

-include_lib("snabbkaffe/include/snabbkaffe.hrl").

start_link(Args) ->
  proc_lib:start_link(?MODULE, init, Args).

init(#{callback:=Callback} = S0) ->
  process_flag(priority, max),
  process_flag(trap_exit, false),
  process_flag(message_queue_data, off_heap),
  proc_lib:init_ack({ok, self()}),
  InitState = Callback:init(S0),
  catch alarm_handler:clear_alarm(maps:get(alarm_name, InitState)),
  flag_man_loop(InitState).

flag_man_loop(#{callback := Callback} = State0) ->
  {DelayMs, NewState} = Callback:check(State0),
  handle_flag(NewState, State0),
  erlang:yield(),
  timer:sleep(DelayMs),
  ?MODULE:flag_man_loop(NewState).


-spec handle_flag(New::map(), Old::map()) -> ok | skip.
handle_flag(#{ is_flagged := true} = NewState,
            #{ is_flagged := false }) ->
  #{ flag_name := FlagName
   , alarm_name := AlarmName
   , alarm_info := AlarmInfo
   } = NewState,
  raise_flag(FlagName, AlarmName, AlarmInfo);
handle_flag(#{ is_flagged := false } = NewState,
            #{ is_flagged := true }) ->
  remove_flag(maps:get(flag_name, NewState));
handle_flag(_, _) ->
  skip.

%% Have a dummy process to register the flag name
%% so that other process could monitor it and get monitor
%% signal when the flag is removed.
-spec raise_flag(FlagName::atom(), AlarmName::atom(), AlarmData::map()) -> ok.
raise_flag(FlagName, AlarmName, AlarmData) ->
  ?tp(debug, ?MODULE, #{event => raise_flag, item => FlagName}),
  Owner = self(),
  case whereis(FlagName) of
    undefined ->
      spawn_link(fun() ->
                     register(FlagName, self()),
                     MRef = erlang:monitor(process, Owner),
                     receive
                       stop -> ok;
                       {'DOWN', MRef, process, _, _} ->
                         ok
                     end,
                     catch alarm_handler:clear_alarm(AlarmName)
                 end);
    Pid ->
      Pid
  end,
  catch alarm_handler:set_alarm({AlarmName, AlarmData#{node => node()}}),
  ok.

-spec remove_flag(FLAG::atom()) -> ok.
remove_flag(Flag) ->
  ?tp(debug, ?MODULE, #{event => remove_flag, item => Flag}),
  case whereis(Flag) of
    undefined -> ok;
    Pid when is_pid(Pid)
             -> Pid ! stop
  end,
  ok.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
