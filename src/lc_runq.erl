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

-module(lc_runq).
-behavior(lc_flag_man).

-include("lc.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

-import(lc_lib, [ configs/0
                , config_get/2
                , config_get/3]).

-export([init/1,
         check/1]).

init(S0)->
  Credit = lc_lib:config_get(?RUNQ_MON_C1, ?RUNQ_MON_C1_DEFAULT),
  S0#{ current_credit => Credit
     , sample => scheduler:sample()
     , last_ts => ts()
     , is_flagged => false
     , flag_name => ?RUNQ_MON_FLAG_NAME
     , alarm_name => ?LC_ALARM_ID_RUNQ
     }.

check(#{ current_credit := CurrentCredit
       , sample := LastSample
       , last_ts := TS
       , is_flagged := IsFlag
       , callback :=  _
       } = State) ->
  RunQLen = erlang:statistics(total_run_queue_lengths),
  ScheduleCount = erlang:system_info(schedulers_online),
  Conf = configs(),
  F0 = config_get(?RUNQ_MON_F0, Conf, ?RUNQ_MON_F0_DEFAULT),
  F1 = config_get(?RUNQ_MON_F1, Conf, ?RUNQ_MON_F1_DEFAULT),
  F2 = config_get(?RUNQ_MON_F2, Conf, ?RUNQ_MON_F2_DEFAULT),
  F3 = config_get(?RUNQ_MON_F3, Conf, ?RUNQ_MON_F3_DEFAULT),
  F4 = config_get(?RUNQ_MON_F4, Conf, ?RUNQ_MON_F4_DEFAULT),
  F5 = config_get(?RUNQ_MON_F5, Conf, ?RUNQ_MON_F5_DEFAULT),
  T1 = config_get(?RUNQ_MON_T1, Conf, ?RUNQ_MON_T1_DEFAULT),
  T2 = config_get(?RUNQ_MON_T2, Conf, ?RUNQ_MON_T2_DEFAULT),
  C1 = config_get(?RUNQ_MON_C1, Conf, ?RUNQ_MON_C1_DEFAULT),

  %% when I should die
  F0 =/= true andalso exit(normal),

  TurnAroundTime = ts() - TS,
  IsLeap = F5 =/= 0
    andalso TurnAroundTime > F5
    andalso TurnAroundTime < T1
    andalso TurnAroundTime < T2,
  IsRunQBurst = RunQLen > ScheduleCount * F1,
  {NewCredit, SleepMs, NewFlag}
    = case IsRunQBurst of
        true when not IsFlag andalso CurrentCredit =< 0 ->
          kill_priority_groups(F3),
          ?tp(debug, lc_runq, State#{event => flag_on}),
          {0, T1 + T2, true};
        true when CurrentCredit > 0 ->
          %% overloaded, but still have credits
          ?tp(debug, lc_runq, State#{event => on_fire}),
          CurrentCredit / C1 * 100 < F4 andalso kill_priority_groups(F3),
          case IsLeap of
            true ->
              {CurrentCredit - 2, T2, IsFlag};
            false ->
              {CurrentCredit - 1, T2, IsFlag}
          end;
        false when IsFlag andalso CurrentCredit == C1 ->
          %% cool down, remove flag
          ?tp(debug, lc_runq, State#{event => flag_off}),
          {C1, T1, false};
        false when CurrentCredit < C1 ->
          %% cool down, recovering
          case lists:keyfind(total, 1, scheduler:utilization(LastSample)) of
            {total, Util, _} when Util < F2 ->
              %% gain credit only when utilization is recovering as well
              ?tp(debug, lc_runq, State#{event => cooldown_success}),
              {CurrentCredit + 1, T1, IsFlag};
            _ ->
              ?tp(debug, lc_runq, State#{event => cooldown_pending}),
              {CurrentCredit, T1, IsFlag}
          end;
        _ ->
          ?tp(lc_runq, State#{event => noop}),
          {CurrentCredit, T1, IsFlag}
      end,

  NewState = State#{ current_credit => NewCredit
                   , sample => scheduler:sample()
                   , last_ts => ts()
                   , is_flagged => NewFlag
                   , alarm_info => #{runq_length => RunQLen}
                   },
  {SleepMs, NewState}.


kill_priority_groups(Threshold) when is_integer(Threshold) ->
  ?tp(debug, lc_runq, #{event => kill_priority_groups}),
  lists:foreach(
    fun(P) ->
        lists:foreach(
          fun(Pid) -> exit(Pid, kill) end,
          pg:get_local_members(?LC_SCOPE, {?LC_GROUP, P})
         )
    end,  lists:seq(0, Threshold)).

ts() ->
  erlang:monotonic_time(millisecond).
%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
