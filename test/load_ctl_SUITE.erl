%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(load_ctl_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").
-include_lib("lc.hrl").


%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  [{timetrap, 60000} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
  Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
  persistent_term:erase(?FLAG_MAN_CONFIGS_TERM),
  application:stop(lc),
  ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
  [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
  [ lc_app_start
  , lc_app_stop
  , lc_flagman_noop
  , lc_flagman_flag_onoff
  , lc_flagman_recover
  , lc_control_pg
  , lc_flagman_flagoff_after_stop
  , lc_maydely_1
  , lc_flagman_start_stop
  , lc_alarm
  , lc_alarm2
  ].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
lc_app_start(_Config) ->
  application:start(lc),
  supervisor:which_children(lc_sup).

lc_app_stop(_Config) ->
  application:stop(lc).

lc_flagman_noop(_Config) ->
  application:ensure_all_started(lc),
  NProc = erlang:system_info(schedulers_online),
  ?check_trace(#{timetrap => 30000},
               begin
                 Pid = spawn(?MODULE, worker_parent, [NProc, {?MODULE, busy_loop, []}]),
                 timer:sleep(timer:seconds(10)),
                 ?assert(not load_ctl:is_overloaded()),
                 exit(Pid, kill),
                 ok
               end,
               fun(_, Trace) ->
                   ?projection_complete(event, ?of_kind(lc_flagman, Trace),
                                        [noop])
               end).

lc_flagman_flag_onoff(_Config) ->
  application:ensure_all_started(lc),
  ok = load_ctl:put_config(#{ ?RUNQ_MON_T1 => 1000
                            , ?RUNQ_MON_T2 => 500
                            , ?RUNQ_MON_C1 => 3
                      }),
  NProc = erlang:system_info(schedulers_online),
  ?check_trace(#{timetrap => 30000},
               begin
                 Ppid = spawn(?MODULE, worker_parent, [NProc * 10, {?MODULE, busy_loop, []}]),
                 timer:sleep(timer:seconds(5)),
                 Check1 = load_ctl:is_overloaded(),
                 exit(Ppid, kill),
                 timer:sleep(timer:seconds(5)),
                 Check2 = load_ctl:is_overloaded(),
                 {Check1, Check2}
               end,
               fun(Result, Trace) ->
                   ct:pal("Trace is ~p", [Trace]),
                   ?assertEqual({true, false}, Result),

                   ?strict_causality(
                      #{?snk_kind := lc_flagman, event := flag_on},
                      #{?snk_kind := lc_flagman, event := flag_off},
                      Trace
                     )
               end).

lc_flagman_recover(_Config) ->
  application:ensure_all_started(lc),
  NProc = erlang:system_info(schedulers_online),
  ok = load_ctl:put_config(#{ ?RUNQ_MON_T1 => 500
                            , ?RUNQ_MON_T2 => 200
                            , ?RUNQ_MON_C1 => 2
                            , ?RUNQ_MON_F2 => 0.0
                            }),
  ?check_trace(#{timetrap => 30000},
               begin
                 Ppid = spawn(?MODULE, worker_parent, [NProc * 10, {?MODULE, busy_loop, []}]),
                 timer:sleep(timer:seconds(5)),
                 Check1 = load_ctl:is_overloaded(),
                 exit(Ppid, kill),
                 timer:sleep(timer:seconds(5)),
                 Check2 = load_ctl:is_overloaded(),
                 {Check1, Check2}
               end,
               fun(Result, Trace) ->
                   ct:pal("Trace is ~p", [Trace]),
                   ?assertEqual({true, true}, Result),
                   ?projection_complete(event, ?of_kind(lc_flagman, Trace),
                                        [on_fire, flag_on, noop, cooldown_pending])
               end).


lc_control_pg(_Config) ->
  application:ensure_all_started(lc),
  NProc = erlang:system_info(schedulers_online),
  ok = load_ctl:put_config(#{ ?RUNQ_MON_T1 => 500
                            , ?RUNQ_MON_T2 => 200
                            , ?RUNQ_MON_F2 =>  0.8
                            , ?RUNQ_MON_F3 => 2
                      }),
  ?check_trace(#{timetrap => 30000},
               begin
                 BusyPid = spawn(?MODULE, worker_parent, [NProc * 10, {?MODULE, busy_loop, []}]),
                 P1pid = spawn(?MODULE, worker_parent, [1, {?MODULE, priority_loop, [1]}]),
                 P2pid = spawn(?MODULE, worker_parent, [1, {?MODULE, priority_loop, [2]}]),
                 P3pid = spawn(?MODULE, worker_parent, [1, {?MODULE, priority_loop, [3]}]),
                 timer:sleep(timer:seconds(5)),
                 Check1 = load_ctl:is_overloaded(),
                 exit(BusyPid, kill),
                 timer:sleep(timer:seconds(5)),
                 Check2 = load_ctl:is_overloaded(),
                 Check3 = is_process_alive(P1pid),
                 Check4 = is_process_alive(P2pid),
                 Check5 = is_process_alive(P3pid),
                 [ exit(P, kill) || P <- [P1pid, P2pid, P3pid] ],
                 {Check1, Check2, Check3, Check4, Check5}
               end,
               fun(Result, Trace) ->
                   ct:pal("Trace is ~p", [Trace]),
                   ?assertEqual({true, false, false, false, true}, Result),
                   ?projection_complete(event, ?of_kind(lc_flagman, Trace),
                                        [on_fire, flag_on, noop,
                                         kill_priority_groups
                                        ])
               end).

lc_flagman_start_stop(_Config) ->
  application:ensure_all_started(lc),
  wait_for_runq_flagman(_Retry = 10),
  ?assert(is_pid(load_ctl:whereis_runq_flagman())),
  ok = load_ctl:stop_runq_flagman(10000),
  ?assertEqual(undefined, load_ctl:whereis_runq_flagman()),
  {ok, Pid} = load_ctl:restart_runq_flagman(),
  ?assertEqual(Pid, load_ctl:whereis_runq_flagman()),
  ok.

lc_flagman_flagoff_after_stop(Config) ->
  lc_flagman_recover(Config),
  ?assert(load_ctl:is_overloaded()),
  ok = load_ctl:stop_runq_flagman(10000),
  ?assert(not load_ctl:is_overloaded()),
  ok.

lc_maydely_1(Config) ->
  lc_flagman_recover(Config),
  StartTS = os:timestamp(),
  ?assertEqual(timeout, load_ctl:maydelay(2000)),
  ?assert(timer:now_diff(os:timestamp(), StartTS) < 2500000),
  exit(whereis(?RUNQ_MON_FLAG_NAME), kill),
  StartTS2 = os:timestamp(),
  ?assertEqual(ok, load_ctl:maydelay(1000)),
  ?assert(timer:now_diff(os:timestamp(), StartTS2) < 50000).

lc_alarm(Config) ->
  alarm_handler:start_link(),
  lc_flagman_recover(Config),
  [{?LC_ALARM_ID_RUNQ, #{node := Node, runq_length := QLen}}] = alarm_handler:get_alarms(),
  ?assertEqual(Node, node()),
  ?assert(QLen > 0),
  LConfig = load_ctl:get_config(),
  load_ctl:put_config(LConfig#{?RUNQ_MON_F2 => 0.5}),
  timer:sleep(5000),
  ?assertMatch([], alarm_handler:get_alarms()),
  ok.

lc_alarm2(Config) ->
  alarm_handler:start_link(),
  lc_flagman_recover(Config),
  ?assertMatch([{?LC_ALARM_ID_RUNQ, _}], alarm_handler:get_alarms()),
  ok = load_ctl:stop_runq_flagman(10000),
  timer:sleep(100),
  ?assertMatch([], alarm_handler:get_alarms()),
  ok.

%% internal helper
worker_parent(N, {M, F, A}) ->
  lists:foreach(fun(_) ->
                    proc_lib:spawn_link(fun() -> apply(M, F, A) end)
                end, lists:seq(1, N)),
  receive stop -> ok end.

busy_loop() ->
  busy_loop().

priority_loop(P) ->
  ok = load_ctl:join(P),
  receive
    stop -> ok;
    Other ->
      ct:pal("recv ~p", [Other])
  end.

wait_for_runq_flagman(0) ->
  ct:fail(flagman_not_up);
wait_for_runq_flagman(Retry) ->
  case load_ctl:whereis_runq_flagman() of
    undefined ->
      timer:sleep(50),
      wait_for_runq_flagman(Retry - 1);
    Pid ->
      Pid
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
