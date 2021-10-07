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

-ifndef(LC_HRL).
-define(LC_HRL, true).

-define(LC_ALARM_ID_RUNQ, lc_runq_alarm).

-define(LC_SCOPE, '_lc_scope_').
-define(LC_GROUP, '_lc_group_').

%% runq overload flag name
-define(RUNQ_MON_FLAG_NAME, '_lc_flag_runq_').

%% configs
-define(FLAG_MAN_CONFIGS_TERM, '_lc_flagman_configs_').

%% enable flag
-define(RUNQ_MON_F0, run_queue_f0).
-define(RUNQ_MON_F0_DEFAULT, true).

%% overload multipler
-define(RUNQ_MON_F1, run_queue_f1).
-define(RUNQ_MON_F1_DEFAULT, 8).

%% for recovering,
%% minimal scheduler utilization that regain credits
-define(RUNQ_MON_F2, run_queue_f2).
-define(RUNQ_MON_F2_DEFAULT, 0.8).

%% Priority Threshold
-define(RUNQ_MON_F3, run_queue_f3).
-define(RUNQ_MON_F3_DEFAULT, 2).

%% credit percentage left to kill low prio process
-define(RUNQ_MON_F4, run_queue_f4).
-define(RUNQ_MON_F4_DEFAULT, 50).

%% extra credit loss when turn around time
%% of flagman is above this threshold (ms)
%% set 0 to turn off, value <0 is for testing
-define(RUNQ_MON_F5, run_queue_f5).
-define(RUNQ_MON_F5_DEFAULT, 0).

%% run_queue length probe inteval (ms)
-define(RUNQ_MON_T1, run_queue_t1).
-define(RUNQ_MON_T1_DEFAULT, 3000).

%% extra probe inteval (ms) when system is overloaded
-define(RUNQ_MON_T2, run_queue_t2).
-define(RUNQ_MON_T2_DEFAULT, 1000).

%% credit
-define(RUNQ_MON_C1, run_queue_c1).
-define(RUNQ_MON_C1_DEFAULT, 3).

-endif. %% LC_HRL
