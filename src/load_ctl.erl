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

-module(load_ctl).

-include("lc.hrl").

-export([ is_overloaded/0
        , is_high_mem/0
        , maydelay/1
        , accompany/2
        , join/1
        , leave/1
        , whereis_runq_flagman/0
        , whereis_mem_flagman/0
        , stop_runq_flagman/0
        , stop_runq_flagman/1
        , stop_mem_flagman/0
        , stop_mem_flagman/1
        , restart_runq_flagman/0
        , restart_mem_flagman/0
        , put_config/1
        , get_config/0
        ]).

%% overload check for realtime processing
-spec is_overloaded() -> boolean().
is_overloaded() ->
  not (undefined =:= whereis(?RUNQ_MON_FLAG_NAME)).

%% overload check for realtime processing
-spec is_high_mem() -> boolean().
is_high_mem() ->
  not (undefined =:= whereis(?MEM_MON_FLAG_NAME)).

-spec maydelay(timer:timeout()) -> ok | timeout | false.
maydelay(Timeout) ->
  case is_overloaded() andalso accompany(?RUNQ_MON_FLAG_NAME, Timeout) of
    ok ->
      ok;
    {error, timeout} ->
      timeout;
    false ->
      false
  end.

%% process is put into a process priority group
%% process will be killed if system is overloaded when priority is under threshold
-spec join(non_neg_integer()) -> ok.
join(Priority) ->
  ok = pg:join(?LC_SCOPE, {?LC_GROUP, Priority}, self()).

%% Leave process priority group, the number of leaves should match the number of join
-spec leave(non_neg_integer()) -> ok.
leave(Priority) ->
  ok = pg:leave(?LC_SCOPE, {?LC_GROUP, Priority}, self()).

-spec put_config(map()) -> ok | {error, badarg}.
put_config(Config) when is_map(Config) ->
  %% @TODO config validation.
  persistent_term:put(?FLAG_MAN_CONFIGS_TERM, filter_config(Config));
put_config(_) ->
  {error, badarg}.

-spec stop_runq_flagman() -> ok.
stop_runq_flagman() ->
  lc_sup:stop_runq_flagman(infinity).

-spec stop_runq_flagman(timer:timeout()) -> ok | {error, timeout}.
stop_runq_flagman(Timeout) ->
  lc_sup:stop_runq_flagman(Timeout).

-spec stop_mem_flagman() -> ok.
stop_mem_flagman() ->
  lc_sup:stop_mem_flagman(infinity).

-spec stop_mem_flagman(timer:timeout()) -> ok | {error, timeout}.
stop_mem_flagman(Timeout) ->
  lc_sup:stop_mem_flagman(Timeout).

-spec whereis_runq_flagman() -> pid() | undefined.
whereis_runq_flagman() ->
  lc_sup:whereis_runq_flagman().

-spec whereis_mem_flagman() -> pid() | undefined.
whereis_mem_flagman() ->
  lc_sup:whereis_mem_flagman().

-spec restart_runq_flagman() -> {ok, pid()} | {error, running | restarting | disabled}.
restart_runq_flagman() ->
  case get_config() of
    #{?RUNQ_MON_F0 := false} ->
      {error, disabled};
    _ ->
      lc_sup:restart_runq_flagman()
  end.

-spec restart_mem_flagman() -> {ok, pid()} | {error, running | restarting | disabled}.
restart_mem_flagman() ->
  case get_config() of
    #{?MEM_MON_F0 := false} ->
      {error, disabled};
    _ ->
      lc_sup:restart_mem_flagman()
  end.

-spec get_config() -> map().
get_config() ->
  case persistent_term:get(?FLAG_MAN_CONFIGS_TERM, undefined) of
    undefined ->
      %% return defaults
      #{ ?RUNQ_MON_F0 => ?RUNQ_MON_F0_DEFAULT
       , ?RUNQ_MON_F1 => ?RUNQ_MON_F1_DEFAULT
       , ?RUNQ_MON_F2 => ?RUNQ_MON_F2_DEFAULT
       , ?RUNQ_MON_F3 => ?RUNQ_MON_F3_DEFAULT
       , ?RUNQ_MON_F4 => ?RUNQ_MON_F4_DEFAULT
       , ?RUNQ_MON_T1 => ?RUNQ_MON_T1_DEFAULT
       , ?RUNQ_MON_T2 => ?RUNQ_MON_T2_DEFAULT
       , ?RUNQ_MON_C1 => ?RUNQ_MON_C1_DEFAULT

       , ?MEM_MON_F0 => ?MEM_MON_F0_DEFAULT
       , ?MEM_MON_F1 => ?MEM_MON_F1_DEFAULT
       , ?MEM_MON_T1 => ?MEM_MON_T1_DEFAULT
       };
    Other ->
      Other
  end.

filter_config(Config) ->
  maps:with([ ?RUNQ_MON_F0
            , ?RUNQ_MON_F1
            , ?RUNQ_MON_F2
            , ?RUNQ_MON_F3
            , ?RUNQ_MON_F4
            , ?RUNQ_MON_T1
            , ?RUNQ_MON_T2
            , ?RUNQ_MON_C1

            , ?MEM_MON_F0
            , ?MEM_MON_F1
            , ?MEM_MON_T1
            ], Config).

-spec accompany(atom() | pid(), timer:timeout()) -> ok | {error, timeout}.
accompany(Target, Timeout) ->
  Mref = erlang:monitor(process, Target),
  receive
    {'DOWN', Mref, process, _Object, _Info} -> ok
  after Timeout ->
      erlang:demonitor(Mref, [flush]),
      {error, timeout}
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
