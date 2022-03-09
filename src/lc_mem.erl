%%--------------------------------------------------------------------
%% Copyright (c) 2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(lc_mem).
-behavior(lc_flag_man).
-export([ init/1
        , check/1]).

-include("lc.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").


-spec init(#{callback := module()}) ->
        #{ is_flagged := boolean()
         , flag_name := atom()
         , alarm_name := atom()
         , callback := module()
         }.
init(#{callback:=_} = S0) ->
  S0#{ is_flagged => false
     , flag_name => ?MEM_MON_FLAG_NAME
     , alarm_name => ?LC_ALARM_ID_MEM
     }.


-spec check(#{callback:=_}) ->
        {NextCheckDelayMs::integer(),  #{ is_flagged := boolean()
                                        , flag_name := atom()
                                        , alarm_name := atom()
                                        , alarm_info := map()
                                        , callback := module()
                                        }}.
check(#{callback:= _} = S) ->
  F0 = lc_lib:config_get(?MEM_MON_F0, ?MEM_MON_F0_DEFAULT),
  true =/= F0 andalso exit(normal),
  New = do_check_memory(S),
  DelayMs = lc_lib:config_get(?MEM_MON_T1, ?MEM_MON_T1_DEFAULT),
  {DelayMs, New}.

do_check_memory(State) ->
  F0 = lc_lib:config_get(?MEM_MON_F1, ?MEM_MON_F1_DEFAULT),
  case lc_lib:get_memory_usage() of
    U when U >= F0->
      Alarm = #{mem_usage => U},
      State#{is_flagged => true, alarm_info => Alarm};
    _ ->
      State#{is_flagged => false}
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
