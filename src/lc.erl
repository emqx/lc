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

-module(lc).

-include("lc.hrl").

-export([ is_overloaded/0
        , join/1
        , leave/1
        , put_config/1
        , get_config/0
        ]).

%% overload check for realtime processing
-spec is_overloaded() -> boolean().
is_overloaded() ->
  not (undefined =:= whereis(?RUNQ_MON_FLAG_NAME)).

%% process is put into a process priority group
%% process will be killed if system is overloaded when priority is under threshold
-spec join(integer()) -> ok.
join(Priority) ->
  ok = pg:join(?LC_SCOPE, {?LC_GROUP, Priority}, self()).

%% Leave process priority group, the number of leaves should match the number of join
-spec leave(integer()) -> ok.
leave(Priority) ->
  ok = pg:leave(?LC_SCOPE, {?LC_GROUP, Priority}, self()).

-spec put_config(map()) -> ok | {error, badarg}.
put_config(Config) when is_map(Config) ->
  %% @TODO config validation.
  persistent_term:put(?FLAG_MAN_CONFIGS_TERM, filter_config(Config));
put_config(_) ->
  {error, badarg}.

-spec get_config() -> map() | undefined.
get_config() ->
  persistent_term:get(?FLAG_MAN_CONFIGS_TERM, undefined).

filter_config(Config) ->
  maps:with([ ?RUNQ_MON_F1
            , ?RUNQ_MON_F2
            , ?RUNQ_MON_T1
            , ?RUNQ_MON_T2
            , ?RUNQ_MON_C1
            ], Config).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
