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
%%%-------------------------------------------------------------------
%% @doc lc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lc_sup).

-behaviour(supervisor).

-export([ start_link/0
        , stop_runq_flagman/1
        , restart_runq_flagman/0
        , whereis_runq_flagman/0
        ]).

-export([init/1]).

-include("include/lc.hrl").

-define(SERVER, ?MODULE).

-define(flagman_runq, flagman_runq).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec whereis_runq_flagman() -> undefined | pid().
whereis_runq_flagman() ->
  case lists:keyfind(?flagman_runq, 1, supervisor:which_children(?MODULE)) of
    {_, Pid, _, _} when is_pid(Pid)->
      Pid;
    _ ->
      undefined
  end.

-spec stop_runq_flagman(timer:timeout()) -> ok | {error, timeout}.
stop_runq_flagman(Timeout)->
  Old = load_ctl:get_config(),
  ok = load_ctl:put_config(Old#{?RUNQ_MON_F0 => false}),
  case whereis_runq_flagman() of
    undefined -> ok;
    Pid when is_pid(Pid) ->
      load_ctl:accompany(Pid, Timeout)
  end.

restart_runq_flagman() ->
  supervisor:restart_child(?MODULE, ?flagman_runq).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 10,
               period => 3},
  ChildSpecs = [#{ id => ?flagman_runq
                 , start => {lc_flag_man, start_link, []}
                 , restart => transient
                 , type => worker
                 },
                #{ id => lc_pg
                 , start => {pg, start_link, [?LC_SCOPE]}
                 , restart => permanent
                 , type => worker
                 }
               ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
