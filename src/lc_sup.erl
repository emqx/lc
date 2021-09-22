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

-export([start_link/0]).

-export([init/1]).

-include("include/lc.hrl").

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 10,
               period => 3},
  ChildSpecs = [#{ id => lc_runq_flag_man
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
