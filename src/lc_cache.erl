%%--------------------------------------------------------------------
%% Copyright (c) 2020-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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
-module(lc_cache).

-behaviour(gen_server).
-define(SYS_MEMORY_CACHE_KEY, sys_memory_cache).

-export([start_link/0, get_sys_memory/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

get_sys_memory(Timeout) ->
    try
        gen_server:call(?MODULE, {get_sys_memory, now_millisecond(), Timeout}, Timeout)
    catch
        exit:{timeout, _} ->
            {Mem, _Time} = get_memory_from_cache(),
            Mem
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [set, named_table, public, {keypos, 1}]),
    {ok, #{}}.

handle_call({get_sys_memory, CallTime, Timeout}, _From, State) ->
    Now = now_millisecond(),
    case Now - CallTime < Timeout of
        true ->
            {reply, update_sys_memory(), State};
        false -> %% don't reply when timeout
            {_LastMem, LastCacheTime} = get_memory_from_cache(),
            case Now - LastCacheTime > Timeout of %% update when cache is stale
                true -> update_sys_memory();
                false -> ok
            end,
            {noreply, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

update_sys_memory() ->
    Memory = load_ctl:get_sys_memory(),
    ets:insert(?MODULE, {?SYS_MEMORY_CACHE_KEY, {Memory, now_millisecond()}}),
    Memory.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_memory_from_cache() ->
    case ets:lookup(?MODULE, ?SYS_MEMORY_CACHE_KEY) of
        [] -> {{0, 0}, 0};
        [{_, CacheVal}] -> CacheVal
    end.

now_millisecond() ->
    erlang:system_time(millisecond).
