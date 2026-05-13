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
-module(lc_lib).

-export([ get_memory_usage/0
        , get_sys_memory/0
        , configs/0
        , config_get/2
        , config_get/3
        ]).

%% hidden export
-export([ get_sys_memory_usage/0
        , get_cgroup_memory_usage/0
        , get_cgroup2_memory_usage/0
        ]).

%% @doc Return RAM usage ratio and total number of bytes.
%% `{0, 0}' indicates an error in collecting the stats.
-spec get_sys_memory() -> {number(), number()}.
get_sys_memory() ->
  case os:type() of
    {unix, linux} ->
          %% Read all three sources and pick the most constraining: smallest
          %% non-zero total wins; ties go to the larger usage ratio.
          %%
          %% On hosts where cgroup is mounted but no memory limit is applied,
          %% cgroup v1 reports a sentinel limit (~8 EiB) for memory.limit_in_bytes,
          %% which dwarfs the actual physical memory and drives the ratio to ~0.
          %% (cgroup v2 reports the string "max" in that case, which already
          %% yields {0, 0} via read_int_fs/1.) The smallest non-zero total is
          %% always the binding limit: a real cgroup cap is at most node memory,
          %% and an unlimited cgroup's sentinel is far larger than node memory.
          pick_most_constraining([do_get_cgroup2_memory_usage(),
                                  do_get_cgroup_memory_usage(),
                                  do_get_sys_memory_usage()]);
      _ ->
          do_get_sys_memory_usage()
  end.

pick_most_constraining(Readings) ->
  case [R || {_, Total} = R <- Readings, Total > 0] of
    [] -> {0, 0};
    [First | Rest] -> lists:foldl(fun pick_reading/2, First, Rest)
  end.

pick_reading({_, T1} = A, {_, T2}) when T1 < T2 -> A;
pick_reading({_, T1}, {_, T2} = B) when T1 > T2 -> B;
pick_reading({R1, _} = A, {R2, _}) when R1 >= R2 -> A;
pick_reading(_, B) -> B.

%% @doc Return RAM usage ratio (from 0 to 1).
%% `0' probably indicates an error in collecting the stats.
-spec get_memory_usage() -> number().
get_memory_usage()->
    {Ratio, _} = get_sys_memory(),
    Ratio.

-spec config_get(atom(), any()) -> any().
config_get(Name, Default)->
    config_get(Name, configs(), Default).

config_get(Name, ConfigTerm, Default) when is_map(ConfigTerm) ->
  maps:get(Name, ConfigTerm, Default).

-spec configs() -> map().
configs() ->
  load_ctl:get_config().

-spec get_sys_memory_usage() -> number().
get_sys_memory_usage() ->
  {Ratio, _Total} = do_get_sys_memory_usage(),
  Ratio.

do_get_sys_memory_usage() ->
  try
    IsMemSup = is_pid(whereis(memsup)),
    do_get_sys_memory_usage(IsMemSup)
  catch _:_ ->
    {0, 0}
  end.

do_get_sys_memory_usage(true)->
  Data = memsup:get_system_memory_data(),
  Avail = resolve_available_mem(Data),
  Total = proplists:get_value(total_memory, Data),
  Used = Total - Avail,
  {Used / Total, Total};
do_get_sys_memory_usage(_) ->
  {0, 0}.

get_cgroup_path(Name) ->
  {ok, Lines} = file:read_file("/proc/self/cgroup"),
  get_cgroup_by_name(binary:split(Lines, <<"\n">>, [global]), Name).

get_cgroup_by_name([], _Name) ->
  %% fallback
  "";
get_cgroup_by_name([H|T], Name) ->
  case binary:split(H, <<":">>, [global]) of
    [_, Name, << $/, Path/binary >>] ->
      Path;
    _ ->
      get_cgroup_by_name(T, Name)
  end.

-spec get_cgroup_memory_usage() -> number().
get_cgroup_memory_usage() ->
    {Ratio, _Total} = do_get_cgroup_memory_usage(),
    Ratio.

do_get_cgroup_memory_usage() ->
  try
    CgroupMem = "/sys/fs/cgroup/memory",
    Paths = [filename:join([CgroupMem, get_cgroup_path(<<"memory">>)]), CgroupMem],
    CgroupPath = first_existing(Paths),
    CgroupUsed = read_int_fs(filename:join([CgroupPath, "memory.usage_in_bytes"])),
    CgroupTotal = read_int_fs(filename:join([CgroupPath, "memory.limit_in_bytes"])),
    {CgroupUsed/CgroupTotal, CgroupTotal}
  catch error:_ ->
    {0, 0}
  end.

first_existing([]) ->
  error(none_exist);
first_existing([H|T]) ->
  case filelib:is_dir(H) of
    true -> H;
    _ -> first_existing(T)
  end.

-spec get_cgroup2_memory_usage() -> number().
get_cgroup2_memory_usage() ->
    {Ratio, _Total} = do_get_cgroup2_memory_usage(),
    Ratio.

do_get_cgroup2_memory_usage() ->
  try
    Cgroup = "/sys/fs/cgroup",
    Paths = [filename:join(Cgroup, get_cgroup_path(<<>>)), Cgroup],
    CgroupPath = first_existing(Paths),
    CgroupUsed = read_int_fs(filename:join([CgroupPath,
                                            "memory.current"])
                            ),
    CgroupTotal = read_int_fs(filename:join([CgroupPath,
                                             "memory.max"])),
    {CgroupUsed/CgroupTotal, CgroupTotal}
  catch error:_ ->
    {0, 0}
  end.

-spec read_int_fs(file:name()) -> non_neg_integer() | error.
read_int_fs(Path) ->
  try
    {ok, Bin} = file:read_file(Path),
    Str = binary:bin_to_list(Bin),
    case lists:suffix("\n", Str) of
      true ->
        %% for cgroup2 we will get "max" which means unlimited
        %% and it is ok to let it throw error here
        list_to_integer(string:strip(Str, right, $\n));
      false ->
        %% We get incomplete data
        error(incomplete_data)
    end
  catch error : _ ->
      error(faild_to_parse)
  end.

%% From OTP doc for `available_memory':
%% This value is currently only present on newer Linux kernels.
%% If this value is not available on Linux,
%% you can use the sum of cached_memory, buffered_memory,
%% and free_memory as an approximation.
resolve_available_mem(Data) ->
  case proplists:get_value(available_memory, Data) of
    V when is_integer(V) ->
      V;
    _ ->
      calculate_available_mem(Data)
  end.

calculate_available_mem(Data) ->
  proplists:get_value(cached_memory, Data, 0) +
  proplists:get_value(buffered_memory, Data, 0) +
  proplists:get_value(free_memory, Data, 0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
get_cgroup_by_name_test_() ->
  In1 = [<<"8:memory:/kubepods.slice/kubepods-pod47f7b13a_7b34_454d_81cd_9f06cf68ffd3.slice/cri-containerd-261ebed2f68c8399165b5ed5d6ecb26cd470722f05429dc96915d373ceb3a1f1.scope">>],
  In2 = [<<"8:memory:kubepods.slice/kubepods-pod47f7b13a_7b34_454d_81cd_9f06cf68ffd3.slice/cri-containerd-261ebed2f68c8399165b5ed5d6ecb26cd470722f05429dc96915d373ceb3a1f1.scope">>],
  [?_assertEqual(<<"kubepods.slice/kubepods-pod47f7b13a_7b34_454d_81cd_9f06cf68ffd3.slice/cri-containerd-261ebed2f68c8399165b5ed5d6ecb26cd470722f05429dc96915d373ceb3a1f1.scope">>,
                 get_cgroup_by_name(In1, <<"memory">>)),
   ?_assertEqual([], get_cgroup_by_name(In2, <<"memory">>))
  ].

pick_most_constraining_test_() ->
  %% cgroup v1 sentinel for "no limit set"
  CgV1Unlimited = 9223372036854771712,
  HostTotal = 32 * 1024 * 1024 * 1024,
  CgLimit = 4 * 1024 * 1024 * 1024,
  [
   %% all sources failed
   ?_assertEqual({0, 0}, pick_most_constraining([{0, 0}, {0, 0}, {0, 0}])),
   %% cgroup unlimited (sentinel total) -> host wins as the smallest real total
   ?_assertEqual({0.5, HostTotal},
                 pick_most_constraining([{0, 0},
                                         {0.0001, CgV1Unlimited},
                                         {0.5, HostTotal}])),
   %% cgroup limit set below host total -> cgroup is the binding limit
   ?_assertEqual({0.9, CgLimit},
                 pick_most_constraining([{0, 0},
                                         {0.9, CgLimit},
                                         {0.3, HostTotal}])),
   %% same total -> larger usage wins (defensive tie-break)
   ?_assertEqual({0.8, HostTotal},
                 pick_most_constraining([{0.2, HostTotal},
                                         {0.8, HostTotal}]))
  ].
-endif.
