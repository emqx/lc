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
        , get_sys_memory_usage/0
        , get_cgroup_memory_usage/0
        , get_cgroup2_memory_usage/0
        , configs/0
        , config_get/2
        , config_get/3
        ]).


-spec get_memory_usage() -> number().
get_memory_usage()->
  case os:type() of
    {unix, linux} ->
      lists:max([get_sys_memory_usage(),
                 get_cgroup_memory_usage(),
                 get_cgroup2_memory_usage()
                ]);
    _ ->
      get_sys_memory_usage()
  end.

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
  try
    IsMemSup = is_pid(whereis(memsup)),
    do_get_sys_memory_usage(IsMemSup)
  catch _:_ ->
      0
  end.

do_get_sys_memory_usage(true)->
  Data = memsup:get_system_memory_data(),
  Avail = proplists:get_value(available_memory, Data),
  Total = proplists:get_value(total_memory, Data),
  1 - (Avail/Total);
do_get_sys_memory_usage(_) ->
  0.

get_cgroup_path(Name) ->
  {ok, Lines} = file:read_file("/proc/self/cgroup"),
  get_cgroup_by_name(binary:split(Lines, <<"\n">>, [global]), Name).

get_cgroup_by_name([], _Name) ->
  %% fallback
  "";
get_cgroup_by_name([H|T], Name) ->
  case binary:split(H, <<":">>, [global]) of
    [_, Name, << $/, Path >>] ->
      Path;
    _ ->
      get_cgroup_by_name(T, Name)
  end.

-spec get_cgroup_memory_usage() -> number().
get_cgroup_memory_usage() ->
  try
    Path = get_cgroup_path(<<"memory">>),
    CgroupUsed = read_int_fs(filename:join(["/sys/fs/cgroup/",
                                            memory, Path, "memory.usage_in_bytes"])
                            ),
    CgroupTotal = read_int_fs(filename:join(["/sys/fs/cgroup/",
                                             memory, Path, "memory.limit_in_bytes"])),
    CgroupUsed/CgroupTotal
  catch
    error:_ ->
      0
  end.

-spec get_cgroup2_memory_usage() -> number().
get_cgroup2_memory_usage() ->
  try
    CgroupUsed = read_int_fs(filename:join(["/sys/fs/cgroup/",
                                            "memory.current"])
                            ),
    CgroupTotal = read_int_fs(filename:join(["/sys/fs/cgroup/",
                                             "memory.max"])),
    CgroupUsed/CgroupTotal
  catch
    error:_ ->
      0
  end.

-spec read_int_fs(filelib:filename()) -> non_neg_integer() | error.
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
        error
    end
  catch error:_:_ ->
      error
  end.

