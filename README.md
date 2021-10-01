# load control erlang application

This is a helper application to enable load control on the erlang node.

The application interface module is the *load_ctl** module, all API calls should go through it.

# APIs

## is_overloaded

``` erlang
load_ctl:is_overloaded() -> boolean().
```

returns true when the system is overloaded.

Ideal for checking in realtime workloads, like before spawning the new process to handle new incoming connection. If the system is overloaded, the new connection could be closed/rejected thus peer client could retry with other nodes in the same cluster. The existing connections/work are kept and service quality (latency, throughput) could be assured. 

## maydelay

``` erlang
maydelay(timer:timeout()) -> ok | timeout.
```

Blocks the caller until the system is not overloaded or timeout.

Ideal for checking in heavy lifting workload. In case some heavy lifting workload is unimportant and could be deferred. 

## join/leave a priority process group

``` erlang
load_ctl:join(Priority::non_neg_integer()) -> ok
do_work()
load_ctl:leave(Priority::non_neg_integer()) -> ok 
```

Caller joins/leaves a priority process group managed by this application. 

While the system is overloaded, the processes in the group under prioriy threshold will be killed forcefully.

It is also configurable that very low priority processes could get killed while system trends become overloaded.

IMPORTANT: Please double check the side effect of the killing process.
If the caller processes are supervised under the same supervior, the bursts restarts could make
the supervior reaches the intensity and period limit and cause the supervior itself get killed.

## enable/disable load control: stop/restart flagman

*flagman* is a daemon process that monitors the system load.
It raises the overload flag when the system is overloaded.
It clears the flag when the system is cooldown.

flagman starts when the application is started.

It can be temptory disabled:

``` erlang
load_ctl:stop_runq_flagman() -> ok. 
%% Or with a TIMEOUT
load_ctl:stop_runq_flagman(timer:timeout()) -> {error, timeout} | ok.

```

It can be restarted after disabled:

``` erlang
load_ctl:restart_runq_flagman() -> ok.
```

note, in current version only runq is monitored.

## Configuration

Get current config

``` erlang
load_ctl:get_config() -> Config::map().
```

Overwrite the current config

``` erlang
load_ctl:put_config(Config::map()) -> ok | {error, badarg}.
```

for config keys in the `Config` map refer to 
[DOC Internals](./docs/internals.md)

## Alarm
Once the system is overloaded, alarm **lc_runq_alarm** is raised via *alarm_handler* with current runq length.

# Dependencies
1. OTP 23+
1. rebar3

# Build

``` sh
$ make
```


