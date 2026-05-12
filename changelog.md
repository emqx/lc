# 0.3.6
- Prefer cgroup/cgroup2 memory readings over host `/proc/meminfo`. In containers, the host view could report >70% while the cgroup limit (what actually constrains the process) is <10%, or vice versa; `lists:max` over the three readings picked the misleading one.

# 0.3.5
- Fix vsn in app.src

# 0.3.4
- Make memory usage reading support EKS

# 0.3.3
- Fix cgroup name parsing

# 0.3.2
- Publish to hex, no diff in Erlang code

# 0.3.1
- Fix return value: should return ratio and total (but not used)
- Calculate available memory (from sum of `cached_memory`, `buffered_memory` and `free_memory`) when `available_memory` is not supported.

# 0.3.0
- Add APIs which return both ratio and total RAM

