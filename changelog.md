# 0.4.0
- Add `load_ctl:get_system_memory/1` function to get system memory information with timeout.

# 0.3.2
- Publish to hex, no diff in Erlang code

# 0.3.1
- Fix return value: should return ratio and total (but not used)
- Calculate available memory (from sum of `cached_memory`, `buffered_memory` and `free_memory`) when `available_memory` is not supported.

# 0.3.0
- Add APIs which return both ratio and total RAM
