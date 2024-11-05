# event.vectors 0.1.5020

## Bug Fixes

- **`retrace.evs()`**: Fixed a bug where `ls()` was being invoked without setting argument **sorted** to `FALSE` resulting in erroneously-mapped lookups.

# event.vectors 0.1.5010

## Updates

- **`signal_processor()`**: Documentation updates

# event.vectors 0.1.5

## Updates

- **`continuity()`**:
   - Internal updates to make code easier to debug
   - Feature `GAP` is now part of the returned columns when `archipelago` is `TRUE`
   - The column order was updated to put columns specified in `map_fields` and conditionally returned via argument `archipelago` at the front.
   - Updated function documentation to reflect changes

# event.vectors 0.1.4

## Enhancements

- **`break_signal()`**: 
   - Added support for `POSIXct` and `character` classes (addresses [\# 7](https://github.com/delriaan/event.vectors/issues/7){title="signal_processor(): Expand Data Types"})
- **`signal_processor()`**: 
   - Added support for `POSIXct` and `character` classes (addresses [\# 7](https://github.com/delriaan/event.vectors/issues/7){title="signal_processor(): Expand Data Types"})
   - Implemented logic to handle the case of serial execution when argument `cl_size` is <= 1L.
   - Added documentation for argument `cl_size`.
- **`event.vectors$make.evs_universe()`**: In the event all source relationships are loops when the event.vectors object configured using the default source mix ("combination"), a warning is produced (regardless of the `chatty` argument), and **no** filtering is performed on the penultimate result that populates class member `space`.

## Updates
- Removed deprecated `purrr` functional style `~{ .x }`
- Replaced references to `purrr::as_mapper()` with anonymous function definitions (i.e. `\(){}`)

# event.vectors 0.1.3.1200

## Bug Fixes

- **`continuity()`**: 
   - Corrected temporal partitioning logic that detects the end of a partition.

- **`event.vectors`**:
   - Replaced references of `%<>%` with explicit re-assignment.
   - Replaced references to `%$%` with the functional form
