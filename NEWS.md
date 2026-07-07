# 0.0.2

### `treeVolume`

+ Added `'huber'` as a new `type` option, which calculates cubic foot volume using Huber's Formula (basal area in sq ft multiplied by merchantable height in ft).
+ Standardized all internal variable names to snake_case.
+ Removed restriction preventing board foot and cubic foot volume types from being mixed within a single call.
+ Function now returns a data frame with columns `volume`, `units` (`'board_ft'` or `'cubic_ft'`), and `type` instead of a plain numeric vector.

### `treeMerch`

+ Updated to handle the new `treeVolume` data frame return; output now includes a `Vol_Units` column indicating `'board_ft'` or `'cubic_ft'` for each tree.
+ Added `'huber'` as a valid `Vol_Type` in the `pricing` data frame.

# 0.0.1

This is the first version of the package, which is being developed for teaching forest mensuration and biometrics at North Carolina State University.
