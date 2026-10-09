# 0.0.3

### `treeVolume`

+ **Breaking change:** `treeVolume` now takes a data frame (`data`) where each row is an individual tree. `dbh` and `mht` are now unquoted names of columns in `data` (e.g., `dbh = DBH`). Quoted column names are not accepted.
+ `type` can be either an unquoted column name in `data` (so the volume type can vary by tree) or a single quoted volume type applied to all trees (e.g., `type = 'doyle'`). In the latter case, a `type` column recording the volume type is added to the output.
+ `gfc` can be either an unquoted column name in `data` or a single numeric value (default 78).
+ Returns the input data frame with `volume` and `units` columns added (existing columns with those names are overwritten with a warning).

### `treeMerch`

+ Updated to use the new `treeVolume` interface. Output is unchanged.

### `standStock`, `calcEsts`, `importanceValue`, `standEsts`

+ **Breaking change:** all arguments that refer to columns in the input data frame (`plotID`, `variable`, `grpBy`, `plotSize`, `baColumn`, `species`, `standID`) now take unquoted column names, following the same approach as `treeVolume` (e.g., `plotID = PointID` instead of `plotID = 'PointID'`). Quoted column names are not accepted.
+ Arguments that can take multiple columns (`grpBy`, `standID`) accept either a single unquoted name (e.g., `grpBy = Species`) or multiple unquoted names combined with `c()` (e.g., `grpBy = c(Species, DIA_Class)`).
+ Informative errors are now given when the data argument is not a data frame, when a column argument is not an unquoted name, or when a supplied column is not in the data. Output is unchanged.

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
