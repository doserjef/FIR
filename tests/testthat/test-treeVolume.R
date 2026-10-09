library(testthat)
library(FIR)

# Helper to build a tree data frame
make_trees <- function(dbh, mht, type, ...) {
  data.frame(DBH = dbh, Height = mht, VolType = type, ...)
}

tv <- function(trees, ...) {
  treeVolume(data = trees, dbh = DBH, mht = Height, type = VolType, ...)
}

# Input validation --------------------------------------------------------

test_that("treeVolume stops when data is missing", {
  expect_error(treeVolume(dbh = DBH, mht = Height, type = VolType),
               'data must be provided')
})

test_that("treeVolume stops when data is not a data frame", {
  expect_error(treeVolume(data = c(10, 12), dbh = DBH, mht = Height, type = VolType),
               'data must be a data frame')
})

test_that("treeVolume stops when dbh is missing", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, mht = Height, type = VolType),
               'dbh must be specified')
})

test_that("treeVolume stops when mht is missing", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = DBH, type = VolType),
               'merchantable height.*must be specified')
})

test_that("treeVolume stops when type is missing", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = DBH, mht = Height),
               'type must be specified')
})

test_that("treeVolume stops when a column name is not in data", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = dbh_in, mht = Height, type = VolType),
               'column "dbh_in" supplied to dbh is not in data')
})

test_that("treeVolume stops when a column argument is not a column name", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = 10, mht = Height, type = VolType),
               'dbh must be the unquoted name of a column')
})

test_that("treeVolume rejects quoted column names", {
  trees <- make_trees(10, 2, 'doyle', GFC = 78)
  expect_error(treeVolume(data = trees, dbh = 'DBH', mht = Height, type = VolType),
               'dbh must be the unquoted name of a column')
  expect_error(treeVolume(data = trees, dbh = DBH, mht = 'Height', type = VolType),
               'mht must be the unquoted name of a column')
  expect_error(tv(trees, gfc = 'GFC'), 'gfc must be the unquoted name of a column')
})

test_that("treeVolume treats a quoted type as a volume type, not a column name", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = DBH, mht = Height, type = 'VolType'),
               'must be one of the following')
})

test_that("treeVolume accepts a single quoted type for all trees", {
  trees <- make_trees(c(10, 12), c(2, 3), 'doyle')
  r1 <- treeVolume(data = trees, dbh = DBH, mht = Height, type = 'doyle')
  r2 <- tv(trees)
  expect_equal(r1$volume, r2$volume)
  expect_equal(r1$units, c('board_ft', 'board_ft'))
})

test_that("treeVolume adds a type column when a single quoted type is used", {
  trees <- data.frame(DBH = c(10, 12), Height = c(2, 3))
  result <- treeVolume(data = trees, dbh = DBH, mht = Height, type = 'huber')
  expect_named(result, c('DBH', 'Height', 'volume', 'units', 'type'))
  expect_equal(result$type, c('huber', 'huber'))
})

test_that("treeVolume does not add a type column when type is a column in data", {
  trees <- make_trees(c(10, 12), c(2, 3), 'doyle')
  expect_named(tv(trees), c('DBH', 'Height', 'VolType', 'volume', 'units'))
})

test_that("treeVolume warns when overwriting an existing type column", {
  trees <- data.frame(DBH = 10, Height = 2, type = 'old')
  expect_warning(result <- treeVolume(data = trees, dbh = DBH, mht = Height, type = 'doyle'),
                 '"type".*will be overwritten')
  expect_equal(result$type, 'doyle')
})

test_that("treeVolume stops when unquoted type is not a column in data", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = DBH, mht = Height, type = vol_method),
               'type must be the unquoted name of a column')
})

test_that("treeVolume stops when type is not character", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(treeVolume(data = trees, dbh = DBH, mht = Height, type = 5),
               'type must be the unquoted name of a column')
})

test_that("treeVolume stops when type is the wrong length", {
  trees <- make_trees(c(10, 12), c(2, 3), 'doyle')
  expect_error(treeVolume(data = trees, dbh = DBH, mht = Height,
                          type = c('doyle', 'huber', 'doyle')),
               'type must be a column name in data, a single volume type')
})

test_that("treeVolume stops when dbh column is non-numeric", {
  trees <- make_trees('ten', 2, 'doyle')
  expect_error(tv(trees), 'dbh column must be numeric')
})

test_that("treeVolume stops when mht column is non-numeric", {
  trees <- make_trees(10, 'two', 'doyle')
  expect_error(tv(trees), 'mht column must be numeric')
})

test_that("treeVolume stops with invalid mht_units", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(tv(trees, mht_units = 'meters'), 'mht_units')
})

test_that("treeVolume stops with non-numeric gfc", {
  trees <- make_trees(10, 2, 'doyle')
  trees$GFC <- 'seventy-eight'
  expect_error(tv(trees, gfc = GFC), 'gfc must be a numeric value')
})

test_that("treeVolume stops when unquoted gfc is not a column in data", {
  trees <- make_trees(10, 2, 'doyle')
  expect_error(tv(trees, gfc = form_class), 'gfc must be the unquoted name of a column')
})

test_that("treeVolume stops when gfc is wrong length", {
  trees <- make_trees(c(10, 12), c(2, 2), 'doyle')
  expect_error(tv(trees, gfc = c(78, 80, 82)), 'gfc must be a column name')
})

test_that("treeVolume stops with invalid type", {
  trees <- make_trees(10, 2, 'bad_type')
  expect_error(tv(trees), 'must be one of the following')
})

# Return structure --------------------------------------------------------

test_that("treeVolume returns the input data frame with volume and units added", {
  trees <- make_trees(c(10, 12), c(2, 3), 'doyle', Species = c('A', 'B'))
  result <- tv(trees)
  expect_s3_class(result, 'data.frame')
  expect_named(result, c('DBH', 'Height', 'VolType', 'Species', 'volume', 'units'))
  expect_equal(result[, names(trees)], trees)
})

test_that("treeVolume returns one row per tree", {
  trees <- make_trees(c(10, 12, 14), c(2, 2.5, 3), 'doyle')
  expect_equal(nrow(tv(trees)), 3)
})

test_that("treeVolume preserves tibble input", {
  skip_if_not_installed('tibble')
  trees <- tibble::tibble(DBH = c(10, 12), Height = c(2, 3), VolType = 'doyle')
  expect_s3_class(tv(trees), 'tbl_df')
})

test_that("treeVolume accepts a factor type column", {
  trees <- make_trees(c(10, 12), c(2, 3), factor(c('doyle', 'huber')))
  expect_equal(tv(trees)$units, c('board_ft', 'cubic_ft'))
})

test_that("treeVolume warns when overwriting existing volume/units columns", {
  trees <- make_trees(10, 2, 'doyle', volume = 0)
  expect_warning(result <- tv(trees), 'will be overwritten')
  expect_true(result$volume > 0)
})

# Units -------------------------------------------------------------------

test_that("treeVolume assigns board_ft units for all board foot rules", {
  trees <- make_trees(c(10, 10, 10), c(2, 2, 2), c('doyle', 'scribner', 'international'))
  expect_true(all(tv(trees)$units == 'board_ft'))
})

test_that("treeVolume assigns cubic_ft units for cubic foot types", {
  trees <- make_trees(c(10, 10), c(2, 2), c('mesavage_cubic_ft', 'huber'))
  expect_true(all(tv(trees)$units == 'cubic_ft'))
})

test_that("treeVolume correctly assigns units when types are mixed", {
  trees <- make_trees(c(10, 12), c(2, 2), c('doyle', 'huber'))
  result <- tv(trees)
  expect_equal(result$units[1], 'board_ft')
  expect_equal(result$units[2], 'cubic_ft')
})

# GFC ---------------------------------------------------------------------

test_that("treeVolume recycles a single numeric gfc across all trees", {
  trees <- make_trees(c(10, 12), c(2, 2), 'doyle', GFC = c(78, 78))
  r1 <- tv(trees, gfc = 78)
  r2 <- tv(trees, gfc = GFC)
  expect_equal(r1$volume, r2$volume)
})

test_that("treeVolume uses per-tree gfc from a column", {
  trees <- make_trees(c(10, 10), c(2, 2), 'doyle', GFC = c(78, 80))
  result <- tv(trees, gfc = GFC)
  expect_equal(result$volume[2], result$volume[1] * (1 + (80 - 78) * 0.03), tolerance = 1e-6)
})

# Calculations ------------------------------------------------------------

test_that("treeVolume computes Huber volumes correctly", {
  dbh <- 10; mht <- 2
  result <- tv(make_trees(dbh, mht, 'huber'))
  expected <- (pi / 4) * (dbh / 12)^2 * (mht * 16)
  expect_equal(result$volume, expected, tolerance = 1e-6)
})

test_that("treeVolume computes Doyle volumes correctly", {
  dbh <- 10; mht <- 2; gfc <- 78
  result <- tv(make_trees(dbh, mht, 'doyle'), gfc = gfc)
  a <- -29.37337 + 41.51275 * mht + 0.55743 * mht^2
  b <- (2.78043 - 8.77272 * mht - 0.04516 * mht^2) * dbh
  c <- (0.04177 + 0.59042 * mht - 0.01578 * mht^2) * dbh^2
  gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
  expect_equal(result$volume, (a + b + c) * gfc_cor, tolerance = 1e-6)
})

test_that("treeVolume computes Scribner volumes correctly", {
  dbh <- 10; mht <- 2; gfc <- 78
  result <- tv(make_trees(dbh, mht, 'scribner'), gfc = gfc)
  a <- -22.50365 + 17.53508 * mht - 0.59242 * mht^2
  b <- (3.02988 - 4.34381 * mht - 0.02302 * mht^2) * dbh
  c <- (-0.01969 + 0.51593 * mht - 0.02035 * mht^2) * dbh^2
  gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
  expect_equal(result$volume, (a + b + c) * gfc_cor, tolerance = 1e-6)
})

test_that("treeVolume computes International volumes correctly", {
  dbh <- 10; mht <- 2; gfc <- 78
  result <- tv(make_trees(dbh, mht, 'international'), gfc = gfc)
  a <- -13.35212 + 9.58615 * mht + 1.52968 * mht^2
  b <- (1.7962 - 2.59995 * mht - 0.27465 * mht^2) * dbh
  c <- (0.04482 + 0.45997 * mht - 0.00961 * mht^2) * dbh^2
  gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
  expect_equal(result$volume, (a + b + c) * gfc_cor, tolerance = 1e-6)
})

test_that("treeVolume applies GFC correction correctly", {
  trees <- make_trees(10, 2, 'doyle')
  r78 <- tv(trees, gfc = 78)
  r80 <- tv(trees, gfc = 80)
  expect_equal(r80$volume, r78$volume * (1 + (80 - 78) * 0.03), tolerance = 1e-6)
})

test_that("treeVolume returns positive volumes for all non-mesavage types", {
  for (type in c('doyle', 'scribner', 'international', 'huber')) {
    result <- tv(make_trees(c(10, 15, 20), c(2, 3, 4), type))
    expect_true(all(result$volume > 0), info = paste('Failed for type:', type))
  }
})

test_that("treeVolume type matching is case-insensitive", {
  r1 <- tv(make_trees(10, 2, 'Doyle'))
  r2 <- tv(make_trees(10, 2, 'doyle'))
  expect_equal(r1$volume, r2$volume)
})

# mht unit conversion -----------------------------------------------------

test_that("treeVolume converts mht from feet to logs and issues a message", {
  result_logs <- tv(make_trees(10, 2, 'doyle'), mht_units = 'log')
  expect_message(
    result_feet <- tv(make_trees(10, 32, 'doyle'), mht_units = 'ft'),
    'Converting'
  )
  expect_equal(result_logs$volume, result_feet$volume)
})

test_that("treeVolume leaves the original mht column unchanged when converting feet", {
  trees <- make_trees(10, 40, 'doyle')
  result <- suppressMessages(tv(trees, mht_units = 'ft'))
  expect_equal(result$Height, 40)
})
