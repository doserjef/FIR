library(testthat)
library(FIR)

# Input validation --------------------------------------------------------

test_that("treeVolume stops when dbh is missing", {
  expect_error(treeVolume(mht = 2, type = 'doyle'), 'dbh must be specified')
})

test_that("treeVolume stops when dbh is non-numeric", {
  expect_error(treeVolume(dbh = 'ten', mht = 2, type = 'doyle'),
               'dbh must be numeric')
})

test_that("treeVolume stops when mht is missing", {
  expect_error(treeVolume(dbh = 10, type = 'doyle'),
               'merchantable height.*must be specified')
})

test_that("treeVolume stops when mht is non-numeric", {
  expect_error(treeVolume(dbh = 10, mht = 'two', type = 'doyle'),
               'mht must be numeric')
})

test_that("treeVolume stops with invalid mht_units", {
  expect_error(treeVolume(dbh = 10, mht = 2, mht_units = 'meters', type = 'doyle'),
               'mht_units')
})

test_that("treeVolume stops with non-numeric gfc", {
  expect_error(treeVolume(dbh = 10, mht = 2, gfc = 'seventy-eight', type = 'doyle'),
               'gfc must be a numeric')
})

test_that("treeVolume stops when gfc is wrong length", {
  expect_error(treeVolume(dbh = c(10, 12), mht = c(2, 2), gfc = c(78, 80, 82), type = 'doyle'),
               'gfc must be a vector')
})

test_that("treeVolume stops with invalid type", {
  expect_error(treeVolume(dbh = 10, mht = 2, type = 'bad_type'),
               'type must be one of the following')
})

test_that("treeVolume stops when type is wrong length", {
  expect_error(
    treeVolume(dbh = c(10, 12), mht = c(2, 2), type = c('doyle', 'scribner', 'international')),
    'type must be either length 1'
  )
})

# Return structure --------------------------------------------------------

test_that("treeVolume returns a data frame with correct columns", {
  result <- treeVolume(dbh = 10, mht = 2, type = 'doyle')
  expect_s3_class(result, 'data.frame')
  expect_named(result, c('volume', 'units', 'type'))
})

test_that("treeVolume returns one row per tree", {
  result <- treeVolume(dbh = c(10, 12, 14), mht = c(2, 2.5, 3), type = 'doyle')
  expect_equal(nrow(result), 3)
})

test_that("treeVolume type column matches input type", {
  result <- treeVolume(dbh = c(10, 12), mht = c(2, 3), type = c('doyle', 'huber'))
  expect_equal(result$type, c('doyle', 'huber'))
})

# Units -------------------------------------------------------------------

test_that("treeVolume assigns board_ft units for all board foot rules", {
  result <- treeVolume(dbh = c(10, 10, 10), mht = c(2, 2, 2),
                       type = c('doyle', 'scribner', 'international'))
  expect_true(all(result$units == 'board_ft'))
})

test_that("treeVolume assigns cubic_ft units for cubic foot types", {
  result <- treeVolume(dbh = c(10, 10), mht = c(2, 2),
                       type = c('mesavage_cubic_ft', 'huber'))
  expect_true(all(result$units == 'cubic_ft'))
})

test_that("treeVolume correctly assigns units when types are mixed", {
  result <- treeVolume(dbh = c(10, 12), mht = c(2, 2), type = c('doyle', 'huber'))
  expect_equal(result$units[1], 'board_ft')
  expect_equal(result$units[2], 'cubic_ft')
})

# Recycling ---------------------------------------------------------------

test_that("treeVolume recycles length-1 type across all trees", {
  result <- treeVolume(dbh = c(10, 12, 14), mht = c(2, 2, 2), type = 'doyle')
  expect_true(all(result$type == 'doyle'))
  expect_equal(nrow(result), 3)
})

test_that("treeVolume recycles length-1 gfc across all trees", {
  r1 <- treeVolume(dbh = c(10, 12), mht = c(2, 2), gfc = 78, type = 'doyle')
  r2 <- treeVolume(dbh = c(10, 12), mht = c(2, 2), gfc = c(78, 78), type = 'doyle')
  expect_equal(r1$volume, r2$volume)
})

# Calculations ------------------------------------------------------------

test_that("treeVolume computes Huber volumes correctly", {
  dbh <- 10; mht <- 2
  result <- treeVolume(dbh = dbh, mht = mht, type = 'huber')
  expected <- (pi / 4) * (dbh / 12)^2 * (mht * 16)
  expect_equal(result$volume, expected, tolerance = 1e-6)
})

test_that("treeVolume computes Doyle volumes correctly", {
  dbh <- 10; mht <- 2; gfc <- 78
  result <- treeVolume(dbh = dbh, mht = mht, gfc = gfc, type = 'doyle')
  a <- -29.37337 + 41.51275 * mht + 0.55743 * mht^2
  b <- (2.78043 - 8.77272 * mht - 0.04516 * mht^2) * dbh
  c <- (0.04177 + 0.59042 * mht - 0.01578 * mht^2) * dbh^2
  gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
  expect_equal(result$volume, (a + b + c) * gfc_cor, tolerance = 1e-6)
})

test_that("treeVolume computes Scribner volumes correctly", {
  dbh <- 10; mht <- 2; gfc <- 78
  result <- treeVolume(dbh = dbh, mht = mht, gfc = gfc, type = 'scribner')
  a <- -22.50365 + 17.53508 * mht - 0.59242 * mht^2
  b <- (3.02988 - 4.34381 * mht - 0.02302 * mht^2) * dbh
  c <- (-0.01969 + 0.51593 * mht - 0.02035 * mht^2) * dbh^2
  gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
  expect_equal(result$volume, (a + b + c) * gfc_cor, tolerance = 1e-6)
})

test_that("treeVolume computes International volumes correctly", {
  dbh <- 10; mht <- 2; gfc <- 78
  result <- treeVolume(dbh = dbh, mht = mht, gfc = gfc, type = 'international')
  a <- -13.35212 + 9.58615 * mht + 1.52968 * mht^2
  b <- (1.7962 - 2.59995 * mht - 0.27465 * mht^2) * dbh
  c <- (0.04482 + 0.45997 * mht - 0.00961 * mht^2) * dbh^2
  gfc_cor <- 1.0 + ((gfc - 78) * 0.03)
  expect_equal(result$volume, (a + b + c) * gfc_cor, tolerance = 1e-6)
})

test_that("treeVolume applies GFC correction correctly", {
  dbh <- 10; mht <- 2
  r78 <- treeVolume(dbh = dbh, mht = mht, gfc = 78, type = 'doyle')
  r80 <- treeVolume(dbh = dbh, mht = mht, gfc = 80, type = 'doyle')
  expect_equal(r80$volume, r78$volume * (1 + (80 - 78) * 0.03), tolerance = 1e-6)
})

test_that("treeVolume returns positive volumes for all non-mesavage types", {
  dbh <- c(10, 15, 20)
  mht <- c(2, 3, 4)
  for (type in c('doyle', 'scribner', 'international', 'huber')) {
    result <- treeVolume(dbh = dbh, mht = mht, type = type)
    expect_true(all(result$volume > 0), info = paste('Failed for type:', type))
  }
})

# mht unit conversion -----------------------------------------------------

test_that("treeVolume converts mht from feet to logs and issues a message", {
  dbh <- 10
  result_logs <- treeVolume(dbh = dbh, mht = 2, mht_units = 'log', type = 'doyle')
  expect_message(
    result_feet <- treeVolume(dbh = dbh, mht = 32, mht_units = 'ft', type = 'doyle'),
    'Converting'
  )
  expect_equal(result_logs$volume, result_feet$volume)
})
