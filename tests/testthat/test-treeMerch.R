library(testthat)
library(FIR)

trees <- data.frame(Product_Type = c('PST', 'PLP'), Height = c(2, 3),
                    Species = 'Loblolly Pine', DBH = c(20, 12), GFC = 78)
price <- data.frame(Product_Type = c('PST', 'PLP'), Price = c(0.20, 0.50),
                    Vol_Type = c('scribner', 'huber'))

test_that("treeMerch with volume matches treeMerch computing volume internally", {
  internal <- treeMerch(data = trees, pricing = price)
  vol_data <- treeVolume(data = trees, dbh = DBH, mht = Height,
                         type = c('scribner', 'huber'), gfc = GFC)
  supplied <- treeMerch(data = vol_data, pricing = price, volume = volume)
  expect_equal(supplied$Volume, internal$Volume)
  expect_equal(supplied$Vol_Units, internal$Vol_Units)
  expect_equal(supplied$Value, internal$Value)
})

test_that("treeMerch with volume only requires Product_Type and the volume column", {
  dat <- data.frame(Product_Type = c('PST', 'PLP'), myVol = c(100, 10))
  result <- treeMerch(data = dat, pricing = price, volume = myVol)
  expect_equal(result$Value, c(20, 5))
  expect_equal(result$Vol_Units, c('board_ft', 'cubic_ft'))
})

test_that("treeMerch rejects invalid volume arguments", {
  expect_error(treeMerch(data = trees, pricing = price, volume = 'DBH'),
               'volume must be the unquoted name of a column')
  expect_error(treeMerch(data = trees, pricing = price, volume = myVol),
               'column "myVol" supplied to volume is not in data')
  expect_error(treeMerch(data = trees, pricing = price, volume = Species),
               'the volume column must contain numeric values')
})

test_that("treeMerch converts dbh in cm to inches", {
  trees_cm <- trees
  trees_cm$DBH <- trees$DBH * 2.54
  expect_equal(treeMerch(data = trees_cm, pricing = price, dbh_units = 'cm')$Value,
               treeMerch(data = trees, pricing = price)$Value)
})

test_that("treeMerch handles a Vol_Type column already in data", {
  trees_vt <- dplyr::left_join(trees, price[, c('Product_Type', 'Vol_Type')],
                               by = 'Product_Type')
  vol_data <- treeVolume(data = trees_vt, dbh = DBH, mht = Height,
                         type = Vol_Type, gfc = GFC)
  expect_no_warning(result <- treeMerch(data = vol_data, pricing = price, volume = volume))
  expect_false(any(c('Vol_Type.x', 'Vol_Type.y') %in% colnames(result)))
  expect_equal(result$Value, treeMerch(data = trees, pricing = price)$Value)
  # Upper/lower case differences are not treated as conflicts
  trees_vt$Vol_Type <- toupper(trees_vt$Vol_Type)
  expect_no_warning(treeMerch(data = trees_vt, pricing = price))
  # Conflicting values warn and the pricing values are used
  trees_vt$Vol_Type <- 'doyle'
  expect_warning(result <- treeMerch(data = trees_vt, pricing = price),
                 'values that differ from pricing')
  expect_equal(result$Vol_Type, price$Vol_Type)
})
