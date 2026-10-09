library(testthat)
library(FIR)

data(vrpData)

plot_data <- data.frame(plotID = 1:8, Stand = rep(c('A', 'B'), 4),
                        domHeight = c(50, 57, 56, 45, 68, 78, 52, 66))

# standStock --------------------------------------------------------------

test_that("standStock accepts unquoted column names", {
  result <- standStock(treeData = vrpData, plotID = PointID, variable = TreeNum,
                       grpBy = c(Species, DIA_Class), plotType = 'variable',
                       BAF = 10, baColumn = BA_sq_ft)
  expect_named(result, c('longTable', 'wideTable'))
  result <- standStock(treeData = vrpData, plotID = PointID, variable = TreeNum,
                       grpBy = DIA_Class, plotType = 'variable',
                       BAF = 10, baColumn = BA_sq_ft)
  expect_s3_class(result, 'data.frame')
})

test_that("standStock accepts plotSize as an unquoted column name", {
  trees <- vrpData
  trees$PlotSize <- 0.1
  result <- standStock(treeData = trees, plotID = PointID, variable = TreeNum,
                       grpBy = Species, plotType = 'fixed', plotSize = PlotSize)
  expect_s3_class(result, 'data.frame')
})

test_that("standStock rejects quoted column names", {
  expect_error(standStock(treeData = vrpData, plotID = 'PointID', variable = TreeNum,
                          grpBy = Species, plotType = 'variable',
                          BAF = 10, baColumn = BA_sq_ft),
               'plotID must be the unquoted name of a column')
  expect_error(standStock(treeData = vrpData, plotID = PointID, variable = TreeNum,
                          grpBy = c('Species', 'DIA_Class'), plotType = 'variable',
                          BAF = 10, baColumn = BA_sq_ft),
               'grpBy must be the unquoted name of a column')
  expect_error(standStock(treeData = vrpData, plotID = PointID, variable = TreeNum,
                          grpBy = Species, plotType = 'variable',
                          BAF = 10, baColumn = 'BA_sq_ft'),
               'baColumn must be the unquoted name of a column')
})

test_that("standStock stops when a column name is not in treeData", {
  expect_error(standStock(treeData = vrpData, plotID = PointID, variable = TreeNum,
                          grpBy = c(Species, DBH_Class), plotType = 'variable',
                          BAF = 10, baColumn = BA_sq_ft),
               'column "DBH_Class" supplied to grpBy is not in treeData')
})

test_that("standStock stops when treeData is not a data frame", {
  expect_error(standStock(treeData = 1:10, plotID = PointID, variable = TreeNum,
                          grpBy = Species, plotType = 'variable',
                          BAF = 10, baColumn = BA_sq_ft),
               'treeData must be a data frame')
})

# calcEsts ----------------------------------------------------------------

test_that("calcEsts accepts unquoted column names", {
  result <- calcEsts(treeData = vrpData, plotID = PointID, variable = TreeNum,
                     grpBy = DIA_Class, plotType = 'variable', BAF = 10,
                     baColumn = BA_sq_ft)
  expect_equal(nrow(result), length(unique(vrpData$DIA_Class)))
  result <- calcEsts(treeData = vrpData, plotID = PointID, variable = TreeNum,
                     plotType = 'variable', BAF = 10, baColumn = BA_sq_ft)
  expect_equal(nrow(result), 1)
})

test_that("calcEsts gives the same estimates whether grpBy uses c() or not", {
  r1 <- calcEsts(treeData = vrpData, plotID = PointID, variable = TreeNum,
                 grpBy = DIA_Class, plotType = 'variable', BAF = 10,
                 baColumn = BA_sq_ft)
  r2 <- calcEsts(treeData = vrpData, plotID = PointID, variable = TreeNum,
                 grpBy = c(DIA_Class), plotType = 'variable', BAF = 10,
                 baColumn = BA_sq_ft)
  expect_identical(r1, r2)
})

test_that("calcEsts rejects quoted column names", {
  expect_error(calcEsts(treeData = vrpData, plotID = PointID, variable = 'TreeNum',
                        plotType = 'variable', BAF = 10, baColumn = BA_sq_ft),
               'variable must be the unquoted name of a column')
  expect_error(calcEsts(treeData = vrpData, plotID = PointID, variable = TreeNum,
                        plotType = 'variable', BAF = 10, baColumn = BA_sq_ft,
                        standID = 'Species'),
               'standID must be the unquoted name of a column')
})

test_that("calcEsts stops when a column name is not in treeData", {
  expect_error(calcEsts(treeData = vrpData, plotID = PointID, variable = TreeNum,
                        plotType = 'variable', BAF = 10, baColumn = BA),
               'column "BA" supplied to baColumn is not in treeData')
})

# importanceValue ---------------------------------------------------------

test_that("importanceValue accepts unquoted column names", {
  result <- importanceValue(treeData = vrpData, plotID = PointID,
                            baColumn = BA_sq_ft, species = Species)
  expect_equal(nrow(result), length(unique(vrpData$Species)))
  expect_named(result, c('Species', 'frequency', 'abundance', 'dominance', 'importance'))
})

test_that("importanceValue rejects quoted column names", {
  expect_error(importanceValue(treeData = vrpData, plotID = PointID,
                               baColumn = BA_sq_ft, species = 'Species'),
               'species must be the unquoted name of a column')
})

test_that("importanceValue stops when a column name is not in treeData", {
  expect_error(importanceValue(treeData = vrpData, plotID = PointID,
                               baColumn = BA_sq_ft, species = Spp),
               'column "Spp" supplied to species is not in treeData')
})

# standEsts ---------------------------------------------------------------

test_that("standEsts accepts unquoted column names", {
  result <- standEsts(plotData = plot_data, variable = domHeight)
  expect_equal(result$estimate, mean(plot_data$domHeight))
  result <- standEsts(plotData = plot_data, variable = domHeight, standID = Stand)
  expect_equal(result$estimate,
               as.numeric(tapply(plot_data$domHeight, plot_data$Stand, mean)))
})

test_that("standEsts rejects quoted column names", {
  expect_error(standEsts(plotData = plot_data, variable = 'domHeight'),
               'variable must be the unquoted name of a column')
  expect_error(standEsts(plotData = plot_data, variable = domHeight, grpBy = 'Stand'),
               'grpBy must be the unquoted name of a column')
})

test_that("standEsts stops when a column name is not in plotData", {
  expect_error(standEsts(plotData = plot_data, variable = height),
               'column "height" supplied to variable is not in plotData')
})

test_that("standEsts stops when plotData is not a data frame", {
  expect_error(standEsts(plotData = as.matrix(plot_data), variable = domHeight),
               'plotData must be a data frame')
})
