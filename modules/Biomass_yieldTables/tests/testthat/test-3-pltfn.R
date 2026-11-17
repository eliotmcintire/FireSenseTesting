test_that("function pltfn works", {
  #####
  # dummy inputs
  ngroup <- 3
  nage <- 5
  nsp <- 2
  AGB <- data.table::as.data.table(
    expand.grid(yieldTableIndex = c(1:ngroup),
                age = c(1:nage),
                speciesCode = as.factor(c(1:nsp)))
  )
  AGB$biomass <- AGB$age
  #####
  
  # general
  expect_true(inherits(pltfn(AGB, 3), what = "ggplot"))
  expect_error(pltfn())
  expect_error(pltfn(AGB, 0))
  expect_invisible(pltfn(AGB, 2))
  expect_message(pltfn(AGB, 10))
  expect_no_error(pltfn(AGB, 1))
  
  # number of facets
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, 2))$data[[1]]$PANEL)), 2)
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, 10))$data[[1]]$PANEL)), ngroup)
})
