context("basic functions")
test_that("test basic functions", {

  # browser()
  my_structure <- get_functional_structure(filename, path)

  testthat::expect_equal(length(my_structure), 5)
  testthat::expect_equal(class(my_structure[[1]]),  "list")
  testthat::expect_equal(names(my_structure[[1]]),
                             c("start", "stop", "calls", "args",
                               "defaults", "in_file", "path2file"))
  testthat::expect_equal(names(my_structure),
                             c("# cool_fun", "mysum", "mydot", "myfun", "stupid_fun"))
  # result my_structure[[1]]$in_file ???


  my_graph <- functional_struture2graph(my_structure)
})
