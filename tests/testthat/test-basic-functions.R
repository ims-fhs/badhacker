context("basic functions")
test_that("test basic functions", {

  my_structure <- create_list_of_functional_structure(filename, path)

  testthat::expect_equal(length(my_structure), 5)
  testthat::expect_equal(class(my_structure[[1]]),  "list")
  testthat::expect_equal(names(my_structure[[1]]),
                             c("start", "stop", "calls", "args",
                               "defaults", "in_file", "path2file"))
  testthat::expect_equal(names(my_structure),
                             c("# cool_fun", "mysum", "mydot", "myfun", "stupid_fun"))

  testthat::expect_equal(my_structure[[4]]$calls, c("mysum", "mydot"))
  testthat::expect_equal(my_structure[[4]]$args, c("x", "y"))
  testthat::expect_true(all(is.na(my_structure[[4]]$defaults)))

  testthat::expect_equal(my_structure[[3]]$args, c("x", "y", "b", "c"))
  testthat::expect_equal(my_structure[[3]]$defaults, c(NA, NA, "0", "4"))
})
