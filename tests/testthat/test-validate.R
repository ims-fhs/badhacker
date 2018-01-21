context("test validate_code")
test_that("test validate code", {

  functional_structure <- create_list_of_functional_structure(filename2, path)
  # my_graph <- create_graphNEL_object(functional_structure)
  # Rgraphviz::plot(my_graph)
  testthat::expect_equal(validate_code(functional_structure), 0)

  functional_structure <- create_list_of_functional_structure(filename, path)
  testthat::expect_message(r <- validate_code(functional_structure))
  testthat::expect_equal(r, 1)

  functional_structure <- create_list_of_functional_structure(filename4, path)
  testthat::expect_message(r <- validate_code(functional_structure))
  testthat::expect_equal(r, 1)
})
