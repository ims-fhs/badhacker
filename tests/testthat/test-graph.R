context("graphs")
test_that("test graphs", {

  # browser()
  my_structure <- create_list_of_functional_structure(filename, path)
  my_graph <- create_graphNEL_object(my_structure)
  connected_components <- graph::connComp(my_graph)

  testthat::expect_equal(class(connected_components), "list")
  testthat::expect_equal(length(connected_components), 3)
  testthat::expect_equal(connected_components[[1]], "# cool_fun")
  testthat::expect_equal(connected_components[[2]], c("mysum", "mydot", "myfun"))
  testthat::expect_equal(connected_components[[3]], "stupid_fun")

  df <- visualize_functional_structure(my_structure)
  testthat::expect_equal(df$x$nodes$label,
                         c("# cool_fun", "mysum", "mydot", "myfun", "stupid_fun"))
  testthat::expect_equal(names(df),
                         c("x", "width", "height", "sizingPolicy", "dependencies",
                           "elementId", "preRenderHook", "jsHooks"))
  testthat::expect_equal(df$x$edges$from, c(4, 4))
  testthat::expect_equal(df$x$edges$to, c(2, 3))
})


