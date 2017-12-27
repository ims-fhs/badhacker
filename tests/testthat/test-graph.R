context("graphs")
test_that("test graphs", {

  my_structure <- create_list_of_functional_structure(filename, path)
  my_graph <- create_graphNEL_object(my_structure)
  # Rgraphviz::plot(my_graph)
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

context("Handling of cycles")
test_that("test with cycles", {

  my_structure <- create_list_of_functional_structure(filename2, path)
  my_graph <- create_graphNEL_object(my_structure)
  # Rgraphviz::plot(my_graph)
  connected_components <- graph::connComp(my_graph)

  testthat::expect_equal(class(connected_components), "list")
  testthat::expect_equal(length(connected_components), 1)
  testthat::expect_equal(connected_components[[1]],
                         c("fun_circle1", "fun_circle2", "fun_circle3", "fun_circle4"))

  df <- visualize_functional_structure(my_structure)
  testthat::expect_equal(df$x$nodes$label,
                         c("fun_circle1", "fun_circle2", "fun_circle3", "fun_circle4"))
  testthat::expect_equal(df$x$edges$from, c(1:4))
  testthat::expect_equal(df$x$edges$to, c(2:4, 1))
})

context("Handling of recursion")
test_that("test with recursion", {

  my_structure <- create_list_of_functional_structure(filename3, path)
  my_graph <- create_graphNEL_object(my_structure)
  # Rgraphviz::plot(my_graph)
  connected_components <- graph::connComp(my_graph)

  testthat::expect_equal(class(connected_components), "list")
  testthat::expect_equal(length(connected_components), 1)
  testthat::expect_equal(connected_components[[1]],
                         c("helper_fun", "helper_fun2", "my_faculty"))

  df <- visualize_functional_structure(my_structure)
  testthat::expect_equal(df$x$nodes$label,
                         c("helper_fun", "helper_fun2", "my_faculty"))
  testthat::expect_equal(df$x$edges$from, rep(3,3))
  testthat::expect_equal(df$x$edges$to, c(1:3))
})

