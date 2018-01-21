context("basic functions")
test_that("test basic functions", {

  lines <- readLines(paste0(path, filename), encoding = "UTF-8")
  open_braces <- calculate_number_of_open_braces(lines)
  testthat::expect_equal(open_braces, c(0, 0, 1, 1, 0, 0, 1, 1, 2, 2, 1, 1, 0,
                                        0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1,
                                        1, 0, 0, 0, 0, 0))

  codelines_with_function_names <- strsplit(lines[grepl(" <- function", lines)],
                                            " <- function", fixed = T)
  function_arguments <- unlist(lapply(c(1:length(codelines_with_function_names)), function(x)
    gsub(" +|(\\()|(\\))|(\\{)", "", codelines_with_function_names[[x]][2])))
  list_of_function_arguments <- strsplit(function_arguments, ",")

  r <- get_variables_defaults(list_of_function_arguments, 1)
  testthat::expect_equal(r$names, "")
  testthat::expect_equal(r$defaults, "")

  r <- get_variables_defaults(list_of_function_arguments, 2)
  testthat::expect_equal(r$names, c("x", "y", "a"))
  testthat::expect_true(all(is.na(r$defaults)))

  r <- get_variables_defaults(list_of_function_arguments, 3)
  testthat::expect_equal(r$names, c("x", "y", "b", "c"))
  testthat::expect_equal(r$defaults, c(NA, NA, "0", "4"))
})

context("functional_structure")
test_that("test create_list_of_functional_structure", {
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

context("Handling similar names")
test_that("test functions with similar names and a function referenced in a comment", {

  my_structure <- create_list_of_functional_structure(filename4, path)
  my_graph <- create_graphNEL_object(my_structure)
  # Rgraphviz::plot(my_graph)
  connected_components <- graph::connComp(my_graph)

  testthat::expect_equal(class(connected_components), "list")
  testthat::expect_equal(length(connected_components), 2)
  testthat::expect_equal(connected_components[[1]], "mysum")
  testthat::expect_equal(connected_components[[2]],
                         c("similar_mysum", "similarmysum", "myfun", "mysum2"))
  testthat::expect_equal(my_structure$myfun$calls,
                         c("similar_mysum", "similarmysum", "mysum2")) # not mysum!
  testthat::expect_equal(length(my_structure$mysum2$calls), 0)
  testthat::expect_equal(length(my_structure$mysum$calls), 0)
  testthat::expect_equal(length(my_structure$similar_mysum$calls), 0)
  testthat::expect_equal(length(my_structure$similarmysum$calls), 0)
})

