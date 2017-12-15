context("graphs")
test_that("test graphs", {

  # browser()
  my_structure <- get_functional_structure(filename, path)
  my_graph <- functional_struture2graph(my_structure)
  connected_components <- graph::connComp(my_graph)

  testthat::expect_equal(class(connected_components), "list")
  testthat::expect_equal(length(connected_components), 3)
  testthat::expect_equal(connected_components[[1]], "# cool_fun")
  testthat::expect_equal(connected_components[[2]], c("mysum", "mydot", "myfun"))
  testthat::expect_equal(connected_components[[3]], "stupid_fun")

  # # expect_equal(length(file_list), 6)
  # # expect_equal(length(file_list), length(file_list_source))
  #
  # # attrs <- list(node=list(fontsize = 8, shape="ellipse", fixedsize=FALSE,
  # #                         label="foo"), #, fillcolor="lightgreen"),
  # #           edge=list(minlen = 2), #, color="cyan"),
  # #           graph=list(rankdir="LR"))
  # # # Users that want to create a clickable graph renderings with drill-down
  # # # capability should see the imageMap function in the biocGraph package.
  # # Rgraphviz::plot(my_graph, attrs=attrs)
  # # Rgraphviz::plot(my_graph, "neato", attrs=attrs)
  # # Rgraphviz::plot(my_graph, "twopi", attrs=attrs)
})


