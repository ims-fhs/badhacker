context("basic functions")
test_that("test basic functions", {

  # browser()
  # my_structure <- get_functional_structure(filename, path)

  # my_graph <- functional_struture2graph(my_structure)

  # Rgraphviz::plot(my_graph) # ................................................ creates a pdf???
  # expect_equal(length(file_list), 6)
  # expect_equal(length(file_list), length(file_list_source))

  # attrs <- list(node=list(fontsize = 8, shape="ellipse", fixedsize=FALSE,
  #                         label="foo"), #, fillcolor="lightgreen"),
  #           edge=list(minlen = 2), #, color="cyan"),
  #           graph=list(rankdir="LR"))
  # # Users that want to create a clickable graph renderings with drill-down
  # # capability should see the imageMap function in the biocGraph package.
  # Rgraphviz::plot(my_graph, attrs=attrs)
  # Rgraphviz::plot(my_graph, "neato", attrs=attrs)
  # Rgraphviz::plot(my_graph, "twopi", attrs=attrs)

  # graph::connComp(my_graph)
  #
  # functions <- get_nodes_from_structure(my_structure)
  # nodes <- data.frame(id = functions$id,
  #                     label = functions$names,
  #                     value = rep(0.3, nrow(functions)),
  #                     # tooltip (html or character), when the mouse is above
  #                     title = paste0("<p><b>", functions$names,"</b><br>Node !</p>"))
  #
  # functional_relations <- get_edges_from_structure(my_strcuture)
  # edges <- data.frame(from = functional_relations$from_id, to = functional_relations$to_id,
  #                     arrows = rep("to", nrow(functional_relations)))
  #
  # visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  #   visOptions(highlightNearest = TRUE) %>%
  #   visLayout(hierarchical = TRUE) %>%
  #   visInteraction(navigationButtons = TRUE)
  #
  #
})
