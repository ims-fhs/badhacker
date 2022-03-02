#' Create a graphNEL object
#'
#' Create a graphNEL object from the functional structure.
#' Needs install.packages("BiocManager") and
#' BiocManager::install("graph")
#' BiocManager::install("Rgraphviz")
#'
#' @param my_structure A list, the functional structure calculated in
#' create_list_of_functional_structure().
#'
#' @return A graphNEL object
#' @export
#'
#' @examples
#' my_structure <- create_list_of_functional_structure(filename, path)
#' my_graph <- create_graphNEL_object(my_structure)
#' attrs <- list(node=list(fontsize = 8, shape="ellipse", fixedsize=FALSE,
#'                         label="foo"),
#'               edge=list(minlen = 2),
#'               graph=list(rankdir="LR"))
#' Rgraphviz::plot(my_graph, attrs=attrs)
#' Rgraphviz::plot(my_graph, "neato", attrs=attrs)
#' Rgraphviz::plot(my_graph, "twopi", attrs=attrs)
create_graphNEL_object <- function(my_structure) {
  nodes <- names(my_structure)
  n0 <- length(nodes)
  list_edges <- vector("list", length=n0)
  names(list_edges) <- nodes # initial list of edges, named by node names.

  # Construct edges from files_uuid and parent_uuid
  for (i in 1:n0) {
    own_parent <- my_structure[[i]]$calls
    parent_of <- get_parents_of(nodes[i], my_structure)
    edges_vec <- c(own_parent, parent_of) # bidirestional
    edges_vec <- c(own_parent)
    list_edges[[i]] <- list(edges=edges_vec, weights=rep(1, length(edges_vec)))
  }
  # Package ‘graph’ was removed from the CRAN repository.
  # Formerly available versions can be obtained from the archive.
  # This package is now available from Bioconductor only, see <http://www.bioconductor.org/packages/release/bioc/html/graph.html>.
  # if (!requireNamespace("BiocManager", quietly = TRUE))
  #   install.packages("BiocManager")
  # BiocManager::install("graph")
  my_graph <- graph::graphNEL(nodes=nodes, edgeL=list_edges, edgemode = "directed")
  return(my_graph)
}

