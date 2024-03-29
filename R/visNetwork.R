#' Retrieve the parents of a node
#'
#' Each node represents a function defined in the code.
#'
#' @param node A data.frame, the nodes calculated in get_nodes_from_structure().
#' @param my_structure A list, the functional structure calculated in
#' create_list_of_functional_structure().
#'
#' @examples
#'
#' @return An array, the parent nodes
get_parents_of <- function(node, my_structure) {
  nodes <- names(my_structure)
  parents <- c()
  for (k in 1:length(my_structure)) {
    cond <- node %in% my_structure[[nodes[k]]]$calls
    if (cond) {
      parents <- c(parents, nodes[k])
    }
  }
  return(parents)
}

#' Calculate nodes
#'
#' @param my_structure A list, the functional structure calculated in
#' create_list_of_functional_structure().
#'
#' @return A data.frame, the nodes.
get_nodes_from_structure <- function(my_structure) {
  nodes <- data.frame(id = c(1:length(my_structure)),
                      # should be my_structure[i]$function_name check where names is used!!!
                      label = names(my_structure),
                      value = rep(0.3, length(my_structure)),
                      # tooltip (html or character), when the mouse is above
                      title = character(length(my_structure)),
                      stringsAsFactors = F)
  # add loop to create titles
  for (i in 1:nrow(nodes)) {
    args <- my_structure[[i]]$args
    args <- ifelse(args[1] == "", "-", paste(args , collapse = ", "))
    defaults <- my_structure[[i]]$defaults
    defaults <- ifelse(defaults[1] == "", "-", paste(defaults, collapse = ", "))
    nodes$title[i] <- paste0("<b>", names(my_structure)[i],"</b><br>",
                             "<b>args    :</b> ", args, "<br>",
                             "<b>defaults:</b> ", defaults, "<br>")
  }
  return(nodes)
}

#' Caluclate edges
#'
#' @param my_structure A list, the functional structure calculated in
#' create_list_of_functional_structure().
#'
#' @return A data.frame, the edges
get_edges_from_structure <- function(my_structure) {
  nodes <- get_nodes_from_structure(my_structure)
  n0 <- nrow(nodes)
  list_edges <- vector("list", length=n0)
  names(list_edges) <- nodes$label # initial list of edges, named by node names.

  for (i in 1:n0) {
    own_parent <- my_structure[[i]]$calls
    parent_of <- get_parents_of(nodes$label[i], my_structure)
    edges_vec <- c(own_parent, parent_of) # bidirestional
    edges_vec <- c(own_parent)
    list_edges[[i]] <- list(edges=edges_vec)
  }

  multiplicity <- unlist(lapply(c(1:length(list_edges)), function(i)
    ifelse(length(list_edges[[i]]$edges)==0, 1, length(list_edges[[i]]$edges))))
  parent_function <- c()
  for (i in 1:length(list_edges)) {
    if(length(list_edges[[i]]$edges)==0) {
      parent_function <- c(parent_function, NA)
    } else {
      parents <- unlist(lapply(c(1:length(list_edges[[i]]$edges)), function(k) list_edges[[i]]$edges[k]))
      parent_function <- c(parent_function, parents)
    }
  }
  df <- data.frame(from_name=rep(nodes$label, multiplicity),
                   from_id=rep(nodes$id, multiplicity),
                   to_name=parent_function, stringsAsFactors = F)
  df <- df[complete.cases(df), ]
  df$to_id <- unlist(lapply(c(1:nrow(df)), function(i) nodes$id[nodes$label == df$to_name[i]]))
  return(df)
}

#' Display a graph using visNetwork of the functional structure.
#'
#' Needs install.packages("BiocManager") and
#' BiocManager::install("graph")
#' BiocManager::install("Rgraphviz")
#' @param my_structure A list, the functional structure calculated in
#' create_list_of_functional_structure().
#' @import visNetwork
#'
#' @return A list
#' @export
#'
#' @examples
#' library(badhacker)
#'
#' # Either
#' filename <- "test-optimize911-onefile.R"
#' path <- "./data/"
#' # OR
#' path <- "./data/Roptimize/"
#' filename <- list.files(path)
#' path <- rep(path, length(filename))
#'
#' my_structure <- badhacker::create_list_of_functional_structure(filename, path)
#' nodes <- badhacker:::get_nodes_from_structure(my_structure)
#' functional_relations <- badhacker:::get_edges_from_structure(my_structure)
#' edges <- data.frame(from = functional_relations$from_id,
#'                     to = functional_relations$to_id,
#'                     arrows = rep("to", nrow(functional_relations)),
#'                     stringsAsFactors = FALSE)
#' my_graph <- visNetwork(nodes, edges) %>%
#'   visOptions(highlightNearest = TRUE) %>%
#'   visInteraction(navigationButtons = TRUE) %>%
#'   visIgraphLayout()
#' my_graph
#'
#' my_structure <- badhacker::create_list_of_functional_structure(filename, path)
#' badhacker::visualize_functional_structure(my_structure)
visualize_functional_structure <- function(my_structure) {
  nodes <- get_nodes_from_structure(my_structure)
  functional_relations <- get_edges_from_structure(my_structure)
  edges <- data.frame(from = functional_relations$from_id,
                      to = functional_relations$to_id,
                      arrows = rep("to", nrow(functional_relations)),
                      stringsAsFactors = FALSE)

  my_graph <- visNetwork(nodes, edges, height = "100%", width = "100%") %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(hierarchical = TRUE) %>%
    visInteraction(navigationButtons = TRUE)
  return(my_graph)
}
