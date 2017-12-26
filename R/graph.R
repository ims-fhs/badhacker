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

get_nodes_from_structure <- function(my_structure) {
  nodes <- data.frame(names=names(my_structure), stringsAsFactors = F)
  nodes$id <- c(1:nrow(nodes))
  return(nodes)
}

get_edges_from_structure <- function(my_strcuture) {
  nodes <- get_nodes_from_structure(my_structure)
  n0 <- nrow(nodes)
  nodes_int <- c(1:n0)
  list_edges <- vector("list", length=n0)
  names(list_edges) <- nodes$names # initial list of edges, named by node names.
  called_functions <- unlist(lapply(c(1:length(my_structure)), function(i)
    ifelse(length(my_structure[[i]]$calls)==0, "", my_structure[[i]]$calls)))
  ll <- lapply(c(1:length(my_structure)), function(i) my_structure[[i]]$calls)
  # Construct edges from files_uuid and parent_uuid
  for (i in 1:n0) {
    own_parent <- my_structure[[i]]$calls
    parent_of <- get_parents_of(nodes$names[i], my_structure)
    edges_vec <- c(own_parent, parent_of) # bidirestional
    edges_vec <- c(own_parent)
    list_edges[[i]] <- list(edges=edges_vec)
  }
  list_df <- data.frame()
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
  df <- data.frame(from_name=rep(nodes$names, multiplicity),
                   from_id=rep(nodes$id, multiplicity),
                   to_name=parent_function, stringsAsFactors = F)
  df <- df[complete.cases(df), ]
  df$to_id <- unlist(lapply(c(1:nrow(df)), function(i) nodes$id[nodes$names == df$to_name[i]]))
  return(df)
}

functional_struture2graph <- function(my_structure) {
  nodes <- names(my_structure)
  n0 <- length(nodes)
  nodes_int <- c(1:n0)
  list_edges <- vector("list", length=n0)
  names(list_edges) <- nodes # initial list of edges, named by node names.
  called_functions <- unlist(lapply(c(1:length(my_structure)), function(i)
    ifelse(length(my_structure[[i]]$calls)==0, "", my_structure[[i]]$calls)))
  ll <- lapply(c(1:length(my_structure)), function(i) my_structure[[i]]$calls)
  # Construct edges from files_uuid and parent_uuid
  for (i in 1:n0) {
    own_parent <- my_structure[[i]]$calls
    parent_of <- get_parents_of(nodes[i], my_structure)
    edges_vec <- c(own_parent, parent_of) # bidirestional
    edges_vec <- c(own_parent)
    list_edges[[i]] <- list(edges=edges_vec, weights=rep(1, length(edges_vec)))
  }
  my_graph <- graph::graphNEL(nodes=nodes, edgeL=list_edges, edgemode = "directed")
  return(my_graph)
}


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
# visNetwork(nodes, edges, height = "100%", width = "100%") %>%
#   visOptions(highlightNearest = TRUE) %>%
#   visLayout(hierarchical = TRUE) %>%
#   visInteraction(navigationButtons = TRUE)

