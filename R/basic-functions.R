calculate_number_of_open_braces <- function(lines_of_code) {
  open <- grepl("\\{+", lines_of_code)
  close <- grepl("\\}+", lines_of_code)
  cond_close <- open & close

  # Lines with open and closed braces in the same line are ignored.
  open[cond_close] <- F
  close[cond_close] <- F
  open_braces <- cumsum(open) - cumsum(close)
  return(open_braces)
}

get_variables_defaults <- function(list_of_function_arguments, i) {
  if (length(list_of_function_arguments[[i]]) == 0) {
    vars_name <- ""
    vars_default <- ""
  } else {
    vars <- strsplit(list_of_function_arguments[[i]], "=")
    vars_name <- unlist(lapply(c(1:length(vars)), function(x) vars[[x]][1]))
    vars_default <- unlist(lapply(c(1:length(vars)), function(x) vars[[x]][2]))
  }
  return(list(names=vars_name, defaults=vars_default))
}

create_list_of_functional_structure <- function(filename, path) {
  lines <- readLines(paste0(path, filename), encoding = "UTF-8")

  codelines_with_function_names <- strsplit(lines[grepl(" <- function", lines)],
                                            " <- function", fixed = T)
  function_names <- unlist(lapply(c(1:length(codelines_with_function_names)),
                                  function(x) codelines_with_function_names[[x]][1]))

  open_braces <- calculate_number_of_open_braces(lines)
  n_level <- 0 # definition of function inside function is not considered at the moment
  ind_start_function <- which(open_braces == n_level &
                                imsbasics::shift_array(open_braces, -1, 0) == n_level+1) + 1
  ind_stop_function <- which(open_braces == n_level+1 &
                               imsbasics::shift_array(open_braces, -1, 0) == n_level) + 1

  function_arguments <- unlist(lapply(c(1:length(codelines_with_function_names)), function(x)
    gsub(" +|(\\()|(\\))|(\\{)", "", codelines_with_function_names[[x]][2])))
  list_of_function_arguments <- strsplit(function_arguments, ",")

  functional_structure <- list()
  for (i in 1:length(function_names)) {
    dependent_functions <- unlist(lapply(function_names[-i], function(x)
      any(grepl(x, lines[c(ind_start_function[i]:ind_stop_function[i])]))))

    r <- get_variables_defaults(list_of_function_arguments, i)
    args_names <- r$names; args_defaults <- r$defaults; rm(r)

    df <- list(start = ind_start_function[i],
               stop = ind_stop_function[i],
               calls = function_names[-i][dependent_functions],
               args = args_names,
               defaults = args_defaults,
               in_file = file,
               path2file = path)
    functional_structure[[function_names[i]]] <- df
  }
  return(functional_structure)
}

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

