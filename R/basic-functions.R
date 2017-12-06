get_variables_defaults <- function(my_str, i) {
  if (length(my_str[[1]]) == 0) {
    vars_name <- ""
    vars_default <- ""
  } else {
    vars <- strsplit(my_str[[i]], "=")
    vars_name <- unlist(lapply(c(1:length(vars)), function(x) vars[[x]][1]))
    vars_default <- unlist(lapply(c(1:length(vars)), function(x) vars[[x]][2]))
  }
  return(list(name=vars_name, defaults=vars_default))
}

get_functional_structure <- function(filename, path) {
  lines <- readLines(paste0(path, filename), encoding = "UTF-8")

  fun_str <- " <- function"
  funs_str <- strsplit(lines[grepl(fun_str, lines)], fun_str, fixed = T)
  # contains names of functions
  funs <- unlist(lapply(c(1:length(funs_str)), function(x) funs_str[[x]][1]))

  functional_structure <- list()

  start <- "\\{+"
  stop <- "\\}+"
  open <- grepl(start, lines) # recurse? how to check for depth of a nested list?
  close <- grepl(stop, lines)
  cond_close <- open & close
  open[cond_close] <- F
  close[cond_close] <- F
  delta <- cumsum(open) - cumsum(close)

  n_level <- 0
  start_function <- which(delta == n_level & imsbasics::shift_array(delta, -1, 0) == n_level+1) + 1
  stop_function <- which(delta == n_level+1 & imsbasics::shift_array(delta, -1, 0) == n_level) + 1
  for (i in 1:length(funs)) {
    found_funs <- unlist(lapply(funs[-i], function(x)
      any(grepl(x, lines[c(start_function[i]:stop_function[i])]))))
    args <- unlist(lapply(c(1:length(funs_str)), function(x)
      gsub(" +|(\\()|(\\))|(\\{)", "", funs_str[[x]][2]))) # contains comma separated args without spaces
    my_str <- strsplit(args, ",")

    df <- list(start=start_function[i],
               stop=stop_function[i],
               calls=funs[-i][found_funs],
               args=get_variables_defaults(my_str, i)$name,
               defaults=get_variables_defaults(my_str, i)$defaults,
               in_file=file,
               path2file=path)
    functional_structure[[funs[i]]] <- df
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
  # browser()
  # nodes$args <- paste()
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

