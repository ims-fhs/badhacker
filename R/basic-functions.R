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

