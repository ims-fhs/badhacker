#' Create array of indicies
#'
#' The value at each position increases (decreases) by one if a brace is opened (closed)
#' in the corresponding line of the code.
#'
#' @param lines_of_code An array, containing the full lines of code as character
#'
#' @return An array
calculate_number_of_open_braces <- function(lines_of_code) {
  open <- grepl("\\{+", lines_of_code)
  close <- grepl("\\}+", lines_of_code)
  cond_close <- open & close

  # Lines with open and closed braces in the same line are ignored.
  open[cond_close] <- F
  close[cond_close] <- F

  open_braces <- cumsum(open) - cumsum(close)
  assertthat::assert_that(open_braces[length(open_braces)] == 0)
  return(open_braces)
}

#' Get the variables and the corresponding default values
#'
#' Return the values for the i-th function with respect to the "array of function names".
#'
#' @param list_of_function_arguments An array of characters, the functions' names
#' @param i An integer, the index
#'
#' @return A list containing the variables names and the default values.
get_variables_defaults <- function(list_of_function_arguments, i) {
  if (length(list_of_function_arguments[[i]]) == 0) {
    vars_name <- ""
    vars_default <- ""
  } else {
    vars <- strsplit(list_of_function_arguments[[i]], "=")
    vars_name <- unlist(lapply(c(1:length(vars)), function(x) vars[[x]][1]))
    vars_default <- unlist(lapply(c(1:length(vars)), function(x) vars[[x]][2]))
  }
  return(list(names = vars_name, defaults = vars_default))
}

#' Create a list of the functional structure
#'
#' The list contains all important information (the start index, the stop index,
#' the names of dependent functions, the arguments, the corresponding defaults,
#' the file name and the path to the file).
#'
#' @param filename A character, the file name
#' @param path A character, the path to the file
#'
#' @return A list
#' @export
#'
#' @examples
#' my_structure <- create_list_of_functional_structure(filename, path)
create_list_of_functional_structure <- function(filename, path) {
  lines <- readLines(paste0(path, filename), encoding = "UTF-8")

  codelines_with_function_names <- strsplit(lines[grepl(" <- function", lines)],
                                            " <- function", fixed = T)
  function_names <- unlist(lapply(c(1:length(codelines_with_function_names)),
                                  function(x) codelines_with_function_names[[x]][1]))

  n_level <- 0 # definition of function inside function is not considered at the moment
  calculate_index_of_start_and_stop_lines <- function(lines, n_level) {
    open_braces <- calculate_number_of_open_braces(lines)
    ind_start_function <- which(open_braces == n_level &
                                  imsbasics::shift_array(open_braces, -1, 0) == n_level+1) + 1
    if (open_braces[1] >= 1) {
      ind_start_function <- c(1, ind_start_function)
    }
    ind_stop_function <- which(open_braces == n_level+1 &
                                 imsbasics::shift_array(open_braces, -1, 0) == n_level) + 1
    return(list(ind_start_function = ind_start_function,
                ind_stop_function = ind_stop_function))
  }
  r <- calculate_index_of_start_and_stop_lines(lines, n_level)
  ind_start_function <- r$ind_start_function; ind_stop_function <- r$ind_stop_function; rm(r)

  function_arguments <- unlist(lapply(c(1:length(codelines_with_function_names)), function(x)
    gsub(" +|(\\()|(\\))|(\\{)", "", codelines_with_function_names[[x]][2])))
  list_of_function_arguments <- strsplit(function_arguments, ",")

  functional_structure <- list()
  for (i in 1:length(function_names)) {
    # address recursion:
    dependent_functions <- unlist(lapply(function_names, function(x)
      any(grepl(x, lines[c((ind_start_function[i]+1):(ind_stop_function[i]-1))]))))

    r <- get_variables_defaults(list_of_function_arguments, i)
    args_names <- r$names; args_defaults <- r$defaults; rm(r)

    df <- list(start = ind_start_function[i],
               stop = ind_stop_function[i],
               calls = function_names[dependent_functions],
               args = args_names,
               defaults = args_defaults,
               in_file = filename,
               path2file = path)
    functional_structure[[function_names[i]]] <- df
  }
  return(functional_structure)
}

