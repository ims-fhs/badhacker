#' Calculate which functions occur in a segment of code
#'
#' Function is tricky as neither match nor grep alone do the job:
#'
#' @param function_names An array of characters, the functions' names.
#' @param lines_of_code
#'
#' @return dependent_functions
calculate_dependent_functions <- function(function_names, lines_of_code) {
  dependent_functions <- unlist(lapply(function_names, function(x) {
    # any(grepl(x, lines_of_code))
    matches <- gregexpr(x, lines_of_code)
    matches_df <- data.frame(line = numeric(), start = numeric(), end = numeric())
    for (i in 1:length(matches)) {
      for (j in 1:length(matches[[i]])) {
        my_match <- data.frame(line = i,
                               start = matches[[i]][j],
                               end = matches[[i]][j] + attributes(matches[[i]])$match.length[j])
        matches_df <- rbind(matches_df, my_match)
      }
    }
    matches_df <- matches_df[matches_df$start != -1,]
    cond_no_bracket <- substr(lines_of_code[matches_df$line], matches_df$end, matches_df$end) == "("
    cond_no_character_before <- !grepl("[a-zA-Z0-9_]",
                                       substr(lines_of_code[matches_df$line],
                                              matches_df$start-1,
                                              matches_df$start-1))
    any(cond_no_bracket & cond_no_character_before)
  }))
  return(dependent_functions)
}

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

#' Calculate the index of lines where functions start and stop.
#'
#' @param lines An array of characters, representing the lines of code
#' @param n_level An integer, 0 if a definition of a function inside a function
#' is forbidden.
#'
#' @return A list, the start and stop indicies
calculate_index_of_start_and_stop_lines <- function(lines, n_level = 0L) {
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

#' Create a list of the functional structure
#'
#' The list contains all important information (the start index, the stop index,
#' the names of dependent functions, the arguments, the corresponding defaults,
#' the file name and the path to the file).
#'
#' @param filename A character, the file name(s)
#' @param path A character, the path to the file(s)
#'
#' @return A list
#' @export
#'
#' @examples
#' my_structure <- create_list_of_functional_structure(filename, path)
create_list_of_functional_structure <- function(filename, path) {
  if (length(filename) == 1 & length(path) == 1) {
    lines <- readLines(paste0(path, filename), encoding = "UTF-8")
    filename_origin_file <- rep(filename, length(lines))
    path_origin_file <- rep(path, length(lines))
    original_filename_line <- c(1:length(lines))
  } else {
    assertthat::assert_that(length(filename) == length(path))
    for (i in 1:length(filename)) {
      if (i == 1) {
        lines <- readLines(paste0(path[i], filename[i]), encoding = "UTF-8")
        filename_origin_file <- rep(filename[i], length(lines))
        path_origin_file <- rep(path[i], length(lines))
        original_filename_line <- c(1:length(lines))
      } else {
        loop_lines <- readLines(paste0(path[i], filename[i]), encoding = "UTF-8")
        lines <- c(lines, loop_lines)
        filename_origin_file <- c(filename_origin_file, rep(filename[i], length(loop_lines)))
        path_origin_file <- c(path_origin_file, rep(path[i], length(loop_lines)))
        original_filename_line <- c(original_filename_line, 1:length(loop_lines))
      }
    }
  }
  merged_filename_line <- c(1:length(lines))

  cond_line_contains_function_def <- grepl(" <- function", lines)
  codelines_with_function_names <- strsplit(lines[cond_line_contains_function_def],
                                            " <- function", fixed = T)
  function_names <- unlist(lapply(c(1:length(codelines_with_function_names)),
                                  function(x) codelines_with_function_names[[x]][1]))
  function_arguments <- unlist(lapply(c(1:length(codelines_with_function_names)), function(x)
    gsub(" +|(\\()|(\\))|(\\{)", "", codelines_with_function_names[[x]][2])))
  list_of_function_arguments <- strsplit(function_arguments, ",")
  in_file <- filename_origin_file[cond_line_contains_function_def]
  path2file <- path_origin_file[cond_line_contains_function_def]

  n_level <- 0 # definition of function inside function is not considered at the moment
  r <- calculate_index_of_start_and_stop_lines(lines, n_level)
  ind_start_function <- r$ind_start_function; ind_stop_function <- r$ind_stop_function; rm(r)

  functional_structure <- list()
  for (i in 1:length(function_names)) {
    dependent_functions <- calculate_dependent_functions(
      function_names,
      lines[c((ind_start_function[i]+1):(ind_stop_function[i]-1))])
    r <- get_variables_defaults(list_of_function_arguments, i)
    args_names <- r$names; args_defaults <- r$defaults; rm(r)

    df <- list(start = ind_start_function[i], # function_names = function_names[i],
               stop = ind_stop_function[i],
               original_start = original_filename_line[
                 which(merged_filename_line == ind_start_function[i])],
               original_stop = original_filename_line[
                 which(merged_filename_line == ind_stop_function[i])],
               calls = function_names[dependent_functions],
               args = args_names,
               defaults = args_defaults,
               in_file = in_file[i],
               path2file = path2file[i])
    functional_structure[[function_names[i]]] <- df
  }
  return(functional_structure)
}

