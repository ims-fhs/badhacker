#' Analyse code structure for critical issues
#'
#' This function analyses the code structure using the concept of graphs. At the
#' moment the function is a beta version
#'
#' @param functional_structure A list, the functional structure calculate from
#' create_list_of_functional_structure()
#'
#' @return flag An integer. 0 = everything ok, 1 = warnings, -1 = bugs.
#' @export
#'
#' @examples
#' functional_structure <- badhacker::create_list_of_functional_structure(filenames, paths)
#' validate_code(functional_structure)
validate_code <- function(functional_structure) {
  analysis_flag <- 0L
  graph_of_functional_structure <- badhacker::create_graphNEL_object(functional_structure)
  connected_components <- graph::connComp(graph_of_functional_structure)

  # Analysis of connected components:
  no_of_functions_in_conncomp <- unlist(lapply(c(1:length(connected_components)), function(i)
    length(connected_components[[i]])))
  unconnected_functions <- unlist(connected_components[no_of_functions_in_conncomp == 1])
  if (length(connected_components) > 1) { # & max(no_of_functions_in_conncomp)
    analysis_flag <- 1L
    message(paste0("The following functions seem to be not connected: ",
            paste(unconnected_functions, collapse = ", ")))
  }
  return(analysis_flag)
}
