% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functional-structure.R
\name{calculate_dependent_functions}
\alias{calculate_dependent_functions}
\title{Calculate which functions occur in a segment of code}
\usage{
calculate_dependent_functions(function_names, lines_of_code)
}
\arguments{
\item{function_names}{An array of characters, the functions' names.}

\item{lines_of_code}{}
}
\value{
dependent_functions
}
\description{
Function is tricky as neither match nor grep alone do the job:
}
