#' Add two step functions
#'
#' One of f1 or f2 might be numeric
#'
#' @param f1 A step function or a numeric
#' @param f2 A step function or a numeric
#'
#' @return A stepfun
#' @export
'+.stepfun' <- function(f1, f2) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))

  if (is.stepfun(f1) & is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x2 <- get("x", envir = environment(f2))
    x <- sort(unique(c(x1, x2)))
    y <- f1(c(x[1] - 1, x)) + f2(c(x[1] - 1, x))
  } else if (is.stepfun(f1) & !is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x <- x1
    y <- f1(c(x[1] - 1, x)) + f2
  } else if (!is.stepfun(f1) & is.stepfun(f2)) {
    x2 <- get("x", envir = environment(f2))
    x <- x2
    y <- f1 + f2(c(x[1] - 1, x))
  }
  return(stepfun(x = x, y = y))
}

#' Multiply two step functions
#'
#' One of f1 or f2 might be numeric
#'
#' @param f1 A step function or a numeric
#' @param f2 A step function or a numeric
#'
#' @return A stepfun
#' @export
'*.stepfun' <- function(f1, f2) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))

  if (is.stepfun(f1) & is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x2 <- get("x", envir = environment(f2))
    x <- sort(unique(c(x1, x2)))
    y <- f1(c(x[1] - 1, x)) * f2(c(x[1] - 1, x))
  } else if (!is.stepfun(f1) & is.stepfun(f2)) {
    x <- get("x", envir = environment(f2))
    y <- f1 * f2(c(x[1] - 1, x))
  } else if (is.stepfun(f1) & !is.stepfun(f2)) {
    x <- get("x", envir = environment(f1))
    y <- f1(c(x[1] - 1, x)) * f2
  }
  return(stepfun(x = x, y = y))
}

#' Subtract two step functions
#'
#' One of f1 or f2 might be numeric
#'
#' @param f1 A step function or a numeric
#' @param f2 A step function or a numeric
#'
#' @return
#' @export
'-.stepfun' <- function(f1, f2) {
  y <- f1 + (-1)*f2
  return(y)
}

#' Prune step functions
#'
#' Remove x- and y-values from step functions, where diff(y) does not change
#'
#' @param f A stepfun
#'
#' @return A stepfun
#' @export
prune_stepfun <- function(f) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f)))

  y <- c(get("yleft", envir = environment(f)),
         get("y", envir = environment(f)))
  ind_dups <- which(diff(y) == 0)
  if (length(ind_dups) > 0) {
    x <- get("x", envir = environment(f))
    x <- x[-ind_dups]
    if (length(x) == 0) {
      res <- stepfun(x = 0, y = c(0, 0))
    } else {
      y <- y[-(ind_dups+1)]
      res <- stepfun(x = x, y = y)
    }
  } else {
    res <- f
  }
  return(res)
}

#' Shift stepfun by dx in x direction
#'
#' @param f A stepfun
#' @param dx A numeric
#'
#' @return The shifted stepfun
shift_stepfun <- function(f, dx) {
  x <- get("x", envir = environment(f))
  y <- c(get("yleft", envir = environment(f)),
         get("y", envir = environment(f)))
  xnew <- x + dx
  return(stepfun(x = xnew, y = y))
}

'/.stepfun' <- function(f1, f2) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))

  if (is.stepfun(f1) & is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x2 <- get("x", envir = environment(f2))
    x <- sort(unique(c(x1, x2)))
    y <- f1(c(x[1] - 1, x)) / f2(c(x[1] - 1, x))
  } else if (is.stepfun(f1) & !is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x <- x1
    y <- f1(c(x[1] - 1, x)) / f2
  } else if (!is.stepfun(f1) & is.stepfun(f2)) {
    x2 <- get("x", envir = environment(f2))
    x <- x2
    y <- f1 / f2(c(x[1] - 1, x))
  }
  return(stepfun(x = x, y = y))
}

