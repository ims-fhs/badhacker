fun_circle1 <- function() {
  fun_circle2()
  return(NULL)
}

fun_circle2 <- function() {
  fun_circle3()
  return(NULL)
}

fun_circle3 <- function() {
  fun_circle4()
  return(NULL)
}

fun_circle4 <- function() {
  fun_circle1()
  return(NULL)
}
