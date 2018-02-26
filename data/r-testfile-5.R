my_fun1 <- function(x, y) {
  return(c(2*x, 2*y))
}

my_fun2 <- function(x, y) {
  cond_my_fun1 <- my_fun1(x, y) # similar names in one line!
  return(cond_my_fun1)
}
