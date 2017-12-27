helper_fun <- function(x) {
  print(x)
  return(NULL)
}

helper_fun2 <- function() {
  print("x > 1")
}

my_faculty <- function(x) {
  helper_fun(x)
  if (x >= 1) {
    helper_fun2()
    return(x * my_faculty(x-1))
  } else {
    return(1)
  }
}

