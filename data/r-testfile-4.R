mysum <- function(x,y,a) {
  if (x > 0) { vals <- "x > 0" }
  if (y > 0) {
    # print("y > 0")
  }
  return(x+y)
}

similar_mysum <- function(x,y,b=0,c=4) {
  # comment (()) and  [[]] in one line allow, but nothing else...
  # z <- cool_fun()
  # x <-#function in comments are considered as calls
  return(x*y)
}

similarmysum <- function() {
  return(0)
}

myfun <- function(x,y) {
  res0 <- similar_mysum(x,y)
  res <- mysum2(x,res0)
  similarmysum()
  return(res)
}

mysum2 <- function(x, hurz="hurz") {
  # just a similar name (like mysum)
  return(NULL)
}
