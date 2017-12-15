# imsbasics::clc()

# cool_fun <- function() {
#   return(NULL)
# }

mysum <- function(x,y,a) {
  if (x > 0) { vals <- "x > 0" }
  if (y > 0) {
    # print("y > 0")
  }
  return(x+y)
}

mydot <- function(x,y,b=0,c=4) {
  # comment (()) and  [[]] in one line allow, but nothing else...
  # z <- cool_fun()
  # x <-#function in comments are considered as calls
  return(x*y)
}

myfun <- function(x,y) {
  return(mysum(x,mydot(x,y)))
}

stupid_fun <- function(x, hurz="hurz") {
  return(NULL)
}

mysum(1,2)
a <- mydot(2,3)
b <- myfun(2,3)
