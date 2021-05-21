insert_line <- function(x, index, insert) {
  if (index >= length(x)) {x <- c(x,insert)}
  if (index == 1) {x <- c(insert,x)}
  if (index <= length(x)) {
    x <- c(x[1:(index-1),insert,x[(index+1):length(x)]])
  }
  x
}