# create a counter function for row and column index generation
make.counter <- function(x) {
  i <- 0
  function() {
    i <<- i+1
    i
  }
}
