# mapping function for a vector returning the sum
mapSum <- function(x) {
  result <- sum(x, na.rm=TRUE)
  if (is.infinite(result)) {
    result <- NA
  }
  return(result)
}
