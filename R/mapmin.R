# mapping function for a vector returning the minimum value
mapMin <- function(x) {
  result <- min(x, na.rm=TRUE)
  if (is.infinite(result)) {
    result <- NA
  }
  return(result)
}
