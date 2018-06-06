process.mappedstates <- function(stateData, mapping) {
  data <- as.data.frame(stateData)
  # keep the row names in result
  result <- process.mapping(data, mapping, mapSum)
  row.names(result) <- row.names(stateData)
  return(result)
}
