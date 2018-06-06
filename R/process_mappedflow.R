process.mappedflow <- function(flowData, mapping) {
  data <- as.data.frame(flowData)
  # keep the row names in result
  result <- process.mapping(data, mapping, mapMin)
  row.names(result) <- row.names(flowData)
  return(result)
}
