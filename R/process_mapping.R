process.mapping <- function(data, mapping, mapFunction) {
  for (row in 1:nrow(mapping)) {
    mapName <- mapping[row, 1]
    mapStartCol <- mapping[row, 2]
    mapEndCol <- mapping[row, 3]
    map = function() {
      apply(data[mapStartCol:mapEndCol], 1, mapFunction)
    }

    data <- dplyr::mutate(data, mapped_ = map())
    names(data)[names(data)=="mapped_"] <- mapName
  }
  # get the mutated colums
  startCol <- ncol(data) - nrow(mapping) + 1
  endCol <- ncol(data)
  return(data[startCol:endCol])
}
