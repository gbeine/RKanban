process.featurestates <- function(data, workflow) {
  dates <- dates.from(data)
  featureStates <- matrix(nrow = count.days(dates), ncol = count.states(workflow), dimnames = list(dates, names(workflow)))

  rowCounter <- make.counter()
  colCounter <- make.counter()

  null <- apply(featureStates, 1, function (k) {
    current.rowId <- rowCounter()
    vapply(k, function(l) {
      current.colId <- colCounter()
      current.colName <- colnames(featureStates)[current.colId]
      current.count <- sum(!is.na(data[current.rowId,]) & data[current.rowId,] == current.colName)
      featureStates[current.rowId, current.colId] <<- current.count
    }, 0)
    colCounter <<- make.counter()
  })

  return(featureStates)
}
