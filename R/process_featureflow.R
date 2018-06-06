process.featureflow <- function(data, workflow) {
  items <- items.from(import)
  featureFlow <- matrix(nrow = count.items(items), ncol = count.states(workflow), dimnames = list(items, names(workflow)))

  rowCounter <- make.counter()
  colCounter <- make.counter()

#
# Daten spalten- und zeilenweise durchlaufen und Datum f??r Statuswechsel eintragen
#
# Hier wird zun??chst ??ber alle Features iteriert (spaltenweise) und dann ??ber die Zeilen.
# F??r jede Zustands??nderung des Features wird das Datum aus der 1. Spalte der importierten Daten ??bernommen.
#
  null <- apply(import, 2,  function (k) {
    current.colId <- colCounter()
    # Die erste Spalte der importierten Daten enth??lt die Datumswerte und wird ignoriert
    if (current.colId > 1) {
      last_state <- ""
      vapply(k, function(l) {
        current.rowId <- rowCounter()
        if ("" != l  && last_state != l ) {
          day <- make.date(as.integer(data[current.rowId, 1]))
          # Die erste Spalte wird ignoriert, wegen der Datumswerte, deshalb hier colId - 1
          featureFlow[current.colId-1, workflow[l]] <<- day
        }
        last_state <<- l
      }, "")
      rowCounter <<- make.counter()
    }
  })

  return(featureFlow)
}
