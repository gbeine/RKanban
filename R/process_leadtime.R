process.leadtime <- function(data, from, to, minTime = 0) {
  df <- as.data.frame(data)

  startCol <- which(colnames(df)==to)
  endCol <- which(colnames(df)==from)

  map = function() {
    apply(df[startCol:endCol], 1, mapDifference)
  }

  df <- dplyr::mutate(df, mapped_ = map())
  names(df)[names(df)=="mapped_"] <- "Lead Time"

  df <- dplyr::filter(df, !is.na(df[to]), !is.na(df["Lead Time"]), df["Lead Time"] > minTime)

  mutate_call = lazyeval::interp(~as.Date(a, origin="1970-01-01"), a = as.name(to))
  df <- dplyr::mutate_(df, EndDate = mutate_call)

  return(df)
}
