process.cumulativeflow <- function(data) {
  df <- as.data.frame(data)
  df <- tibble::rownames_to_column(df, var = "_Dates")
  df$`_Dates` <- as.numeric(df$`_Dates`)

  map = function(startCol, endCol) {
    apply(df[startCol:endCol], 1, sum)
  }

  startCol <- 2
  endCol <- ncol(df)

  for (col in startCol:endCol) {
    df <- dplyr::mutate(df, mapped_ = map(2,col))
    names(df)[names(df)=="mapped_"] <- paste("cum", names(df)[col], sep = "")
  }

  mutate_call = lazyeval::interp(~as.Date(a, origin="1970-01-01"), a = as.name("_Dates"))
  df <- dplyr::mutate_(df, cum_Date = mutate_call)

  resCol <- endCol + 1

  return(df[resCol:ncol(df)])
}
