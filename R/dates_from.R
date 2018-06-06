dates.from <- function(data) {
  lapply(data[[1]], function(x) { make.date(as.integer(x)) })
}
