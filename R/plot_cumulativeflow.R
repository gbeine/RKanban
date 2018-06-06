plot.cumulativeflow <- function(data, title, fileName) {
  # TODO: beliebige Linien plotten, for-loop
  # TODO: farben automatisch vergeben?
  p <- ggplot2::ggplot(data, ggplot2::aes(x = cum_Date)) +
    ggplot2::geom_line(ggplot2::aes(y = cum_Open), colour="red") +
    ggplot2::geom_line(ggplot2::aes(y = `cum_In Progress`), colour="blue") +
    ggplot2::geom_line(ggplot2::aes(y = cum_Done), colour="green") +
    ggplot2::scale_x_date(labels = scales::date_format("%y-%m-%d"), breaks = scales::date_breaks("2 weeks")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
    ggplot2::labs(x = "Day of Calendar", y = "Number of Items") +
    ggplot2::ggtitle(title) +
    ggplot2::expand_limits(y = 0)
  file <- paste(today, fileName, sep=" ")
  print(file)
  pdf(file, paper = "a4")
  plot(p)
  dev.off()
}
