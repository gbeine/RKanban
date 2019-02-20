plot.leadtime.absolute <- function(data, title, fileName) {

  p <- ggplot2::ggplot(leadtimes, ggplot2::aes(x = `Lead Time`, y=..count..)) +
    ggplot2::geom_histogram(bins = max(leadtimes$`Lead Time`)) +
    ggplot2::geom_vline(xintercept = stats::quantile(leadtimes$`Lead Time`, prob = c(0.5, 0.75, 0.85, 0.9, 0.95)), color = 'blue', lty = 3) +
    ggplot2::labs(x = "Lead Time", y = "Number of items") +
    ggplot2::geom_density(ggplot2::aes(y=..count..)) +
    ggplot2::ggtitle(title)

  file <- paste(today, fileName, sep=" ")
  print(file)
  pdf(file, paper = "a4")
  plot(p)
  dev.off()
}
