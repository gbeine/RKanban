plot.cumulativeflow <- function(data, mapping, title, fileName) {

  plot_command <- "ggplot2::ggplot(cumulativeFlow, ggplot2::aes(x = cum_Date))"

  for(i in 1:nrow(mapping)) {
    name <- paste0("cum", mapping[i,1])
    color <- mapping[i, ncol(mapping)]
    plot_command <- paste0(plot_command, " + ggplot2::geom_line(ggplot2::aes(y = `", name, "`), colour='", color ,"')")
  }

  p <- eval(parse(text=plot_command))
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
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
