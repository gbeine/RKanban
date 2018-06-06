plot.runtimes <- function(cycle, column, title, fileName) {
  ct.mean <- mean(cycle$"Lead Time")
  ct.median <- median(cycle$"Lead Time")
  ct.sd <- sd(cycle$"Lead Time")
  ct.var <- var(cycle$"Lead Time")
  ct.points <- nrow(cycle)
  ct.quantile <- quantile(cycle$"Lead Time", c(.50, .75, .85, .90))

  grobMean <- grid::grobTree(
    grid::textGrob(paste("Mean:", ct.mean, sep = " "), x=0.1,  y=0.95, hjust=0,
                   gp=grid::gpar(col="red", fontsize=13, fontface="italic")))
  grobMedian <- grid::grobTree(
    grid::textGrob(paste("Median:", ct.median, sep = " "), x=0.1,  y=0.90, hjust=0,
                   gp=grid::gpar(col="red", fontsize=13, fontface="italic")))
  grobVariance <- grid::grobTree(
    grid::textGrob(paste("Variance:", ct.var, sep = " "), x=0.1,  y=0.85, hjust=0,
                   gp=grid::gpar(col="red", fontsize=13, fontface="italic")))
  grobStdDeviation <- grid::grobTree(
    grid::textGrob(paste("Std deviation:", ct.sd, sep = " "), x=0.1,  y=0.80, hjust=0,
                   gp=grid::gpar(col="red", fontsize=13, fontface="italic")))
  grobPoints <- grid::grobTree(
    grid::textGrob(paste("D points:", ct.points, sep = " "), x=0.1,  y=0.75, hjust=0,
                   gp=grid::gpar(col="red", fontsize=13, fontface="italic")))

  p <- ggplot2::ggplot(cycle,
                       ggplot2::aes(x = EndDate, y = `Lead Time`)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_date(labels = scales::date_format("%y-%m-%d"), breaks = scales::date_breaks("2 weeks")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
    ggplot2::labs(x = "Day of Calendar", y = "Time in Days") +
    ggplot2::ggtitle(title) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::stat_quantile(color = "red", quantiles = c(0.5)) +
    ggplot2::stat_quantile(color = "blue", quantiles = c(0.75)) +
    ggplot2::geom_hline(yintercept=ct.quantile[1], linetype="dotted") +
    ggplot2::geom_hline(yintercept=ct.quantile[2], linetype="dotted") +
    ggplot2::geom_hline(yintercept=ct.quantile[3], linetype="dotted") +
    ggplot2::geom_hline(yintercept=ct.quantile[4], linetype="dotted") +
    ggplot2::annotation_custom(grobMedian) +
    ggplot2::annotation_custom(grobMean) +
    ggplot2::annotation_custom(grobStdDeviation) +
    ggplot2::annotation_custom(grobVariance) +
    ggplot2::annotation_custom(grobPoints)
  file <- paste(today, fileName, sep=" ")
  print(file)
  pdf(file, paper = "a4")
  plot(p)
  dev.off()
}
