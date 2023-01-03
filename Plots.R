library(ggplot2)
source("Quantiles.R")

previousValuesHistogram <- function(df, binWidth, min, max, distribution) {
  plot <- ggplot() +
    geom_bar(
      data = distribution$get_ideal_histogram(min, max, binWidth),
      mapping = aes(x = Value, y = Probability / sum(Probability)),
      stat = "identity",
      just = 0,
      width = binWidth
    ) +
    ggtitle("Previous Value Distribution vs Ideal") +
    xlab("Value") +
    ylab("Probability")
  
  quartiles <- distribution$get_ideal_quartiles()
  for (q in quartiles) {
    plot <- plot +
      geom_vline(
        xintercept = q,
        size = 1)
  }
  
  if (nrow(df) == 0) {
    plot <- plot +
      scale_x_continuous(breaks=seq(min, max, max(binWidth, ((max - min) / 10))))
  } else {
    plot <- plot +
      geom_histogram(
        data = df,
        mapping = aes(x = Value, y = ..count.. / sum(..count..)),
        binwidth = binWidth,
        boundary = 0,
        closed = "left",
        color = "#ff8800a0",
        fill = "#ff880080"
      ) +
      scale_x_continuous(breaks=seq(min, max, binWidth))
    
    quartiles <- calculate_quartiles(distribution)
    for (q in quartiles) {
      plot <- plot +
        geom_vline(
          xintercept = q,
          color = "#ff000080",
          size = 1)
    }
  }
  return(plot)
}
