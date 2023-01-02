library(ggplot2)

previousValuesHistogram <- function(df, binWidth, min, max, distribution) {
  if (nrow(df) == 0) {
    ggplot() +
      geom_bar(
        data = distribution$get_ideal_histogram(min, max, binWidth),
        mapping = aes(x = Value, y = Probability / sum(Probability)),
        stat = "identity",
        just = 0,
        width = binWidth
      ) +
      ggtitle("Previous Value Distribution vs Ideal") +
      xlab("Value") +
      ylab("Probability") +
      scale_x_continuous(breaks=seq(min, max, max(binWidth, ((max - min) / 10))))
  } else {
    ggplot() +
      geom_bar(
        data = distribution$get_ideal_histogram(min, max, binWidth),
        mapping = aes(x = Value, y = Probability / sum(Probability)),
        stat = "identity",
        just = 0,
        width = binWidth,
        color = "#00000080"
      ) +
      geom_histogram(
        data = df,
        mapping = aes(x = Value, y = ..count.. / sum(..count..)),
        binwidth = binWidth,
        boundary = 0,
        closed = "left",
        color = "#00000080",
        fill = "#ff880080"
      ) +
      ggtitle("Previous Value Distribution vs Ideal") +
      xlab("Value") +
      ylab("Probability") +
      scale_x_continuous(breaks=seq(min, max, binWidth))
  }
}