library(ggplot2)

previousValuesHistogram <- function(df, binWidth, min, max, distribution) {
  # Set up basic plot
  plot <- ggplot() +
    ggtitle("Previous Value Distribution vs Ideal") +
    xlab("Value") +
    ylab("Probability")
  
  # Plot the ideal histogram for the distribution
  plot <- plot +
    geom_bar(
      data = distribution$get_ideal_histogram(min, max, binWidth),
      mapping = aes(x = Value, y = Probability / sum(Probability)),
      stat = "identity",
      just = 0,
      width = binWidth
    )

  # Plot the ideal quartiles
  quartiles <- distribution$get_ideal_quartiles()
  for (q in quartiles) {
    plot <- plot +
      geom_vline(
        xintercept = q,
        linewidth = 1
        )
  }
  
  if (nrow(df) == 0) {
    plot <- plot +
      scale_x_continuous(breaks=seq(min, max, max(binWidth, ((max - min) / 10))))
  } else {
    # Plot the actual histogram
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
    
    # Plot the actual quartiles
    quartiles <- distribution$calculate_quartiles()
    for (q in quartiles) {
      plot <- plot +
        geom_vline(
          xintercept = q,
          color = "#ff000080",
          size = 1
          )
    }
  }
  return(plot)
}
