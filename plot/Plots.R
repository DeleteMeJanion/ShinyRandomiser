library(ggplot2)
source("plot/PlotData.R")

previousValuesHistogram <- function(distribution, min, max, binWidth) {
  histogram_df <- create_histogram_data_frame(distribution, min, max, binWidth)
  quartiles_data_frame <- create_quartile_data_frame(distribution)
  
  # Set up basic plot
  plot <- ggplot() +
    ggtitle("Generated Distribution vs Ideal (With Quartiles)") +
    xlab("Value") +
    ylab("Probability")
  
  # Plot the ideal and actual histograms as bar charts
  plot <- plot +
    geom_bar(
      data = histogram_df,
      mapping = aes(x=Value, y=Probability, fill=Distribution),
      stat = "identity",
      just = 0,
      width = binWidth,
      position=position_identity()
    )
  
  # Plot the ideal and actual quartiles
  plot <- plot +
    geom_vline(
      data = quartiles_data_frame,
      mapping = aes(xintercept=Quartile, colour=Distribution),
      linewidth = 1
    )
  
  # Set the colours
  if (is.null(distribution$previous_values)) {
    plot <- plot +
      scale_colour_manual(values=c("#444444")) +
      scale_fill_manual(values=c("#bbbbbb"))
  } else {
    plot <- plot +
      scale_colour_manual(values=c("#ff6600", "#444444")) +
      scale_fill_manual(values=c("#cc6600a0", "#bbbbbb"))
  }
  
  if (nrow(histogram_df) == 0) {
    plot <- plot +
      scale_x_continuous(breaks=seq(min, max, max(binWidth, ((max - min) / 10))))
  }
  
  return(plot)
}
