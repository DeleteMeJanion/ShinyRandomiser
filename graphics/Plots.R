library(ggplot2)

create_histogram_data_frame <- function(distribution, min, max, binWidth) {
  ideal_vector <- distribution$get_ideal_histogram(min, max, binWidth)
  bins_vector <- c()
  actual_vector <- c()
  for (i in seq(min, max - binWidth, binWidth)) {
    bins_vector <- append(bins_vector, i)
    actual_vector <- append(actual_vector, sum(i <= distribution$previous_values & distribution$previous_values < (i + binWidth)) / length(distribution$previous_values))
  }
  
  if (all(is.nan(actual_vector))) {
    df <- data.frame(c(bins_vector),
                     c(ideal_vector),
                     c(rep("Ideal", length(bins_vector))))
  } else {
    df <- data.frame(c(bins_vector, bins_vector),
                     c(ideal_vector, actual_vector),
                     c(rep("Ideal", length(bins_vector)), rep("Actual", length(bins_vector))))
  }
  colnames(df) <- c("Value", "Probability", "Distribution")
  
  return(df)
}

previousValuesHistogram <- function(distribution, min, max, binWidth) {
  df <- create_histogram_data_frame(distribution, min, max, binWidth)
  
  # Set up basic plot
  plot <- ggplot() +
    ggtitle("Generated Distribution vs Ideal") +
    xlab("Value") +
    ylab("Probability")
  
  # Plot the ideal and actual histograms as bar charts
  plot <- plot +
    geom_bar(
      data = df,
      mapping = aes(x=Value, y=Probability, color=Distribution, fill=Distribution),
      stat = "identity",
      just = 0,
      width = binWidth,
      position=position_identity()
    )
  
  # Set the colours
  if (is.null(distribution$previous_values)) {
    plot <- plot +
      scale_color_manual(values=c("#333333")) +
      scale_fill_manual(values=c("#222222"))
  } else {
    plot <- plot +
      scale_color_manual(values=c("#ff8800a0", "#333333")) +
      scale_fill_manual(values=c("#ff880080", "#222222"))
  }

  # # Plot the ideal quartiles
  # quartiles <- distribution$get_ideal_quartiles()
  # for (q in quartiles) {
  #   plot <- plot +
  #     geom_vline(
  #       xintercept = q,
  #       linewidth = 1
  #       )
  # }
  
  if (nrow(df) == 0) {
    plot <- plot +
      scale_x_continuous(breaks=seq(min, max, max(binWidth, ((max - min) / 10))))
  } else {
    # # Plot the actual quartiles
    # quartiles <- distribution$calculate_quartiles()
    # for (q in quartiles) {
    #   plot <- plot +
    #     geom_vline(
    #       xintercept = q,
    #       color = "#ff000080",
    #       size = 1
    #       )
    # }
  }
  return(plot)
}
