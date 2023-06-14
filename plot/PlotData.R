create_histogram_data_frame <- function(distribution, values, min, max, binWidth) {
  bins_vector <- c()
  ideal_vector <- distribution$get_ideal_histogram(min, max, binWidth)
  actual_vector <- c()
  for (i in seq(min, max - binWidth, binWidth)) {
    bins_vector <- append(bins_vector, i)
    actual_vector <- append(actual_vector, sum(i <= values & values < (i + binWidth)) / length(values))
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

create_quartile_data_frame <- function(distribution) {
  ideal_vector <- distribution$get_ideal_quartiles()
  actual_vector <- distribution$calculate_quartiles()
  
  if (length(actual_vector) == 0) {
    df <- data.frame(c(ideal_vector),
                     c(rep("Ideal", 3)))
  } else {
    df <- data.frame(c(ideal_vector, actual_vector),
                     c(rep("Ideal", 3), rep("Actual", 3)))
  }
  colnames(df) <- c("Quartile", "Distribution")
  
  return(df)
}
