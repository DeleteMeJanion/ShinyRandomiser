source("distributions/Distribution.R")

# Distribution Function
generate_normal <- function(count, mean, stdev) {
  return(rnorm(count, mean, stdev))
}

# Ideal Distribution Function
generate_ideal_normal_histogram <- function(mean, stdev) {
  return(
    function(x, binWidth) {
      return(pnorm(x + binWidth, mean = mean, sd = stdev) - pnorm(x, mean = mean, sd = stdev))
    }
  )
}

# Ideal Quartile Function
generate_ideal_normal_quartiles <- function(mean, stdev) {
  return(
    function() {
      quartileSpacing <- stdev * 0.67449
      return(c(mean - quartileSpacing, mean, mean + quartileSpacing))
    }
  )
}

# Distribution Creation Function
Normal <- function(mean, stdev) {
  if (is.na(mean) || is.na(stdev) || stdev <= 0) {
    return(NULL)
  }
  print(paste("Creating Normal Distribution (", mean, ",", stdev, ")"))
  return(Distribution(name = "Normal",
                      is_continuous = TRUE,
                      generator = function(count) {generate_normal(count, mean, stdev)},
                      ideal_histogram_generator = generate_ideal_normal_histogram(mean, stdev),
                      ideal_quartiles_generator = generate_ideal_normal_quartiles(mean, stdev)))
}
