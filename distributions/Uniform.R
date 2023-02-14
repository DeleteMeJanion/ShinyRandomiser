source("distributions/Distribution.R")

# Distribution Function
generate_uniform <- function(count, min, max) {
  return(runif(count, min, max))
}

# Ideal Distribution Function
generate_ideal_uniform_histogram <- function(min, max) {
  return(
    function(x, binWidth) {
      return(1 / ((max - min) * binWidth))
    }
  )
}

# Ideal Quartile Function

generate_ideal_uniform_quartiles <- function(min, max) {
  return(
    function() {
      range <- max - min
      return(c(min + range / 4, min + range / 2, max - range / 4))
    }
  )
}

# Distribution Creation Function
Uniform <- function(min, max) {
  if (is.na(min) || is.na(max) || min >= max) {
    return(NULL)
  }
  print(paste("Creating Uniform Distribution (", min, ",", max, ")"))
  return(Distribution(name = "Uniform",
                      is_continuous = TRUE,
                      generator = function(count) {generate_uniform(count, min, max)},
                      ideal_histogram_generator = generate_ideal_uniform_histogram(min, max),
                      ideal_quartiles_generator = generate_ideal_uniform_quartiles(min, max)))
}
