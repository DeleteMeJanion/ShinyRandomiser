source("distributions/Distribution.R")

# Distribution Function
generate_poisson <- function(count, mean) {
  return(rpois(count, mean))
}

# Ideal Distribution Function
generate_ideal_poisson_histogram <- function(mean) {
  return(
    function(x, binWidth) {
      return(ppois(x + binWidth - 1, lambda = mean) - ppois(x - 1, lambda = mean))
    }
  )
}

# Ideal Quartile Function
generate_ideal_poisson_quartiles <- function(mean) {
  return(
    function() {
      lowerQ = NULL
      middleQ = NULL
      upperQ = NULL
      
      total <- 0
      counter <- 0
      while (is.null(upperQ)) {
        probability <- ppois(counter, lambda = mean)
        if (is.null(lowerQ) && probability > 0.25) {
          lowerQ <- counter
        } else if (is.null(middleQ) && probability > 0.5) {
          middleQ <- counter
        } else if (is.null(upperQ) && probability > 0.75) {
          upperQ <- counter
        }
        counter <- counter + 1
      }
      return(c(lowerQ, middleQ, upperQ))
    }
  )
}

# Distribution Creation Function
Poisson <- function(mean) {
  if (is.na(mean) || mean <= 0) {
    return(NULL)
  }
  print(paste("Creating Poisson Distribution (", mean, ")"))
  return(Distribution(name = "Poisson",
                      is_continuous = FALSE,
                      generator = function(count) {generate_poisson(count, mean)},
                      ideal_histogram_generator = generate_ideal_poisson_histogram(mean),
                      ideal_quartiles_generator = generate_ideal_poisson_quartiles(mean)))
}
