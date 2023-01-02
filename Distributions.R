############################
## Distribution Functions ##
############################

generate_uniform <- function(count, min, max) {
  return(runif(count, min, max))
}

generate_normal <- function(count, mean, stdev) {
  return(rnorm(count, mean, stdev))
}

generate_poisson <- function(count, mean) {
  return(rpois(count, mean))
}

##################################
## Ideal Distribution Functions ##
##################################

generate_ideal_histogram <- function(min, max, binWidth, barHeightFunction) {
  barPositions <- vector(mode = "numeric")
  barHeights <- vector(mode = "numeric")
  
  for (x in 0:(((max - min) / binWidth) - 1)) {
    barPosition <- min + (x * binWidth)
    barPositions <- append(barPositions, barPosition)
    barHeights <- append(barHeights, barHeightFunction(barPosition, binWidth))
  }
  df = data.frame(barPositions, barHeights)
  colnames(df) <- c("Value", "Probability")
  return(df)
}

generate_ideal_uniform <- function(min, max) {
  return(
    function(x, binWidth) {
      return(1 / ((max - min) * binWidth))
    }
  )
}

generate_ideal_normal <- function(mean, stdev) {
  return(
    function(x, binWidth) {
      return(pnorm(x + binWidth, mean = mean, sd = stdev) - pnorm(x, mean = mean, sd = stdev))
    }
  )
}

generate_ideal_poisson <- function(mean) {
  return(
    function(x, binWidth) {
      return(ppois(x + binWidth - 1, lambda = mean) - ppois(x - 1, lambda = mean))
    }
  )
}

#####################################
## Distribution Creation Functions ##
#####################################

Distribution <- setRefClass("Distribution",
                            fields = list(
                              name = "character",
                              is_continuous = "logical",
                              previous_values = "vector",
                              generator = "function",
                              ideal_generator = "function"
                              ),
                            methods=list(
                              get_values = function(count) {
                                value <- generator(count)
                                if (is.null(previous_values)) {
                                  previous_values <<- vector(mode = "numeric")
                                }
                                previous_values <<- append(previous_values, value)
                                return(value)
                              },
                              get_ideal_histogram = function(min, max, binWidth) {
                                return(generate_ideal_histogram(min, max, binWidth, ideal_generator))
                              }
                              )
                            )

Uniform <- function(min, max) {
  if (is.na(min) || is.na(max) || min >= max) {
    return(NULL)
  }
  print(paste("Creating Uniform Distribution (", min, ",", max, ")"))
  return(Distribution(name = "Uniform",
                      is_continuous = TRUE,
                      generator = function(count) {generate_uniform(count, min, max)},
                      ideal_generator = generate_ideal_uniform(min, max)))
}

Normal <- function(mean, stdev) {
  if (is.na(mean) || is.na(stdev) || stdev <= 0) {
    return(NULL)
  }
  print(paste("Creating Normal Distribution (", mean, ",", stdev, ")"))
  return(Distribution(name = "Normal",
                      is_continuous = TRUE,
                      generator = function(count) {generate_normal(count, mean, stdev)},
                      ideal_generator = generate_ideal_normal(mean, stdev)))
}

Poisson <- function(mean) {
  if (is.na(mean) || mean <= 0) {
    return(NULL)
  }
  print(paste("Creating Poisson Distribution (", mean, ")"))
  return(Distribution(name = "Poisson",
                      is_continuous = FALSE,
                      generator = function(count) {generate_poisson(count, mean)},
                      ideal_generator = generate_ideal_poisson(mean)))
}
