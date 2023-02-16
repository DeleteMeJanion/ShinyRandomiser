library(reticulate)
py_run_file("distributions/quantiles.py")

# Ideal Distribution Function
generate_ideal_histogram <- function(min, max, binWidth, barHeightFunction) {
  barHeights <- vector(mode = "numeric")
  
  for (x in 0:(((max - min) / binWidth) - 1)) {
    barPosition <- min + (x * binWidth)
    barHeights <- append(barHeights, barHeightFunction(barPosition, binWidth))
  }
  return(barHeights)
}

Distribution <- setRefClass("Distribution",
                            fields = list(
                              name = "character",
                              is_continuous = "logical",
                              previous_values = "vector",
                              generator = "function",
                              ideal_histogram_generator = "function",
                              ideal_quartiles_generator = "function"
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
                              calculate_quartiles = function() {
                                d <- py$Distribution(previous_values)
                                return(d$calculate_quantiles(as.integer(4)))
                              },
                              get_ideal_histogram = function(min, max, binWidth) {
                                return(generate_ideal_histogram(min, max, binWidth, ideal_histogram_generator))
                              },
                              get_ideal_quartiles = function() {
                                return(ideal_quartiles_generator())
                              }
                              )
                            )
