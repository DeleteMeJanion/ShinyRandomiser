library(reticulate)
py_run_file("quantiles.py")

calculate_quartiles <- function(distribution) {
  d <- py$Distribution(distribution$previous_values)
  return(d$calculate_quantiles(as.integer(4)))
}