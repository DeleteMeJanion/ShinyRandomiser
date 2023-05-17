library(shiny)
source("graphics/PlotPanel.R")

distribution_panel <- function() {
  splitLayout(cellWidths = c("25%", "75%"),
    div(tableOutput(outputId = "previousValuesTable"), style='height:500px; overflow-y: scroll;'),
    plot_panel()
  )
}