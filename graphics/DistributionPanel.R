library(shiny)
source("graphics/PlotPanel.R")

distribution_panel <- function() {
  verticalLayout(
    div(
      div("Generated Random Number: ", style = "display:inline-block"),
      div(textOutput(outputId = "rand"), style = "display:inline-block")
    ),
    br(),
    actionButton(inputId = "generate_btn", label = "Generate"),
    br(),
    splitLayout(cellWidths = c("25%", "75%"),
      div(tableOutput(outputId = "previousValuesTable"), style='height:500px; overflow-y: scroll;'),
      plot_panel()
    )
  )
}