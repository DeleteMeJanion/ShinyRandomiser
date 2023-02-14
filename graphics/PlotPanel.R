library(shiny)

plot_panel <- function() {
  verticalLayout(
    plotOutput(outputId = "previousValuesPlot"),
    conditionalPanel(
      condition = "output.isContinuous",
      numericInput(inputId = "continuousBinWidthBox", label = "Bin Width (Decimal)", value = 1, min = 0.1, step = 1)
    ),
    conditionalPanel(
      condition = "!output.isContinuous",
      numericInput(inputId = "discreteBinWidthBox", label = "Bin Width (Integer)", value = 1, min = 1, step = 1)
    )
  )
}