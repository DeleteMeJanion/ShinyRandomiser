library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
  ),
  
  titlePanel("Generate a Random Number"),
  sidebarLayout(
    sidebarPanel(
      verticalLayout(
        selectInput(
          inputId = "distribution_box",
          label = "Distribution",
          choices = c(
            "Uniform" = "uniform",
            "Normal" = "normal",
            "Poisson" = "poisson"
          )
        ),
        "Parameters:",
        numericInput(inputId = "count", label = "Generation Count", value = 1, min=1, max=1000),
        conditionalPanel(
          condition = "input.distribution_box == 'uniform'",
          numericInput(inputId = "uMin", label = "Minimum", value = 0),
          numericInput(inputId = "uMax", label = "Maximum", value = 1)
        ),
        conditionalPanel(
          condition = "input.distribution_box == 'normal'",
          numericInput(inputId = "nMean", label = "Mean", value = 0),
          numericInput(inputId = "nStdev", label = "Standard Deviation", value = 1, min = 0.1)
        ),
        conditionalPanel(
          condition = "input.distribution_box == 'poisson'",
          numericInput(inputId = "pMean", label = "Mean", value = 1, min = 0.1)
        )
      )
    ),
    mainPanel(
      verticalLayout(
        div(
          div("Generated Random Number: ", style = "display:inline-block"),
          div(textOutput(outputId = "rand"), style = "display:inline-block")
        ),
        br(),
        actionButton(inputId = "generate_btn", label = "Generate"),
        br(),
        splitLayout(
          div(tableOutput(outputId = "previousValuesTable"), style='height:500px; overflow-y: scroll;'),
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
        )
      )
    )
  )
)