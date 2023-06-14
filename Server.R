library(shiny)
source("distributions/Uniform.R")
source("distributions/Normal.R")
source("distributions/Poisson.R")
source("plot/Plots.R")

server <- function(input, output) {
  distribution_selection <- reactive(input$distribution_box)
  count <- reactive(input$count)
  
  # Create the distribution when the type is selected or a configuration value changes
  distribution <- reactive(
    if (distribution_selection() == "uniform") {
      return(Uniform(input$uMin, input$uMax))
    } else if (distribution_selection() == "normal") {
      return(Normal(input$nMean, input$nStdev))
    } else if (distribution_selection() == "poisson") {
      return(Poisson(input$pMean))
    } else {
      stop(paste("No Distribution called: ", distribution_selection()))
    }
  )
  
  # Change which bin width box is shown based on whether the distribution is continuous or not
  output$isContinuous <- reactive({
    if (!is.null(distribution())) {
      return(distribution()$is_continuous)
    }
    return(FALSE)
  })
  outputOptions(output, "isContinuous", suspendWhenHidden = FALSE)
  
  continuousBinWidth <- reactive(input$continuousBinWidthBox)
  discreteBinWidth <- reactive(input$discreteBinWidthBox)
  
  # Reactive value holding the previous values of the distribution
  # This needs be created thusly because the vector itself cannot be made reactive
  previous_values <- reactiveVal()
  
  # Generate number(s) on button click
  observeEvent(
    input$generate_btn,
    {
      distribution()$get_values(count())
      previous_values(distribution()$previous_values)
    }
  )
  
  # Clear previous values on distribution change
  observe({
    previous_values(distribution()$previous_values)
  })
  
  # Update table of previous values when the distribution is changed or when values are generated
  output$previousValuesTable <- renderTable({
    prevValues <-
      prettyNum(rev(previous_values()), digits = 4)
    if (!is.null(prevValues)) {
      df <- data.frame(prevValues)
    } else {
      df <- data.frame(double())
    }
    colnames(df) <- c("Generated Values")
    return(df)
  })
  
  # Plot the histogram of values when the distribution changes or when the generate button is clicked
  output$previousValuesPlot <- renderPlot({
    if (is.null(distribution())) {
      return()
    } else if (distribution()$name == "Uniform") {
      min <- input$uMin
      max <- input$uMax
      binWidth <- continuousBinWidth()
    } else if (distribution()$name == "Normal") {
      min <- input$nMean - (3 * input$nStdev)
      max <- input$nMean + (3 * input$nStdev)
      binWidth <- continuousBinWidth()
    } else if (distribution()$name == "Poisson") {
      min <- 0
      max <- max(input$pMean * 3, 5)
      binWidth <- discreteBinWidth()
    } else {
      stop(paste(
        "Cannot plot histogram for distribution named: ",
        distribution()$name
      ))
    }
    
    if (is.na(binWidth)) {
      return()
    }
    
    return(previousValuesHistogram(distribution(), previous_values(), min, max, binWidth))
  })
}
