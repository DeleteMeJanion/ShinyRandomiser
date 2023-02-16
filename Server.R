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
  
  # Generate a number(s) on button click
  observeEvent(priority = 1,
               input$generate_btn,
               output$rand <- renderText({
                 value <- isolate(distribution())$get_values(isolate(count()))
                 return(prettyNum(value[length(value)], digits = 4))
               })
  )
  
  # Update table of previous values when the distribution is changed or when values are generated
  observeEvent({
                 input$generate_btn
                 distribution()
               },
               output$previousValuesTable <- renderTable({
                 prevValues <- prettyNum(isolate(distribution()$previous_values), digits = 4)
                 if (!is.null(prevValues)) {
                   df <- data.frame(prevValues)
                 } else {
                   df <- data.frame(double())
                 }
                 colnames(df) <- c("Previous Values")
                 return(df)
               })
  )
  
  # Plot the histogram of values when the distribution changes or when the generate button is clicked
  observeEvent({
                 input$generate_btn
                 distribution()
               },
               output$previousValuesPlot <- renderPlot({
                 if (is.null(distribution())) {
                   min <- 0
                   max <- 1
                   binWidth <- 1
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
                   stop(paste("Cannot plot histogram for distribution named: ", distribution()$name))
                 }

                 if (is.na(binWidth)) {
                   return()
                 }

                 return(previousValuesHistogram(distribution(), min, max, binWidth))
               })
  )
}
