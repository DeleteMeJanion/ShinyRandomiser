library(shiny)
source("graphics/ConfigurationPanel.R")
source("graphics/DistributionPanel.R")

distribution_page <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
    ),
    
    titlePanel("Generate a Random Number"),
    sidebarLayout(
      sidebarPanel(configuration_panel()),
      mainPanel(distribution_panel())
    )
  )
}