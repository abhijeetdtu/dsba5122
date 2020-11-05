#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

df <- readr::read_csv('hotels.csv')

source("orig.R")
source("orig_improv.R")
source("page1.R")
source("correlation.R")
source("tree.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("Group Project",
               tabPanel("Original", origUI("orig")),
               tabPanel("Improved", origImprovUI("origimprov")),
               tabPanel("Segment", page1UI("page1")),
               tabPanel("Correlation", correlationUI("correlation")),
               tabPanel("Tree", decisionTreeUI("dtree"))
    )
    
    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    page1Server("page1")
    correlationServer("correlation")
    origServer("orig")
    origImprovServer("origimprov")
    decisionTreeServer("dtree")
}

# Run the application 
shinyApp(ui = ui, server = server)
