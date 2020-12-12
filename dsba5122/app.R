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
library(plotrix)
library(plotly)
library(shinythemes)

df <- readr::read_csv('hotels.csv')

source("orig.R")
source("orig_improv.R")
source("midterm_pages/page1.R")
source("midterm_pages/correlation.R")
source("midterm_pages/tree.R")
source("midterm_pages/MonthlyHotelBookings.R")
source("midterm_pages/AverageMonthlyHotelBookings.R")
source("midterm_pages/CancellationsvsRest.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("sandstone"),
    navbarPage("Group Project",
               tabPanel("Original", origUI("orig")),
               tabPanel("Improved", origImprovUI("origimprov")),
               navbarMenu("Midterm",
                   tabPanel("AverageMonthlyBookings", AverageMonthlyBookingsUI("AverageMonthlyHotelBookings")),
                   tabPanel("MonthlyBookings", MonthlyBookingsUI("MonthlyHotelBookings")),
                   tabPanel("Correlation", correlationUI("correlation")),
                   #tabPanel("Segment", page1UI("page1")),
                   tabPanel("Segment", cancellationUI("page2")),
                   tabPanel("Tree", decisionTreeUI("dtree")))
                             
    )
    
    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    page1Server("page1")
    CancellationServer("page2")
    correlationServer("correlation")
    origServer("orig")
    origImprovServer("origimprov")
    decisionTreeServer("dtree")
    MonthlyBookingsServer("MonthlyHotelBookings")
    AverageMonthlyBookingsServer("AverageMonthlyHotelBookings")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
