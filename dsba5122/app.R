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
library(stringr)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(countrycode)

df <- readr::read_csv('hotels.csv')

source("orig.R")
source("orig_improv.R")
source("midterm_pages/page1.R")
source("midterm_pages/correlation.R")
source("midterm_pages/tree.R")
source("midterm_pages/MonthlyHotelBookings.R")
source("midterm_pages/AverageMonthlyHotelBookings.R")
source("midterm_pages/CancellationsvsRest.R")


source("final_pages/company_adr.R")
source("final_pages/adr_regression.R")
source("final_pages/repeatcancel.R")
source("final_pages/Lead_time.R")
source("final_pages/Map.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("sandstone"),
    navbarPage("Group Project",
               tabPanel("Original", origUI("orig")),
               tabPanel("Improved", origImprovUI("origimprov")),
               navbarMenu("Exploratory Analysis",
                      "Bookings",
                      tabPanel("Bookings by Country", MapUI("Map")),
                      tabPanel("Average Monthly Bookings", AverageMonthlyBookingsUI("AverageMonthlyHotelBookings")),
                      tabPanel("Monthly Bookings", MonthlyBookingsUI("MonthlyHotelBookings")),
                      "---------",
                      "Cancellations",
                      #tabPanel("Segment", page1UI("page1")),
                      tabPanel("Total Cancellations", cancellationUI("page2")),
                      tabPanel("Repeat Cancels", repeatcancelUI("repeatcancel")),
                      tabPanel("Lead Time vs Cancellations", LeadTimeUI("LeadTime")),
                      tabPanel("Correlation", correlationUI("correlation")),
                      "--------",
                      "Average Daily Rate",
                      tabPanel("Company Wise ADR", companyADRUI("company_adr"))
                     
               ),
               navbarMenu("Analytical Models",
                    tabPanel("Tree", decisionTreeUI("dtree")),
                    tabPanel("Regression", adrRegressionUI("adr_regression"))
               )
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
    companyADRServer("company_adr")
    adrRegressionUIServer("adr_regression")
    repeatcancelServer("repeatcancel")
    LeadTimeServer("LeadTime")
    MapServer("Map")
}

# Run the application 
shinyApp(ui = ui, server = server)
