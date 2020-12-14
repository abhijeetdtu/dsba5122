# These are the datapoints where there is no adults, children and babies so we will not consider these points

MapUI <- function(id){
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(2 , fluidRow(
        fluidRow(uiOutput(NS(id,'hotelsegment'))),
        fluidRow(uiOutput(NS(id,'bookingssegment'))),
        fluidRow(checkboxInput(NS(id,'logscale'), "Log Scale", FALSE)),
      )),
      column(10,plotlyOutput(NS(id,'plot')))
    )
  )
}

MapServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$hotelsegment <- renderUI({
      selectInput(NS(id , "hotelsegment"), "Hotel" , c("All","City Hotel","Resort Hotel"))
    })
    output$bookingssegment <- renderUI({
      selectInput(NS(id , "bookingssegment"), "Booking Type" , c("All","Only Cancelled","Only Non-Cancelled"))
    })  
    output$plot <- renderPlotly({
      df <- readr::read_csv('hotels.csv')
      hotel_data <- df
      
      if(input$hotelsegment=='City Hotel'){
        sub_hotel_data <-hotel_data[hotel_data$hotel == "City Hotel",]
        
      }
      else if(input$hotelsegment=='Resort Hotel'){
        sub_hotel_data <-hotel_data[hotel_data$hotel == "Resort Hotel",]
      }
      else {
        sub_hotel_data <- hotel_data
        
      } 
     
      if(input$bookingssegment=='Only Cancelled'){
        sub_hotel_data <-sub_hotel_data[sub_hotel_data$is_canceled == 1,]
        
      }
      else if(input$bookingssegment=='Only Non-cancelled'){
        sub_hotel_data <-sub_hotel_data[sub_hotel_data$is_canceled == 0,]
        
      }
      else {
        (input$bookingssegment=='All')
        sub_hotel_data <- sub_hotel_data
        
      } 
      
      #sub_hotel_data <- sub_hotel_data[sub_hotel_data$is_canceled == 1,]
      # Subset the data to include the countries which has more than 2000 reservation request
      hotelbycountry <- sub_hotel_data %>%
        
        group_by(country) %>% summarise(n = n())
      
      library(countrycode)
      hotelbycountry$county_name <- countrycode(hotelbycountry$country,
                                                origin = "iso3c",
                                                destination = "country.name")
      
      if(input$logscale){
        hotelbycountry$n <- log10(hotelbycountry$n)
      }
      
      p <- plot_ly(
        hotelbycountry,
        type = "choropleth",
        locations = hotelbycountry$country,
        z = hotelbycountry$n,
        text = hotelbycountry$county_name,
        colorscale = "Reds",
        title = "Total Bookings"
      )
      g <- layout(p, title = 'Count of Bookings by Countries',titlefont=list(size=17))
      g
    
    })
  
  
 })
}
    