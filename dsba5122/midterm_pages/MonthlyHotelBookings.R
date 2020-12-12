MonthlyBookingsUI <- function(id){
  
  fluidPage(
    # Show a plot of the generated distribution
    fluidRow(
      column(8 ,offset=3 ,tags$h2("Hotel Reservations from July 2015 - August 2017"))
    ),
    
    fluidRow(
      column(12,plotOutput(NS(id,'MonthlyBookings')))
      
    )
  )
}

MonthlyBookingsServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    
    
    df <- readr::read_csv('hotels.csv')
    p <- df %>% group_by(hotel, arrival_date_year,arrival_date_month ) %>% count(hotel)
    
    output$MonthlyBookings <- renderPlot({
      ggplot(p, aes(x = factor(arrival_date_month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December")),y=n, fill=hotel)) + geom_bar(stat = "identity") + facet_wrap(~hotel+arrival_date_year) +
        scale_x_discrete("Month")+
        scale_y_continuous("Reservations", breaks = seq(0,6000, by = 1000))+
        facet_wrap(~arrival_date_year) + facet_wrap(~hotel+arrival_date_year) +
        theme_bw()+ 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
        
      
    })
  })
  
}


