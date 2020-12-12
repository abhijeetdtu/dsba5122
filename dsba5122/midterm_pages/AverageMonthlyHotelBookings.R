AverageMonthlyBookingsUI <- function(id){
  fluidPage(
  # Show a plot of the generated distribution
    fluidRow(
     column(8 ,offset=3 ,tags$h2("Average Monthly Hotel Reservations"))
     ),
  
    fluidRow(
    column(12,plotOutput(NS(id,'AverageMonthlyBookings')))
    
    )
  )
  
}

AverageMonthlyBookingsServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
   df <- readr::read_csv('hotels.csv')
   p <- df %>% group_by(hotel, arrival_date_year,arrival_date_month ) %>% count(hotel)
   monthly_df <- p %>% group_by(hotel,arrival_date_month)%>% mutate(Monthlymean=mean(n))

   output$AverageMonthlyBookings <- renderPlot({
     ggplot(monthly_df, aes(x = factor(arrival_date_month,levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December")),y=Monthlymean, fill=hotel)) + 
     geom_bar(stat = "identity") + 
     scale_x_discrete("Month")+
     scale_y_continuous("Avg Reservations", breaks = seq(0,10000, by = 1000)) +
    theme_bw()+
     facet_wrap(~hotel) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
      

  })
 })

}

