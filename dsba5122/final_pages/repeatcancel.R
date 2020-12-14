df <- readr::read_csv('hotels.csv')

repeatcancelUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(8 ,offset=2 ,tags$h1("Deeper Look at Guests who had Prior Cancellations and Non-cancellations"))
    ),
    fluidRow(
      column(2,uiOutput(NS(id , "option"))),
      column(10,plotOutput(NS(id,'trend_plot')))
    ),
    
    
    fluidRow(
      column(2,uiOutput(NS(id , "option2"))),
      column(10, plotOutput(NS(id, 'trend_plot2')))
    )
    
  )
}

repeatcancelServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    output$option <- renderUI({
      selectInput(NS(id , "option"), "Choose a variable to filter by" , c("is_repeated_guest" , "customer_type" , "deposit_type", "booking_changes", "total_of_special_requests"))
    })
    
    output$option2 <- renderUI({
      selectInput(NS(id , "option2"), "Choose a variable to filter by" , c("is_repeated_guest" , "customer_type" , "deposit_type",  "booking_changes", "total_of_special_requests"))
    })
    
    output$trend_plot <- renderPlot({
      
      df$arrival_date_month <- factor(df$arrival_date_month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December"))   
      
      g <- df %>% 
        group_by_at(all_of(c(input$option ,"arrival_date_year" , "arrival_date_month"))) %>% 
        summarize(counts = sum(previous_cancellations)) %>%
        ggplot( aes_string(x="arrival_date_month" ,y="counts", group=input$option, color=input$option))+
        geom_line(size = 2) +
        facet_wrap(~ arrival_date_year ) +
        theme_light() + 
        xlab("Arrival Month") +
        ylab("Count of Cancelled Bookings") +
        ggtitle("Number of Bookings Cancelled")+
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5)) +
        theme(axis.text.y = element_text(size = 10, angle = 90, vjust = 0.5)) +
        theme(axis.title = element_text(size = 16))
      
      g
      
    })
    
    
    output$trend_plot2 <- renderPlot({
      
      df$arrival_date_month <- factor(df$arrival_date_month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December"))   
      
      m <- df %>% 
        group_by_at(all_of(c(input$option2 ,"arrival_date_year" , "arrival_date_month"))) %>% 
        summarize(counts = sum(previous_bookings_not_canceled)) %>%
        ggplot( aes_string(x="arrival_date_month" ,y="counts", group=input$option2, color=input$option2))+
        geom_line(size = 2) +
        facet_wrap(~ arrival_date_year ) +
        theme_light() + 
        xlab("Arrival Month") +
        ylab("Count of Uncancelled Bookings") +
        ggtitle("Number of Bookings Not Cancelled")+
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5)) +
        theme(axis.text.y = element_text(size = 12, angle = 90, vjust = 0.5)) +
        theme(axis.title = element_text(size = 16))
      
      m
      
    })
    
  })
}