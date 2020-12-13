df <- readr::read_csv('hotels.csv')

repeatcancelUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(8 ,offset=2 ,tags$h1("Deeper Look at Repeat Guests and Cancellations"))
    ),
    #    fluidRow(
    #      column(2,uiOutput(NS(id , "daterange"))),
    #    ),
    
    fluidRow(
      # column(3,plotOutput(NS(id,'count_canceled'))),
      column(10,plotOutput(NS(id,'trend_plot'))),
      br(),
      br(),
      column(10, plotOutput(NS(id, 'trend_plot2')))
      
    )
  )
}

repeatcancelServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    #    output$daterange <- renderUI({
    #      selectInput(NS(id , "daterange"), "Choose a date format to filter by" , c("arrival_date_year" , "arrival_date_month" , "arrival_date_week_number"))
    #    })
    
    output$trend_plot <- renderPlot({
      
      df$arrival_date_month <- factor(df$arrival_date_month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December"))   
      
      g <- df %>% 
        group_by_at(all_of(c("is_repeated_guest" ,"arrival_date_year" , "arrival_date_month"))) %>% 
        summarize(counts = sum(previous_cancellations)) %>%
        ggplot( aes_string(x="arrival_date_month" ,y="counts", group="is_repeated_guest", color="is_repeated_guest"))+
        geom_line(size = 2) +
        facet_wrap(~ arrival_date_year ) +
        theme_light() + 
        xlab("Arrival Month") +
        ylab("Count of Cancelled Bookings") +
        ggtitle("Number of Bookings Cancelled by Repeat Guests")+
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5)) +
        theme(axis.text.y = element_text(size = 10, angle = 90, vjust = 0.5)) +
        theme(axis.title = element_text(size = 16))
      
      g
      
    })
    
    
    output$trend_plot2 <- renderPlot({
      
      df$arrival_date_month <- factor(df$arrival_date_month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December"))   
      
      m <- df %>% 
        group_by_at(all_of(c("is_repeated_guest" ,"arrival_date_year" , "arrival_date_month"))) %>% 
        summarize(counts = sum(previous_bookings_not_canceled)) %>%
        ggplot( aes_string(x="arrival_date_month" ,y="counts", group="is_repeated_guest", color="is_repeated_guest"))+
        geom_line(size = 2) +
        facet_wrap(~ arrival_date_year ) +
        theme_light() + 
        xlab("Arrival Month") +
        ylab("Count of Uncancelled Bookings") +
        ggtitle("Number of Bookings Not Cancelled by Repeat Guests")+
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5)) +
        theme(axis.text.y = element_text(size = 12, angle = 90, vjust = 0.5)) +
        theme(axis.title = element_text(size = 16))
      
      m
      
    })
    
  })
}