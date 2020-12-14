df <- readr::read_csv('hotels.csv')
 
cancellationUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(8 ,offset=3 ,tags$h1("Deeper Dive Into Cancellations"))
    ),
    fluidRow(
      column(2,uiOutput(NS(id , "segment"))),
    ),
    
    fluidRow(
      column(3,plotOutput(NS(id,'count_canceled'))),
      column(9,plotOutput(NS(id,'trend_plot')))
      
    )
  )
}

CancellationServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    
    
    subdf <- df %>% group_by(country) %>% summarise(num_canceled = sum(is_canceled))
    
    output$slider <-  renderUI({
      sliderInput("bins", inputId=NS(id, "filterlevel"),
                  min = min(subdf$num_canceled) ,
                  max = max(subdf$num_canceled) ,
                  value = quantile(subdf$num_canceled , 0.75 )[[1]])
    })
    
    
    output$segment <- renderUI({
      selectInput(NS(id , "segment"), "Segment By" , c("hotel" , "customer_type" , "deposit_type","market_segment","assigned_room_type"))
    })
    
    output$count_canceled <- renderPlot({
      
      selectedCols <-  c("is_canceled" , input$segment)
      print(selectedCols)
      canceled <- df %>% 
        group_by_at(all_of(selectedCols)) %>% 
        summarise(counts = sum(is_canceled)) %>% 
        mutate(counts = round(counts / sum(counts) , 4)*100) %>%
        as.data.frame()
      #colnames(canceled) <- c("IsCanceled" , "Counts")
      #rownames(canceled) <- c("No" , "Yes")
      
      g <- ggplot(canceled , aes_string(x=1 ,y="counts",label="counts",fill=input$segment))+
        geom_bar(width = 1, stat = "identity")+
        geom_label_repel(position=position_stack(vjust = 0.5)) + 
        coord_polar("y", start=0) + 
        scale_fill_discrete(guide=F) + 
        theme_minimal() +
        theme(axis.text = element_blank())+
        ylab("Percentage")
      g
      
    })
    
    output$trend_plot <- renderPlot({
      
      #colnames(canceled) <- c("IsCanceled" , "Counts")
      #rownames(canceled) <- c("No" , "Yes")
      df$arrival_date_month <- factor(df$arrival_date_month, levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December"))   
      g <- df %>% 
        group_by_at(all_of(c(input$segment, "arrival_date_year" , "arrival_date_month"))) %>% 
        summarize(counts = sum(is_canceled)) %>%
        ggplot( aes_string(x="arrival_date_month" ,y="counts" , group=input$segment , color=input$segment ))+
        geom_line(size=2) +
        facet_wrap(~ arrival_date_year ) +
        theme_light() + 
        xlab("Arrival Month") +
        ylab("Count of Cancelled Bookings") +
        ggtitle("Total Cancelled Bookings Over Each Year")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
        theme(plot.title = element_text(size = 20, face = "bold")) +
        theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5)) +
        theme(axis.text.y = element_text(size = 12, angle = 90, vjust = 0.5)) +
        theme(axis.title = element_text(size = 16))
      
      
      
      g
      
    })
  })
}