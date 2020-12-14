library(plotly)
df <- readr::read_csv('hotels.csv')

LeadTimeUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(tags$h3("Total Number of Cancellations by Lead Time")),
    fluidRow(
      column(3,uiOutput(NS(id,'segment')))
    ),
    
    fluidRow(
      column(12,plotlyOutput(NS(id,'plot')))
    )
  )
}
LeadTimeServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    output$segment <- renderUI({
      selectInput(NS(id , "segment"), "Lead Time Bin Size (in days)" , choices=list(7 , 30,90,365) , selected=7)
    })
    
    output$plot <- renderPlotly({
      df$Total_Cancelations = sum(df$is_canceled)
      df$bins <- cut(df$lead_time, breaks=seq(min(df$lead_time), max(df$lead_time), as.numeric(input$segment)))
      new_df = df %>% group_by(bins)%>% summarise(Percent_canceled = (sum(is_canceled)/Total_Cancelations)*100)
      new_df = distinct(new_df)
      # Basic line plot with points
      new_df$cum_sum <- cumsum(new_df$Percent_canceled)
      
      g <- ggplot(data=new_df, aes(x=bins, y=Percent_canceled )) +
        geom_point(color="blue")+
        geom_col(width = 0.1)+
        geom_line(aes(y=cum_sum) , group=1 ,  linetype='dotted') + 
        labs(title = "Each Bin's Share of Total Cancelations")+
        #annotate(geom="text",x=levels(df$bins)[-1] , y=110 ,label="Cum-Sum")+
        theme(axis.text.x = element_text(angle = 90)) +
        ylab(label="Percent") + 
        xlab(label="Lead Time") +
        theme_minimal()
      g
    })
    
  })
}

