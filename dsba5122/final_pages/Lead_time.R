library(plotly)
df <- readr::read_csv('hotels.csv')

LeadTimeUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(2,uiOutput(NS(id,'segment')))
    ),
    
    fluidRow(
      column(12,plotlyOutput(NS(id,'plot')))
    )
  )
}
LeadTimeServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    output$segment <- renderUI({
      selectInput(NS(id , "segment"), "Lead Time" , c(7,30,90,365))
    })
    output$plot <- renderPlotly({
      df <- readr::read_csv('hotels.csv')
      if(input$segment=='7'){
        X <- 7
        
        
      } 
      else if(input$segment=='30'){
        X<-30
        
      }
      else if(input$segment=='90'){
        X<-90
        
      }
      else if(input$segment=='365'){
        X<-365
        
      }
      df$Total_Cancelations = sum(df$is_canceled)
      df$bins <- cut(df$lead_time, breaks=seq(0, 750, X))
      new_df = df %>% group_by(bins)%>% summarise(Percent_canceled = (sum(is_canceled)/Total_Cancelations)*100)
      new_df = distinct(new_df)
      # Basic line plot with points
      g <- ggplot(data=new_df, aes(x=bins, y=Percent_canceled , group=1)) +
        geom_point(color="blue")+geom_col(width = 0.1)+labs(title = "Lead Time vs Cancellations")+
        theme(axis.text.x = element_text(angle = 90)) +
        
        ylab(label="Percent Canceled") + xlab(label="Lead Time") + 
        scale_x_discrete(labels=seq(X, 750+X, by=X))
      g
    })
    
  })
}

