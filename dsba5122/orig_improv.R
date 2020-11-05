df <- readr::read_csv('hotels.csv')

origImprovUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(2,uiOutput(NS(id,'segment')))
    ),
    fluidRow(
      column(12,plotOutput(NS(id,'orig_plot')))
      
    )
  )
}

origImprovServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    
    
    pltdf <- df %>% 
      group_by(arrival_date_year , hotel , is_canceled) %>% 
      summarise(count = n()) %>% 
      mutate(ratio=round((count/sum(count)),digits= 2) )
    
    output$segment <- renderUI({
      selectInput(NS(id , "segment"), "Compare" , c("hotel" , "year"))
    })
    
    output$orig_plot <- renderPlot({
      
      x <- if_else(input$segment == "hotel" , "hotel" , "arrival_date_year")
      facet <- if_else(x == "hotel" , "arrival_date_year" , "hotel")
      xlabel <- if_else(input$segment == "hotel" , "Hotel Type" , "Year")
      
      g <- (
        ggplot(pltdf , aes_string(x=x , y="ratio" , fill="factor(is_canceled)"))
        + geom_col(position=position_fill())
        + facet_wrap(paste("~ " , facet))
        + scale_y_continuous(labels=scales::percent)
        + geom_text(aes(y = ratio, label = scales::percent(ratio , accuracy = 1)),
                    position=position_fill(0.5), color = "black" , size=10 , alpha=0.4)
        + scale_fill_manual(values = c("#95d5b2", "#f28482") , name="Canceled ?" , labels=c("No" , "Yes"))
        #+ theme(legend.title = element_text("Canceled ?"))
      )
      
       g+ 
        theme_classic() +
        labs(title = "Hotels Booking Cancellation Percentage",
             #subtitle = "~booking was canceled (red) or not (gray)~",
             caption = "Source: Antonio, Almeida, and Nunes, 2019\n Code by: @magwanjiru", 
             x = "Hotel Type", y = "Percentage") +
        xlab(xlabel)+
        theme(legend.position="top",
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size=12, face = "italic", hjust = 0.5),
              plot.caption = element_text(size = 8, face = "italic", color = "blue"),
              axis.text.x = element_text(size=15 , face="bold" , color=alpha('black', 0.5)))
      
    })
  })
  
}