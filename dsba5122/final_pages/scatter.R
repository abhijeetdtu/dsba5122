df <- readr::read_csv('hotels.csv')

scatterUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(8,tags$h3("Scatter"))
    ),
    
    fluidRow(
      column(12,plotOutput(NS(id,'scatter_plot')))
      
    )
  )
}

scatterServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    output$scatter_plot <- renderPlot({
        g <- ggplot(df , aes(x=adr , y=adults , color=factor(is_canceled)))+
          geom_point(stroke=0) +
        theme_light() 
        
        g
      
    })
  })
}