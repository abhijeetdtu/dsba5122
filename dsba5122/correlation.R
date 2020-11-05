df <- readr::read_csv('hotels.csv')

correlationUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(8,tags$h3("Variable Correlations with Cancellations"))
    ),
    
    fluidRow(
      column(12,plotOutput(NS(id,'corr_plot')))
      
    )
  )
}

correlationServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    
      
    output$corr_plot <- renderPlot({
        cor_tri <- df %>% 
                    select_if(is.numeric) %>% 
                    cor() %>% 
                    as.data.frame() %>% 
                    mutate(Var1 = factor(row.names(.), levels=row.names(.))) %>% 
                    gather(key = Var2, value = value, -Var1, na.rm = TRUE, factor_key = TRUE) 
        print(cor_tri)
        
        g <- ggplot(cor_tri %>% filter(Var1 == "is_canceled") , aes(x=Var1 , y=Var2 , fill=value)) +
              geom_tile() + 
              scale_fill_gradient2(low="#e76f51" ,mid="#e5e5e5", high="#2a9d8f" ,limits=c(-1,1) , name="Correlation \n Coefficient") +
              xlab("Cancellation")+
              ylab("Variable Name")+
              theme(axis.text.x  = element_blank()) + 
              theme_light() 
        g
      
    })
  })
}