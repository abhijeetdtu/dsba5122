df <- readr::read_csv('hotels.csv')

origUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
    fluidRow(
      column(4 ,offset=4 ,tags$h1("Original Submission"))
    ),
    
    fluidRow(
      column(12,plotOutput(NS(id,'orig_plot')))
      
    )
  )
}

origServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    
    
     percentData <- df %>% group_by(hotel, arrival_date_year) %>% count(is_canceled) %>%
      mutate(ratio=scales::percent(n/sum(n)))
    
    output$orig_plot <- renderPlot({
      
      ggplot(percentData,aes(x=factor(hotel), y = n, fill=factor(is_canceled)))+
        geom_bar(position="fill", stat = "identity")+
        geom_text(data=percentData, aes(y = n, label= ratio), 
                  position=position_fill(0.5), color = "black")+
        scale_fill_manual(values = c("gray", "red"))+
        scale_y_continuous(labels=scales::percent)+
        facet_grid( .~ arrival_date_year)+
        theme_classic() +
        labs(title = "Hotels Booking Cancellation Ratio per Year",
             subtitle = "~booking was canceled (red) or not (gray)~",
             caption = "Source: Antonio, Almeida, and Nunes, 2019\n Code by: @magwanjiru", 
             x = "Hotel", y = "Distribution") +
        theme(legend.position="none",
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size=12, face = "italic", hjust = 0.5),
              plot.caption = element_text(size = 8, face = "italic", color = "blue"))
      
    })
  })
    
}