


companyADRUI <- function(id){
  
  # Show a plot of the generated distribution
  
  fluidPage(
    fluidRow(
      column(8,fluidRow(
        tags$h3("Company Wise Average Daily Rates")
        ,"Average Daily Rate as defined by dividing the sum of all lodging transactions by the total number of staying nights")
      )
    ),
    fluidRow(tags$hr()),
    fluidRow(
      column(4, fluidRow(
                  fluidRow(sliderInput(NS(id,'min_adr'), "Minium Mean $ ADR",
                             min = 10, max =200 , value = 10))
        ))),
    fluidRow(
      column(6, plotOutput(NS(id,'packed_circles'))),
      column(6, plotOutput(NS(id,'trend')))
    )
  )
}

companyADRServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    get_company_spends <- reactive({
      company_spends <- df %>% group_by(company) %>% summarise(mean_adr = mean(adr)) %>% as.data.frame()
      company_spends <- company_spends %>% filter(mean_adr > input$min_adr)
      company_spends
    })
    
    output$packed_circles <- renderPlot({
      company_spends <- get_company_spends()
      
      packing <- circleProgressiveLayout(company_spends$mean_adr, sizetype='area')
      coefs <- cbind(company_spends, packing)
      dat_gg <- circleLayoutVertices(packing, npoints=50)
      
      (
        ggplot() + 
          geom_polygon(data = dat_gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
          geom_text(data = coefs, aes(x, y, size=mean_adr, label = company)) +
          scale_size_continuous(range = c(1,4) , guide=F) + 
          scale_fill_discrete(guide=F) + 
          ggtitle("Which Companies Spend the Most ?") + 
          theme_minimal()+
          xlab('')+
          ylab("")+
          theme(axis.text = element_blank())
      )
      
    })
    
    output$trend <- renderPlot({
      company_trend <- df %>% inner_join(get_company_spends() , by="company") %>% group_by(company , arrival_date_month) %>% summarise(mean_adr = mean(adr))
      
      company_trend$arrival_date_month  <- company_trend$arrival_date_month  %>% factor( levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December")) 
      (
        ggplot(company_trend,
               aes(x=arrival_date_month , y=mean_adr , fill=arrival_date_month)) +
          geom_boxplot()+ 
          ggtitle("When do Companies spend more ?") + 
          scale_fill_discrete(guide=F)+
          theme_minimal()
      )
    })
   
  })
}