


adrRegressionUI <- function(id){
  
  # Show a plot of the generated distribution

  fluidPage(
    fluidRow(
      column(8,fluidRow(
        tags$h3("Average Daily Rate Regression")
        ,"Average Daily Rate as defined by dividing the sum of all lodging transactions by the total number of staying nights")
      )
    ),
    fluidRow(tags$hr()),
    fluidRow(
      column(3, fluidRow(
        fluidRow(sliderInput(NS(id,'topn'), "Top N Most Impactful:",
                            min = 10, max = 20, value = 10)),
        fluidRow(sliderInput(NS(id,'min_dollar_impact'), "Minimum Absolute $ ADR:",
                             min = 10, max = 100,step=20, value = 10)),
        fluidRow(checkboxInput(NS(id,'positive_only'), "Positive Impact Only?", FALSE))
        ,
      )),
      column(9,plotOutput(NS(id,'most_impact')))
    ),
    fluidRow(tags$hr()),
    fluidRow(
      column(4, plotOutput(NS(id,'room_type'))),
      column(4,plotOutput(NS(id,'month'))),
      column(4,plotOutput(NS(id,'family')))
    )
  )
}

adrRegressionUIServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    plot_bar_chart <- function(df , title ,text_size=22){
      g <- (
        ggplot(df
               ,aes(x=variable, y=estimate , fill= estimate > 0)) +
          geom_col() + 
          scale_fill_discrete(guide=F) +
          coord_flip() +
          theme_light() +
          ggtitle(title) +
          ylab("ADR in $") + 
          theme(plot.title = element_text(size=text_size))
      )
      g
    }
    
    df %>% select(-one_of(c("company" , "agent"))) %>% colnames()
    linearmodel <- lm(adr ~ . , df %>% select(-one_of(c("company" , "agent"))))
    lm_summary <- summary(linearmodel)
    coefs <- lm_summary$coefficients %>% as.data.frame() 
    colnames(coefs) <- c("estimate" , "stderror" , "tval" , "pval")
    coefs <- coefs %>% filter(pval < 0.05)
    coefs$variable <- rownames(coefs)
    
    output$room_type <- renderPlot({
      room_type_coef <- coefs %>% filter(grepl("assigned_room",variable))
      room_type_coef$variable <- str_replace(room_type_coef$variable , "assigned_room_type" , "")
      
      plot_bar_chart(room_type_coef,"Impact of Room Type on ADR" ,18)

    })
    
    output$month <- renderPlot({
      month_coefs <- coefs %>% filter(grepl("arrival_date_month",variable))
      month_coefs$variable <- str_replace(month_coefs$variable , "arrival_date_month" , "")
      month_coefs$variable <- month_coefs$variable %>% factor( levels=c("January", "February", "March", "April","May","June","July","August","September","October","November","December")) 
      
      plot_bar_chart(month_coefs,"Monthly Impact on ADR" , 18)

    })
    
    output$family <- renderPlot({
      family_coefs <- coefs %>% filter(variable %in% c("adults" , "children","mealFB" , "mealHB" , "mealSC"))
      
      plot_bar_chart(family_coefs,"Family Size and Meals Impact", 18)
    })
    
    output$most_impact <- renderPlot({
      most_impact_coefs <- coefs %>% 
                              arrange(abs(estimate))  %>% 
                              filter(variable != "(Intercept)") %>% 
                              filter(abs(estimate) > input$min_dollar_impact)
                              
      if(input$positive_only){
        most_impact_coefs <- most_impact_coefs %>% filter(estimate > 0)
      }
      
      most_impact_coefs <- most_impact_coefs %>% tail(input$topn)
      
      plot_bar_chart(most_impact_coefs,"Most Impactful Variables for ADR", 22)
    })
  })
}