df <- readr::read_csv('hotels.csv')

library(rpart)
library(rpart.plot)

decisionTreeUI <- function(id){
  
  # Show a plot of the generated distribution
  fluidPage(
      
    fluidRow(
      column(10,
        fluidRow(
          column(12 ,tags$h2("Decision Tree"))
        )       
      )
    ),
    fluidRow(
      column(12,uiOutput(NS(id,'checkbox')))
    ),
    fluidRow(
      column(12,tags$h3("Is Cancelled?"))
    ),
    fluidRow(
      column(10,plotOutput(NS(id,'orig_plot')))
      
    )
    ,
    fluidRow(
      fluidRow(tags$h3("People are likely to cancel if :")),
      column(12,tags$ul(
        tags$li("they booked way too long ago "),
        tags$li("made very few special requests"),
        tags$li("made very few booking changes")
      ))
      
    )
  )
}

decisionTreeServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    output$checkbox <- renderUI({
      vars <- c("total_of_special_requests" , "required_car_parking_spaces" , "booking_changes" ,"is_repeated_guest" , "adults","lead_time" , "previous_cancellations","days_in_waiting_list" , "adr" )
      checkboxGroupInput(NS(id , "checkbox"),"Select Variables", choices=vars ,selected=vars, inline=T)
    })
    
    output$orig_plot <- renderPlot({
      sdf <- df %>% select_at(all_of(c(input$checkbox , "is_canceled")))
      print(input$checkbox)
      sdf$is_canceled <- if_else(sdf$is_canceled == 0, "no", "yes") %>% as.factor()
      fit <- rpart(is_canceled ~ ., method="class", data=sdf)
      rpart.plot(fit,type=4, box.palette = c("#95d5b2", "#f28482"))
    })
  })
  
}