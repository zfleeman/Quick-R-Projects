age <- 27
weight <- 169 # in Pounds
heart_rate <- 152
duration_of_activity <- 32.25 # in minutes
MET_value <- 12 #running, 7 mph (8.5 min/mile) # there is a table of these values we can leverage


library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Calorie Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("MET", "METs", min = 1, max = 20, value = 10),sliderInput("age", "age:", min = 1, max = 40, value = 27),sliderInput("heart_rate", "ending heart rate:", min = 70, max = 200, value = 152),sliderInput("duration_of_activity", "howlong:", min = 1, max = 100, value = 32),sliderInput("weight","weight",min = 50, max = 400,value = weight)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h3("Equation 1"),textOutput("eq1"),h3("Equation 2"),textOutput("eq2")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$eq1 <- renderText({
     Calories_Burned<-(((input$age*0.2017)-(input$weight*0.09036)+(input$heart_rate*0.6309)-55.0969)*input$duration_of_activity)/4.184
     Calories_Burned
   })
   
   output$eq2 <- renderText({
     calories_burned_MET<-input$MET*(input$weight/2.2)*(input$duration_of_activity/60)
     calories_burned_MET
   })
   
})

# Run the application 
shinyApp(ui = ui, server = server)

