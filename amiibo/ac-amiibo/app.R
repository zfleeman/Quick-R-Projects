library(shiny)
library(RMySQL)
options(mysql = list(
  "host" = "zfleeman.com",
  "port" = 3306,
  "user" = "amiiboapp",
  "password" = ""
))

db <- dbConnect(MySQL(), dbname = "amiibo", host = options()$mysql$host, 
                port = options()$mysql$port, user = options()$mysql$user, 
                password = options()$mysql$password)


table_series <- "series14"
table_welcome <- "welcomeamiibo"
table_figures <- "figures"

#lists for the initial load
series14list <- c("...")
welcomeamiibolist <- c("...")
figurelist <- c("...")

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    titlePanel("Animal Crossing amiibo Scan Log"),
    sidebarLayout(
      sidebarPanel(
        textInput("username","", placeholder = "Enter Username", width = 150),
        actionButton("retrieve","Get Collection"),
        hr(),
        selectInput("series14","Series 1-4 left to scan", series14list),
        selectInput("welcomeamiibo", "Welcome amiibo left to scan", welcomeamiibolist),
        selectInput("amiibofigures", "amiibo Figures left to scan", figurelist),
        actionButton("scanbutton", "Scan!")
        ),
      mainPanel(
        tableOutput("seriestable")
      ))
    ))

server <- shinyServer(function(input, output){
  observeEvent(input$retrieve,{
    un <- input$username
    statement <- sprintf("SELECT * FROM series14 WHERE user = '%s'",un)
    series <- dbGetQuery(db, statement)
    l <- c("th", "rd", "nd", "st")
    for(i in 1:length(l)){
      series$birthday <- gsub(l[i], "", series$birthday)
    }
    series$birthday <- gsub(" ", "", series$birthday)
    series$thisyearbirthday <- as.Date(series$birthday, format = "%b%d")
    output$seriestable <- renderTable(dbGetQuery(db, statement))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)