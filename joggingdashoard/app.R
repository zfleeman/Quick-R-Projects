library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(lubridate)

age <- round(as.numeric(as.character(Sys.Date(), "%Y")) + round(yday(Sys.Date())/365,2) - 1989.2, 2)
weight <- 169.5

miles <- read.csv("https://docs.google.com/spreadsheets/d/1xe4rePemIs319-zKM49bO5eE5CVeacOjhSwpF7IgfH0/pub?output=csv", stringsAsFactors = FALSE)
miles$Day <- as.Date(miles$Day, "%m/%d/%Y")
miles$Minutes <- miles$Minutes + (miles$Seconds/60)
miles$permile <- miles$Minutes/miles$Distance..miles.
miles$permile_char <- paste(floor(miles$permile),":",ifelse(100 - floor((miles$permile - floor(miles$permile))*60) > 90,"0",""), floor((miles$permile - floor(miles$permile))*60), sep="")
miles$month <- as.Date(cut(miles$Day, breaks = "1 month"))
miles <- miles[order(miles$Day, decreasing = TRUE),]
colnames(miles) <- c("Timestamp","Date","Time","Miles","Minutes","Seconds","Rating","Location", "HR","Per.Mile.Decimal","Per.Mile","month")
miles$Minutes <- round(miles$Minutes, digits = 2)
miles <- subset(miles, Miles !=0)
row.names(miles) <- NULL
miles$Day <- as.character(miles$Date, "%b %d, %Y")

monthly_plot <- subset(miles, month > Sys.Date() - 365)

monthagg <- aggregate(data = monthly_plot, Miles ~ month, FUN = "sum")
currMonthMiles <- monthagg$Miles[nrow(monthagg)]

#    MILE CALCS
thisweek <- subset(miles, Date >= Sys.Date() - 7)
thisweekmiles <- sum(thisweek$Miles)
thisweekstatus <- ifelse(thisweekmiles < 12, "red", "green")
thisweekicon <- ifelse(thisweekstatus == "red", "thumbs-o-down", "thumbs-o-up")

#    MOOD CALCS
thisweekmood <- mean(thisweek$Rating)
moodicon <- ifelse(thisweekmood >= 3.5, "smile-o", ifelse(thisweekmood < 3.5 & thisweekmood > 2.5, "meh-o", "frown-o"))
moodstatus <- ifelse(thisweekmood >= 3.5, "green", ifelse(thisweekmood < 3.5 & thisweekmood > 2.5, "yellow", "red"))

#    PER MILE CALCS
avgpermile <- mean(thisweek$Per.Mile.Decimal)
avgpermile_readable <- paste(floor(avgpermile),":",ifelse(100 - floor((avgpermile - floor(avgpermile))*60) > 90,"0",""), floor((avgpermile - floor(avgpermile))*60), sep="")
#permileicon <- ifelse(avgpermile <= 8.5, "smile-o", ifelse(avgpermile < 9 & avgpermile > 8.5, "meh-o", "frown-o"))
permilestatus <- ifelse(avgpermile <= 8.5, "green", ifelse(avgpermile < 9 & avgpermile > 8.5, "yellow", "red"))

#    CALORIES
thisweek$calories <- (((age*0.2017)-(weight*0.09036)+(thisweek$HR*0.6309)-55.0969)*thisweek$Minutes)/4.184

#    ZIP CODES
zipcodes <- read.csv("https://gist.githubusercontent.com/erichurst/7882666/raw/5bdc46db47d9515269ab12ed6fb2850377fd869e/US%2520Zip%2520Codes%2520from%25202013%2520Government%2520Data")
miles$Location <- as.integer(miles$Location)

jogged_zips <- data.frame(table(miles$Location))

jogged_zips <- merge(jogged_zips, zipcodes, by.x = "Var1", by.y = "ZIP", all.x = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Jogging Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Miles & Metrics", tabName = "metrics", icon = icon("line-chart"), selected = TRUE),
      menuItem("Health Measures", tabName = "health", icon = icon("heartbeat")),
      menuItem("Locations", tabName = "locations", icon = icon("globe")),
      menuItem("Back to homepage", href = "http://zfleeman.com", icon = icon("home"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "metrics",
        fluidRow(
          valueBoxOutput("monthly", width = 3),
          valueBoxOutput("weekly", width = 3),
          valueBoxOutput("permile", width = 3),
          valueBoxOutput("mood", width = 3)
        ),
        fluidRow(
            box(plotOutput("frequency", height = 600), width = 7),
            column(5,
              box(tableOutput("jogs"), width = 12),
              box(title = "Speed vs. Mood Chart", plotOutput("moodvspeed", height = 400), collapsed = TRUE, collapsible = TRUE, width = 12)
            )
          )
        ),
      tabItem(tabName = "locations",
              box(plotOutput("map"))
        ),
      tabItem(tabName = "health", 
        fluidRow(
          valueBoxOutput("caloriesweeklyvalue", width = 3),
          valueBoxOutput("activeminutes", width = 3),
          valueBoxOutput("age", width = 3),
          valueBoxOutput("weight", width = 3)
        ),
        fluidRow(
          box(plotOutput("caloriesweekly"), title = "Calories Burned in the Past Seven Days", width = 8)
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$frequency <- renderPlot({
    ggplot(data = monthly_plot, aes(x = month, y = Miles)) + stat_summary(fun.y = "sum", geom = "bar") + labs(title="Miles Jogged per Month // One Year", x = "", y = "Miles") + scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$jogs <- renderTable({
    head(miles[,c("Day","Miles","Minutes","Per.Mile","Rating","Location")])
  }, width = "auto")
  
  output$monthly <- renderValueBox(valueBox(currMonthMiles, "miles jogged this month.", icon = icon("calendar")))

  output$weekly <- renderValueBox(valueBox(thisweekmiles, "miles jogged in the past 7 days.", icon = icon(thisweekicon), color = thisweekstatus))
  
  output$mood <- renderValueBox(valueBox(round(thisweekmood,2), "average jog rating /5 in the past 7 days.", icon = icon(moodicon), color = moodstatus))
  
  output$permile <- renderValueBox(valueBox(avgpermile_readable, "average pace in the past 7 days.", icon = icon("clock-o"), color = permilestatus))

  output$moodvspeed <- renderPlot({
    mood <- subset(miles, Date > Sys.Date()-30 & Miles != 0 & !is.na(Per.Mile.Decimal))
    par(mar = c(5,5,2,5))
    plot(mood$Date, mood$Per.Mile.Decimal, type = "l", col = "blue", xlab = "", ylab = "Minutes Per Mile")
    par(new=T)
    plot(mood$Date,mood$Rating, type = "l", col = "red", ylab = "", xlab = "", axes=F)
    axis(side = 4, at = c(1,2,3,4,5))
    mtext(side = 4, line = 3, 'Run Rating')
    legend("topleft", legend = c("Per Mile", "Raiting"),col = c("blue","red"), lty=c(1,1))
  })
  
  output$caloriesweeklyvalue <- renderValueBox(valueBox(round(sum(thisweek$calories, na.rm = TRUE), 0), "calories burned this week.", icon = icon("heartbeat"), color = "fuchsia"))
  
  output$age <- renderValueBox(valueBox(age, "years old.", icon = icon("calendar-plus-o")))
  
  output$activeminutes <- renderValueBox(valueBox(sum(thisweek$Minutes), "active minutes.", icon = icon("clock-o")))
  
  output$weight <- renderValueBox(valueBox(weight, "pounds.", icon = icon("cutlery")))
  
  output$caloriesweekly <- renderPlot({
    ggplot(data = thisweek, aes(x = Date, y = calories)) + stat_summary(fun.y = "sum", geom = "bar") + labs(y = "Calories", x = "")
  })
  
  output$map <- renderPlot({
    qmplot(LNG, LAT, data = jogged_zips, color = "red", maptype = "toner-lite", size = Freq)
  })
  
  }

shinyApp(ui, server)

