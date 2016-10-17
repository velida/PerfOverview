#title:Stock Performance Overview for VOW, TM
#author: Elida Vaccea

library(shiny)
library(plotly)
library(forecast)
library(dplyr)

datasvw <- read.csv2("VW_20140324_20161013.csv", header=TRUE, sep=",", dec =".", na.strings="NA")
datasvw$Date <- as.Date(datasvw$Date,"%Y-%m-%d")
datasvw <- arrange(datasvw,Date)

datastm <- read.csv2("TM_20140324_20161013.csv", header=TRUE, sep=",", dec =".", na.strings="NA")
datastm$Date <- as.Date(datastm$Date,"%Y-%m-%d")
datastm <- arrange(datastm,Date)

# UI
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("VOW, TM stock price overview"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("radio", label = h4("Select stock:"),
                     choices = list("VOW" = 1, "TM" = 2), 
                     selected = 1),
        numericInput("numper", h4("Number of Prediction Periods:"),
                     value = 30, min = 0, max = 100, step = 1),
        dateInput("date1", label = h4("Start Date"), value = "2014-03-24"),
        dateInput("date2", label = h4("End Date"), value = "2017-10-06")
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        imageOutput("logo",height=50, width=50),
         br(),
         tabsetPanel(
                    tabPanel("Plot", plotlyOutput("plot")), 
                    tabPanel("Data Set", tableOutput("table")),
                    tabPanel("VW Fact Sheet", tags$iframe(style="height:400px; width:100%; scrolling=yes", src="vw.pdf"))
                    )
      )
  )
))


# Server logic 
server <- shinyServer(function(input, output) {
  #source function
  source("forecast.R")
  
  #select the dataset
  datas <- reactive({
    if (input$radio == 1){
      datas <- datasvw
      } else if (input$radio == 2){
        datas <- datastm
        }
  return(datas)
  })
    
  #result from forecasting model
  res <- reactive({
    resultfore <- forecastStl(datas(), n.ahead = input$numper)
    resultfore$date <- as.Date(resultfore$date,"%Y-%m-%d")
    return(resultfore)
  })
  #Filter the data frame according to the selected dates
  filtered <- reactive ({
    d1 <- as.Date(input$date1)
    d2 <- as.Date(input$date2)
    re <- filter(res(), between(date, d1, d2))
    return(re)
  })
  
 
  output$plot <- renderPlotly({
    filtered <- filtered()

    plot_ly(filtered, mode = "lines", x = filtered[,6], colors =c('#8dd3c7','#1f78b4','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')) %>%
      add_lines(y=filtered[,1], name ="Closing Price", color = "Actual") %>%
      add_lines(y=filtered[,2], name ="Trend", color = "Trend") %>%
      add_lines(y=filtered[,3], name ="Prediction", color = "Prediction") %>%
      add_lines(y=filtered[,4], name ="Lower Bound", color = "Lower bound") %>%
      add_lines(y=filtered[,5], name ="Upper Bound", color = "Upper bound") %>%
      layout(
        title = "Stock overview 2014/03/24 - 2016/10/13",
        xaxis = list(
          rangeselector = list(
            y = 0,
            buttons = list(
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(step = "all"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Stock price"))
  })
  
  
  # Table Slide
  output$table <- renderTable({
    filt <- filtered()
    filt$date <- as.character(filt$date)
    data.frame(filt)
  })
  
  #Logo
  output$logo <- renderImage({
    if (input$radio == 1){
      return(list(
        src = "images/VOW.png",
        contentType = "image/png",
        alt = "VOW",
        width = "55px",
        height = "50px"
      ))
    } else if (input$radio == 2){
      return(list(
        src = "images/TM.jpg",
        contentType = "image/jpg",
        alt = "TM",
        width = "55px",
        height = "50px"
      ))
    }
  }, deleteFile = FALSE)
}
)

# Run the application 
shinyApp(ui = ui, server = server)

