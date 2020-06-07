library(plotly)
library(data.table)
library(tidyr)
library(leaflet)
library(shiny)
library(forecast)
library(shinythemes)
# Loading data
timeseriesconfirmed <- read.csv("time_series_covid19_confirmed_global.csv")
timeseriesdead <- read.csv("time_series_covid19_deaths_global.csv")
timeseriesrecovered <- read.csv("time_series_covid19_recovered_global.csv")
server <- function(input, output) {
  map<-leaflet() %>% 
    addTiles() %>%
    #addProviderTiles("Esri.WorldStreetMap") %>% 
    addCircleMarkers(~Long, 
                     ~Lat, 
                     data = timeseriesconfirmed,
                     radius = 5, 
                     weight = 0.2,
                     stroke = TRUE,
                     color = "navy",
                     popup = paste(
                                   "Country:", 
                                   timeseriesconfirmed$`Country.Region`,
                                   "<br>",
                                   "Total Confirmed Cases:", 
                                   timeseriesconfirmed[,ncol(timeseriesconfirmed)],
                                   "<br>",
                                   "Total Deaths:", 
                                   timeseriesdead[,ncol(timeseriesdead)],
                                   "<br>",
                                   "Total Recovered Cases:", 
                                   timeseriesrecovered[,ncol(timeseriesrecovered)]))
  # output the map
  output$map <- renderLeaflet(map)
  #  timeseriesconfirmed <- read.csv("time_series_covid19_confirmed_global.csv")
  #  timeseriesdead <- read.csv("time_series_covid19_deaths_global.csv")
  #  timeseriesrecovered <- read.csv("time_series_covid19_recovered_global.csv")
  # Function to find rotal for every country
  country_total <- function(df){
    sums <- colSums(df[,5:ncol(df)])
    sums <- as.integer(sums)
    levels(df[,1]) <- c(levels(df[,1]),"Total")
    levels(df[,2]) <- c(levels(df[,2]),"Total")
    totalsums <- data.frame(Province.State = "Total", Country.Region = "Total", Lat = 30.97560, Long = 112.270700,t(colSums(df[,5:ncol(df)])))
    df[nrow(df)+1,] <- totalsums
    return(df)}
  timeseriesconfirmed <- country_total(timeseriesconfirmed)
  timeseriesdead <- country_total(timeseriesdead)
  timeseriesrecovered <- country_total(timeseriesrecovered)
  data <- reactive({
    country <- as.character(input$country)
    subsetdata <- timeseriesconfirmed[timeseriesconfirmed$Country.Region == country,]
    totalconfirmed <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesdead[timeseriesdead$Country.Region == country,]
    totaldead <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesrecovered[timeseriesrecovered$Country.Region == country,]
    totalrecovered <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    total <- cbind(totalconfirmed,totaldead,totalrecovered)
    colnames(total) <- c("Confirmed", "Dead", "Recovered")
    rownames(total) <- 1:nrow(total)
    setDT(total, keep.rownames = TRUE)[]
    totalgathered <- total
    totalgathered <- gather(total,"Status", "Amount",-rn)
    totalgathered$Status <- as.factor(totalgathered$Status)
    data <- totalgathered
  })
  fit <- reactive({
    country <- as.character(input$country)
    subsetdata <- timeseriesconfirmed[timeseriesconfirmed$Country.Region == country,]
    totalconfirmed <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesdead[timeseriesdead$Country.Region == country,]
    totaldead <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    subsetdata <- timeseriesrecovered[timeseriesrecovered$Country.Region == country,]
    totalrecovered <- as.data.frame(colSums(subsetdata[,5:ncol(subsetdata)]))
    total <- cbind(totalconfirmed,totaldead,totalrecovered)
    colnames(total) <- c("Confirmed", "Dead", "Recovered")
    if (input$status == "Confirmed"){timeseries <- ts(totalconfirmed)}
    if (input$status == "Dead"){timeseries <- ts(totaldead)}
    if (input$status == "Recovered"){timeseries <- ts(totalrecovered)}
    fit <- ets(timeseries)
  })
  # Plot for specific country
  output$CountryPlot <- renderPlotly({
    plot_ly(data(), y = data()$Amount, color = ~Status, type = "scatter", mode = "lines") %>% layout(
      title = as.character(input$country), xaxis = list(title = "Days Since January 22nd"),
      yaxis = list(title = "Count"))
  })
  # Forecast plot
  output$Forecast <- renderPlot({
    plot(forecast(fit(), 10), type = "l", main = c("Forecast",as.character(input$status)), ylab = "Cases", xlab = "Days Since January 22nd", col = "red",lwd = 2)
  })
}

library(plotly)
ui <- shinyUI(  fluidPage(    
  theme = shinythemes::shinytheme("superhero"),
  navbarPage("Coronavirus outbreak",
  
  
  tabPanel("Forecasting",
  titlePanel("Coronavirus cases by country"),
  
  sidebarLayout(      
    sidebarPanel(
      selectInput("country", "Country to Display:", 
                  choices=levels(timeseriesconfirmed$Country.Region)),
      selectInput("status", "Status to Forecast:", 
                  choices=c("Confirmed", "Dead", "Recovered")),
      hr(),
      
    ),
    # Barplot
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Country Data", plotlyOutput("CountryPlot")),
                  tabPanel("Forecast", plotOutput("Forecast"))
      )
    ))
  ),
  tabPanel("Map",
           h2("Map showing country wise Coronavirus cases in the world"),
          leafletOutput("map")))))


shinyApp(ui = ui, server = server)