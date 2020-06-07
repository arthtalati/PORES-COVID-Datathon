library(plotly)
library(data.table)
library(tidyr)
library(leaflet)
library(forecast)
library(rgdal)
library(leaflet.extras)



# Loading data
timeseriesconfirmed <- read.csv("time_series_covid19_confirmed_global.csv")
timeseriesdead <- read.csv("time_series_covid19_deaths_global.csv")
timeseriesrecovered <- read.csv("time_series_covid19_recovered_global.csv")


leaflet() %>% 
  addTiles() %>%
  #addProviderTiles("Esri.WorldStreetMap") %>% 
  addCircleMarkers(~Long, 
             ~Lat, 
             data = timeseriesconfirmed,
             radius = 5, 
             weight = 0.2,
             stroke = TRUE,
             color = "navy",

             popup = paste("State:", 
                           timeseriesconfirmed$`Province.State`, 
                           "<br>",
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

