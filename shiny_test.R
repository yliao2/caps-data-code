library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sqldf)
library(chron)
library(forecast)
library(bda)
library(shiny)


crash <- read.csv("F:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("F:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("F:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("F:/caps/data/Highway.csv",sep=",", header=TRUE)


c <- crash %>% 
  mutate(CrashDate=as.Date(CrashDate,"%m/%d/%Y"),
         CrashTime=chron(times=CrashTime),
         Year=year(CrashDate),
         Day=day(CrashDate),
         Weekend = ifelse(DayOfWeekUSA %in% c(1, 2, 3, 4, 5), 1, 2),
         shift12 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="02:00:00"), 1, 
                   ifelse(CrashTime>=chron(times="02:00:00") & CrashTime<chron(times="04:00:00"), 2, 
                   ifelse(CrashTime>=chron(times="04:00:00") & CrashTime<chron(times="06:00:00"), 3, 
                   ifelse(CrashTime>=chron(times="06:00:00") & CrashTime<chron(times="08:00:00"), 4,
                   ifelse(CrashTime>=chron(times="08:00:00") & CrashTime<chron(times="10:00:00"), 5, 
                   ifelse(CrashTime>=chron(times="10:00:00") & CrashTime<chron(times="12:00:00"), 6, 
                   ifelse(CrashTime>=chron(times="12:00:00") & CrashTime<chron(times="14:00:00"), 7, 
                   ifelse(CrashTime>=chron(times="14:00:00") & CrashTime<chron(times="16:00:00"), 8,
                   ifelse(CrashTime>=chron(times="16:00:00") & CrashTime<chron(times="18:00:00"), 9, 
                   ifelse(CrashTime>=chron(times="18:00:00") & CrashTime<chron(times="20:00:00"), 10,
                   ifelse(CrashTime>=chron(times="20:00:00") & CrashTime<chron(times="22:00:00"), 11, 12))))))))))),
         shift6 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="04:00:00"), 1, 
                  ifelse(CrashTime>=chron(times="04:00:00") & CrashTime<chron(times="08:00:00"), 2, 
                  ifelse(CrashTime>=chron(times="08:00:00") & CrashTime<chron(times="12:00:00"), 3, 
                  ifelse(CrashTime>=chron(times="12:00:00") & CrashTime<chron(times="16:00:00"), 4,
                  ifelse(CrashTime>=chron(times="16:00:00") & CrashTime<chron(times="20:00:00"), 5, 6))))),
         shift3 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="08:00:00"), 1, 
                  ifelse(CrashTime>=chron(times="08:00:00") & CrashTime<chron(times="16:00:00"), 2, 3)),
         shift2 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="12:00:00"), 1, 2)) %>% 
  left_join(sqldf("select HighwayKey as HighwayID, Route as HWY_Route, logmile, 
                  Longitude as H_longitude, Latitude as H_latitude, AvgDailyTr, 
                  YearADT, TypeRoad, RouteSign, APHN, Access, TypeOperat, MedianType, 
                  NumberLane, roadid from highwaycrash"), by=c("HighwayID", "HWY_Route"))


route <- function(year, num)
{
  mydata <- subset(c, Year==year & HWY_Route %in% num)
  return(mydata)
}



#shiny app here-----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "year", label = "Year", choices = distinct(c, Year)),
                 selectInput(inputId = "route", label = "HWY Route", choices = distinct(c, HWY_Route)[order(distinct(c, HWY_Route)),]),
                 selectInput(inputId = "time", label = "type of time", 
                             choices = c("DayOfWeekUSA", "Month", "Day", "Weekend", "shift12",
                                         "shift6", "shift3", "shift2"))),
    mainPanel(plotOutput(outputId = "plot1", width = "100%"))))

server <- function(input, output)
{
  output$plot1 <- renderPlot({
    mydata <- route(input$year, input$route)
    ggplot(data = mydata, 
           mapping = aes(Logmile, col = factor(mydata[[input$time]]))) +
      geom_density(aes(y = ..count..)) + labs(colour = input$time)})
}

shinyApp(ui = ui, server = server)


