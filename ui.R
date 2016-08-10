library(shiny)
library(shinyBS)
library(DT)
library(leaflet)
library(rCharts)
library(dplyr)
library(dataRetrieval)
library(data.table)
library(ggplot2)
library(stringr)


# Load files for individual screens
QueryData<-source("external/QueryData.R",local=T)$value
CheckData<-source("external/CheckData.R",local=T)$value
Help<-source("external/Help.R",local=T)$value
ViewData <- source("external/ViewData.R", local = TRUE)$value

shinyUI(navbarPage("WQP STORET Data Discovery Tool", 
                   theme = "bootstrap.css",
                   inverse = TRUE,
                   QueryData(),
                   CheckData(),
                   ViewData(),
                   Help()
                            
))











