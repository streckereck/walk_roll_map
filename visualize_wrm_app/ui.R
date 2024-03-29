library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(scales)
library(sf)
library(shiny)
library(tidyverse)
library(tidytext)
library(treemap)
library(wordcloud2)

source("setup.R")

ui <- fluidPage(
  # help from: https://stackoverflow.com/questions/60151766/adding-images-in-the-title-and-at-the-right-of-navbarpage
  
  tags$head(
    includeHTML(("google-analytics.html"))
  ),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "layers",
                         label = "Layers",
                         choices = c("Hazard / Concern",
                                     "Amenity",
                                     "Incident"),
                         selected = c("Hazard / Concern",
                                      "Amenity",
                                      "Incident")),
      tabsetPanel(
        tabPanel("Top reports",
                 plotOutput(outputId = "top_concerns")),
        tabPanel("All reports",
                 plotOutput(outputId = "treemap")),
        tabPanel("Descriptions",
                 wordcloud2Output(outputId = "wordcloud")),
        tabPanel("Download",
                 selectInput("format", "Choose a format:",
                             choices = c("csv", "kml", "gpkg")),
                 downloadButton(outputId = "downloadData", 
                                label = "Download"))
      )),
    
    mainPanel(
      fluidRow(
        column(4,
               selectInput(inputId = "spatial_subset",
                           label = "Spatial subset",
                           choices = extents,
                           selected = map_mode)),
        column(4,
               dateRangeInput(inputId = "date_range_input",
                              label = "Date range",
                              start =  "2021-01-01",
                              end = Sys.Date())
        ),
        column(4,
               fluidRow(actionButton(inputId = "all_reports",
                                     label = "All time")),
               fluidRow(actionButton(inputId = "this_year",
                                     label = "This year")))
        
      ),
      textOutput(outputId = "report_count"),
      fluidRow(leafletOutput(outputId = "leaflet_map")),
      fluidRow(plotOutput(outputId = "timebars",
                          height = 100)),
    )
  ))