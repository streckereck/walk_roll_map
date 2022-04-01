library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)
library(tidyverse)
library(treemap)
library(tidytext)
library(wordcloud2)

source("setup.R")

ui <- fluidPage(
  titlePanel("Walk Roll Map Visualize"),
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
                 wordcloud2Output(outputId = "wordcloud"))
      )),
    
    mainPanel(
      fluidRow(
        column(4,
               selectInput(inputId = "spatial_subset",
                           label = "Spatial subset",
                           choices = c("All reports",
                                       "Map extent",
                                       "Capital Regional District (CRD)",
                                       "Langford",
                                       "Saanich",
                                       "Victoria"),
                           selected = "All reports")),
        column(4,
               dateRangeInput(inputId = "date_range_input",
                              label = "Date range",
                              start =  min(wrm$date),
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