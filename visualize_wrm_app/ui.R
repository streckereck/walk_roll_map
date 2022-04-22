library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(plotly)
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
    tags$link(rel = "shortcut icon", type = "image/png", href = "wrm_logo.png"),
    tags$title("Walk Roll Map Visualize")
  ),
  navbarPage(
    tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://walkrollmap.org/\"><img src=\"wrm_logo.png\" alt=\"Walk Roll Map Logo\" style=\"float:right;width:35px;height:41px;padding-top:5px;padding-left:5px;\"> </a></div>');
    console.log(header)")
    ),
    tags$script(HTML("var header = $('.navbar > .container-fluid');
 header.append('<div style=\"float:right\"><h4>data from <a href=\"https://walkrollmap.org/\">walkrollmap.org </h4></a></div>');"
    )),
    title = "Walk Roll Map Visualize"
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
                           choices = c("All reports",
                                       "Map extent",
                                       "Capital Regional District (CRD)",
                                       "Langford",
                                       "Saanich",
                                       "Victoria",
                                       "View Royal"),
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
      fluidRow(plotlyOutput(outputId = "timebars",
                          height = 100)),
    )
  ))