server <- function(input, output) {
  observe({
    if(input$all_reports){
      updateDateRangeInput(inputId = "date_range_input",
                           start = min(wrm$date),
                           end = max(wrm$date))
    }
  })
  
  observe({
    if(input$this_year){
      updateDateRangeInput(inputId = "date_range_input",
                           start = Sys.Date() - as.difftime(365, unit="days"),
                           end = Sys.Date())
    }
  })
  
  filteredData <- reactive({
    selected_data <- wrm_spatial
    spatial_subset_name <- input$spatial_subset
    layer_subset <- input$layers
    
    selected_data <- selected_data %>%
      filter(date > input$date_range_input[1] &
               date < input$date_range_input[2])

    
    if(spatial_subset_name %in% "All reports"){
      map_hide_boundary()
      map_zoom_exent(selected_data)
    } else if(spatial_subset_name %in% "Map extent"){
      map_hide_boundary()
        selected_data <- selected_data %>%
          st_intersection(map_extent())
    } else {
      boundary <- geographic_presets %>% filter(name %in% spatial_subset_name)
      selected_data <- selected_data %>%
        st_intersection(boundary)
      map_hide_boundary()
      map_zoom_exent(boundary)
      map_show_boundary(boundary)
    }

    if(! "Hazard / Concern" %in% layer_subset){
      selected_data <- selected_data %>%
        filter(! type %in% "hazard-concern")
    }

    if(! "Amenity" %in% layer_subset){
      selected_data <- selected_data %>%
        filter(! type %in% "amenity")
    }

    if(! "Incident" %in% layer_subset){
      selected_data <- selected_data %>%
        filter(! type %in% "incident")
    }
    return(selected_data)
  })
  
  map_extent <- reactive({
    if(! is.null(input$leaflet_map_bounds)){
      coordlist <- unlist(input$leaflet_map_bounds)
    } else {
      coordlist <- c(-90, -180, 90, 180)
    }
    
    
    map_extent_coords <- data.frame(lat = c(coordlist[1], coordlist[3]),
                                    lon = c(coordlist[2], coordlist[4]))
    
    extent <- map_extent_coords %>%
      st_as_sf(coords = c("lon", "lat"),
               crs = 4326) %>%
      st_bbox() %>%
      st_as_sfc()
    
    return(extent)
  })
  
  output$report_count <- renderPrint({
    cat("Number of reports: ", nrow(filteredData()))
  })
  
  output$treemap <- renderPlot({
    # arrange colour pallete
    cols <- c(colour_palette[1],
              colour_palette[2],
              colour_palette[3])
    
    if(! "Hazard / Concern" %in% input$layers){
      cols <- cols[-1]
    } 
    
    if(! "Amenity" %in% input$layers){
      cols <- cols[-2]
    } 
    
    if(! "Incident" %in% input$layers){
      cols <- cols[-3]
    }
    
    if(nrow(filteredData()) > 0){
      treemap(filteredData() %>%
                st_drop_geometry() %>%
                arrange(type) %>%
                group_by(type, feature_type, feature_subtype) %>%
                summarise(reports = n()),
              index=c("type","feature_type", "feature_subtype"),
              vSize="reports",
              type="index",
              palette = cols,
              bg.labels=100,
              align.labels=list(
                c("center", "center"),
                c("right", "bottom")
              )
      )
    }
  })
  
  output$timebars <- renderPlot({
    # all reports in the background in gray,
    # visible reports in the foreground in black
    reports_in_last_year <- which(as.integer(as.character(wrm$days_since_report)) <= 365)
    
    report_table <- function(reports){
      doy <- table(reports$days_since_report %>%
                     factor(levels = as.character(1:365 %>% str_pad(width = 3,
                                                                    pad = 0))))
      doy <- data.frame(doy)
      names(doy) <- c("days_since_report", "Count")
      doy$days_since_report_int <- doy$days_since_report %>%
        as.character() %>%
        as.integer()
      
      return(doy)
    }
    
    all_reports_in_last_year <- report_table(wrm[reports_in_last_year, ]) %>%
      rename(count_all = Count)
    
    visible_reports_in_last_year <- report_table(filteredData() %>%
                                                   st_drop_geometry()) %>%
      rename(count_visible = Count)
    
    all_reports <- all_reports_in_last_year %>%
      select(days_since_report, count_all) %>%
      left_join(visible_reports_in_last_year %>% select(days_since_report,
                                                        count_visible),
                by = "days_since_report")
    
    all_reports_long <- all_reports %>%
      mutate(count_not_visible = count_all - count_visible) %>%
      select(days_since_report, count_visible, count_not_visible) %>%
      pivot_longer(!days_since_report,
                   names_to = "Visible",
                   values_to = "Count")
    
    time_graph <- ggplot(all_reports_long,
           aes(x = days_since_report,
               y = Count,
               fill = Visible)) +
      geom_bar(stat = "identity",
               position = "stack") +
      scale_fill_manual(values= rev(colour_palette_tf)) +
      theme_minimal() +
      theme(legend.position="none",
            axis.title.x = element_blank()) +
      scale_x_discrete(name = "date",
                       breaks = breaks,
                       labels = labels,
                       limits=rev) +
      ggtitle("Reports in the last year")
    
    return(time_graph)
  })
  
  output$leaflet_map <- renderLeaflet({
    leaflet(wrm_spatial) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(lng1 = st_bbox(wrm_spatial)$xmin %>% as.numeric(), 
                lat1 = st_bbox(wrm_spatial)$ymin %>% as.numeric(), 
                lng2 = st_bbox(wrm_spatial)$xmax %>% as.numeric(), 
                lat2 = st_bbox(wrm_spatial)$ymax %>% as.numeric())
    })
  
  observe({
    if(nrow(filteredData()) > 0){
      cols <- colorFactor(palette = colour_palette[c(2,1,3)],
                          domain = c(wrm_levels))
      
      leafletProxy(mapId = "leaflet_map", 
                   data = filteredData()) %>%
        clearGroup("circleMarkers") %>%
        addCircleMarkers(
          data = filteredData(),
          label = ~as.character(str_to_sentence(type)),
          popup = ~as.character(descriptions),
          fillOpacity = 0.5,
          color = ~cols(type),
          clusterOptions = markerClusterOptions(),
          group = "circleMarkers")
    } else {
      leafletProxy(mapId = "leaflet_map", 
                   data = filteredData()) %>%
        clearGroup("circleMarkers")
    }
    
  })
  
  map_show_boundary <- function(boundary){
    leafletProxy(mapId = "leaflet_map") %>%
      addPolygons(
        data = boundary,
        color = "#40e0d0",
        fill = NULL,
        opacity = 0.8,
        group = "boundary")
  }
  
  map_hide_boundary <- function(){
    leafletProxy(mapId = "leaflet_map") %>%
      clearGroup("boundary")
  }
  
  map_zoom_exent <- function(boundary){
    boundary_bbox <- st_bbox(boundary)
    
    leafletProxy(mapId = "leaflet_map") %>%
      fitBounds(lng1 = boundary_bbox$xmin %>% as.numeric(), 
                lat1 = boundary_bbox$ymin %>% as.numeric(), 
                lng2 = boundary_bbox$xmax %>% as.numeric(), 
                lat2 = boundary_bbox$ymax %>% as.numeric())
  }
  
  output$wordcloud <- renderWordcloud2({
    text <- filteredData() %>%
      st_drop_geometry() %>%
      select(description) %>%
      unnest_tokens(word, description) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)
    
    if(nrow(text) > 100){
      text <- top_n(text, 100)
    }
    
    if(nrow(text) > 10){
      wordcloud2a(text)
    }
    
  })
  
  output$top_concerns <- renderPlot({
    if(nrow(filteredData()) > 0){
      reports <- filteredData() %>%
        st_drop_geometry() %>%
        mutate(detail = ifelse((is.na(feature_subtype) |
                                  feature_subtype %in% "other"),
                               feature_type,
                               feature_subtype)) %>%
        group_by(detail) %>%
        summarise(issue_count = n()) %>%
        top_n(5) %>%
        mutate(detail = str_to_sentence(detail)) %>%
        rename(Report = detail,
               Count = issue_count) %>%
        mutate(Report = reorder(Report, Count)) %>%
        arrange(-Count)
      
      
      report_graph <- reports %>%
        ggplot(aes(x = Count,
                   y = Report)) +
        geom_col(fill = "black") +
        labs(y = NULL) +
        theme_minimal()
      
      return(report_graph)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("walk_roll_map_",
            as.character(Sys.Date()),
            ".",
            input$format, sep = "")
    },
    content = function(file) {
      if (input$format == "csv"){
        write.csv(x = filteredData() %>%
                    st_drop_geometry() %>%
                    select(-c(doy, days_since_report, descriptions)), 
                  file = file,
                  row.names = F)
      } else if (input$format == "kml"){
        st_write(filteredData() %>%
                   select(Name = type,
                          Description = descriptions),
                 dsn = file,
                 driver = "KML")
      } else if (input$format == "gpkg"){
        st_write(filteredData() %>%
                   select(-c(doy, days_since_report, descriptions, lat, lon)),
                 dsn = file,
                 driver = "GPKG")
      }

    }
  )
}