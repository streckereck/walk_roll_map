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
                           start = now() - as.difftime(365, unit="days"),
                           end = now())
    }
  })
  
  filteredData <- reactive({
    
    selected_data <- wrm_spatial %>%
      filter(date > input$date_range_input[1] &
               date < input$date_range_input[2])
    
    if(input$spatial_subset %in% "Map extent"){
      selected_data <- selected_data %>%
        st_intersection(map_extent())
    } else if(input$spatial_subset %in% "Capital Regional District (CRD)"){
      selected_data <- selected_data %>%
        st_intersection(crd)} 
    else if(input$spatial_subset %in% "Langford"){
      selected_data <- selected_data %>%
        st_intersection(langford)
    } else if(input$spatial_subset %in% "Saanich"){
      selected_data <- selected_data %>%
        st_intersection(saanich)
    } else if(input$spatial_subset %in% "Victoria"){
      selected_data <- selected_data %>%
        st_intersection(victoria)
    }
    
    if(! "Hazard / Concern" %in% input$layers){
      selected_data <- selected_data %>%
        filter(! type %in% "hazard-concern")
    } 
    
    if(! "Amenity" %in% input$layers){
      selected_data <- selected_data %>%
        filter(! type %in% "amenity")
    } 
    
    if(! "Incident" %in% input$layers){
      selected_data <- selected_data %>%
        filter(! type %in% "incident")
    }
    return(selected_data)
  })
  
  map_extent <- reactive({
    coordlist <- unlist(input$leaflet_map_bounds)
    
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
    
    ggplot(all_reports_long,
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
  })
  
  output$leaflet_map <- renderLeaflet({
    # default centre on Victoria
    lat_centre <- 48.4295352
    lon_centre <- -123.3596742
    default_zoom = 12
    
    leaflet(wrm_spatial) %>% 
      setView(lng = lon_centre, 
              lat = lat_centre, 
              zoom = default_zoom) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) 
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
        rename(Report = detail,
               Count = issue_count) %>%
        mutate(Report = reorder(Report, Count)) %>%
        arrange(-Count)
      
      
      reports %>%
        ggplot(aes(x = Count,
                   y = Report)) +
        geom_col(fill = "black") +
        labs(y = NULL) +
        theme_minimal()
    }
  })
}