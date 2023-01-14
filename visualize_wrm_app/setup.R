# load current data
hazard <- jsonlite::fromJSON("https://walkrollmap.org/api/hazard",
                              flatten = T,
                              simplifyDataFrame = T) %>%
    as.data.frame() %>%
  select(
    id = features.properties.id,
    date = features.properties.date,
    type = features.properties.type,
    feature_type = features.properties.hazard_type,
    feature_subtype = features.properties.hazard_subtype,
    description = features.properties.description,
    geometry = features.geometry.coordinates)

amenity <- jsonlite::fromJSON("https://walkrollmap.org/api/amenity",
                             flatten = T,
                             simplifyDataFrame = T) %>%
  as.data.frame() %>%
  mutate(feature_subtype = NA) %>%
  select(
    id = features.properties.id,
    date = features.properties.date,
    type = features.properties.type,
    feature_type = features.properties.amenity_type,
    feature_subtype,
    description = features.properties.description,
    geometry = features.geometry.coordinates)

incident <- jsonlite::fromJSON("https://walkrollmap.org/api/incident",
                              flatten = T,
                              simplifyDataFrame = T) %>%
  as.data.frame() %>%
  select(
    id = features.properties.id,
    date = features.properties.date,
    type = features.properties.type,
    feature_type = features.properties.incident_type,
    feature_subtype = features.properties.incident_with,
    description = features.properties.description,
    geometry = features.geometry.coordinates)

wrm <- rbind(hazard,
             amenity,
             incident) %>%
  arrange(id) %>%
  mutate(date = as.POSIXct(date/1000, origin="1970-01-01 00:00:00")) %>%
  rowwise() %>%
  mutate(
    x = geometry[1],
    y = geometry[2],
    feature_subtype = ifelse(feature_subtype %in% "vehicle turning head-on",
                             "vehicle head-on",
                             feature_subtype)) %>%
  select(-geometry)

# colours and order to match the map
colour_palette <- c("#FFC945",  # hazard: yellow
                    "#4281CD",  # missing amenity: blue
                    "#F8485E")  # incident: red

colour_palette_tf <- c("#252525", # true: black
                       "#d9d9d9") # false: grey

wrm_levels <- c("hazard-concern",
                "amenity",
                "incident")

# epsg code for wgs 84
geographic_projection <- 4326
pseudo_mercator <- 3857

# for setting and monitoring the state of the map
map_mode <- "Map extent"

# spatial data (city extents in geographic projection)
geographic_presets <- st_read("data/geographic_presets.gpkg")

# test
# geographic_presets <- st_read("visualize_wrm_app/data/geographic_presets.gpkg")

extents <- c("All reports",
             "Map extent",
             "Canada", # put the national extent at the top of the list
             sort(geographic_presets$name[-which(geographic_presets$name %in% "Canada")]))


# for dates: how many days ago were reports?
days_since <- function(date_in_past){
  difference <- difftime(Sys.time(),
                         as.POSIXct(date_in_past),
                         units = "days")
  
  difference %>% round(0) %>%
    as.integer() %>%
    as.character() %>%
    str_pad(width = 3,
            pad = 0)
}

wrm$doy <- format(wrm$date, "%j") %>%
  factor(levels = as.character(0:365 %>% str_pad(width = 3,
                                                 pad = 0)))

wrm$days_since_report <- days_since(wrm$date)

wrm_spatial <- wrm %>%
  st_as_sf(coords = c("x", "y"),
           crs = pseudo_mercator) %>%
  st_transform(geographic_projection)

wrm_spatial$lon <- unlist(map(wrm_spatial$geometry, 1))
wrm_spatial$lat <- unlist(map(wrm_spatial$geometry, 2))

# descriptions for leaflet map
wrm_spatial$descriptions <- paste0("<b>",stringr::str_to_title(wrm_spatial$type),": </b>", 
                                   str_to_sentence(wrm_spatial$feature_type), "<br>",
                                   "<b>Date: </b>", format(wrm_spatial$date, "%B %d, %Y"), "<br>",
                                   "<b>Id: </b>", wrm_spatial$id, "<br>",
                                   "<b>Description: </b>", wrm_spatial$description)

# create factors for consistent colours
wrm_spatial$type <- factor(wrm_spatial$type,
                           levels = wrm_levels)

## custom labels for time bar graph
how_many_days_ago_was_the_first_day_of_the_month <- function(month_num){
  # if the month is earlier (or the same) than this month, use this year
  # if the month is later than this month, use last year
  earlier <- format(Sys.Date(), "%m") >= month_num
  first_day_of_month <- strptime(paste0("01/",
                                        month_num, "/",
                                        ifelse(earlier,
                                               as.numeric(format(Sys.Date(), "%Y")),
                                               as.numeric(format(Sys.Date(), "%Y"))-1)), 
                                 format = "%d/%m/%Y")
  
  days_since_first_day_of_month <- days_since(first_day_of_month) %>%
    factor(levels = as.character(0:365 %>% str_pad(width = 3,
                                                   pad = 0)))
  return(days_since_first_day_of_month)
}

breaks <-  how_many_days_ago_was_the_first_day_of_the_month(
  c("01", "02", "03", "04", "05", "06", 
    "07", "08" ,"09", "10", "11", "12"))

labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# custom function for wordcloud plotting. 
# Solves an issue where the wordcloud causes the barplot to not render
# source: https://github.com/rstudio/shinydashboard/issues/281

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}