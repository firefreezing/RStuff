library(dplyr)



# Wraps strings for ggplot ------------------------------------------------

wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}


# Define functions and constants ------------------------------------------

state_info <- c(state.name, "District of Columbia") %>%
  tolower() %>%
  cbind(c(state.abb, "DC")) %>%
  `colnames<-`(c("long", "abbr"))


# Generate hexagon US map -------------------------------------------------

# state_hexmap reads in a statefile with 2 columns: state and value,
# where state is in the two-letter abbreviation format
# it returns a hexmap ggplot that colors each state hex by value
state_hexmap <- function(statefile, varname = "Performance \nScore", this_title = "Title"){
  
  # state.abb2 is a vector of all state abbreviations, plus
  # Washington, DC
  state.abb2 <- c(state.abb, "DC")
  colnames(statefile) <- c("iso3166_2", "value")
  
  # Create statefile2, which just has 'NA' for all the states
  # that are not in statefile
  statefile2 <- statefile %>%
    rbind.data.frame(state.abb2[!state.abb2 %in% statefile$iso3166_2] %>% 
                       data.frame() %>%
                       mutate(value = NA) %>%
                       `colnames<-`(c("iso3166_2", "value")))
  
  # Read in data on the position of hexes, etc. for each state
  us <- readOGR("//mathematica.net/ndrive/Project/50171_QM_DualsLTSSIAPs/Restricted/DC1/Task 6 Testing/Duals/Programs_Analysis/DUALS_1/RMarkdown/Exploratory/us_states_hexgrid.geojson", "OGRGeoJSON", verbose = FALSE)
  # and merge it with the values for each state
  us@data <- merge(us@data, statefile2, by = "iso3166_2", all.x = TRUE, sort = FALSE)
  us@data$value <- as.numeric(us@data$value)
  
  centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
  us_map <- tidy(us, region = "iso3166_2")
  
  gg <- ggplot()+ 
    # Lay out all of the hexes
    geom_map(data=us_map, map=us_map, 
             aes(x=long, y=lat, map_id=id), color="white", size=0.5) + 
    # Colors according to value
    geom_map(data=us@data, map=us_map, 
             aes(fill=value, map_id=iso3166_2)) + 
    # Adds a white border between each hex
    geom_map(data=us@data, map=us_map, 
             aes(map_id=iso3166_2), fill="#ffffff", alpha=0, color="white", show.legend=FALSE) + 
    # Adds state names
    geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=3, fontface = "bold") + 
    # Rescale axes so that the hexes are regular
    coord_map() + 
    labs(x=NULL, y=NULL, title = this_title) +
    theme_bw() +
    # Change color scale to red (bad) - green (good)
    scale_fill_distiller(palette = "RdYlGn", direction = -1, name = paste0(varname, "\n ")) +
    # Clean up the graph
    theme(panel.border=element_blank()) + 
    theme(panel.grid=element_blank()) + 
    theme(axis.ticks=element_blank()) + 
    theme(axis.text=element_blank()) 
  gg
  
  return(gg)
}

# Create state_hexmap for measure results --------------------------------
state_hexmap_measure <- function(status = "Acute", case = "1", strat = "Age", strat_value = 0){
  # Read in status, case, strat, and strat value
  status2 <- recode(status, "'Acute' = 'acu'; 'Chronic' = 'chr';'Total' = 'tot'")
  strat2 <- recode(strat, "'Age' = 'age'; 'Sex'='sex'; 'None' = 'age'; 'HCBS'= 'hcbs'; 'None'='no'")
  strat_value2 <- ifelse(strat == "no", "", strat_value)
  
  # Read in corresponding data set
  filename = paste0("vizop.duals1_",status2,case,"_",strat2,strat_value2,"_st.csv")
  measure_data_dir = "//mathematica.net/ndrive/Project/50171_QM_DualsLTSSIAPs/Restricted/SASGRID/DATA/DUALS/RMarkdown/Exploratory/output/"
  
  # Stop if no such file exists
  if (!filename %in% list.files(measure_data_dir)){
    stop("Invalid combination of status, case number, stratifier, and stratifier value.")
  }
  
  # Merge with state info
  dat <- read.csv(paste0(measure_data_dir, filename))
  statefile <- merge(dat, state_xwalk, by.x = "STATE", by.y = "Value") %>%
    select(Abbr, RATE_NEW_COMPOSITE)
  
  # Recode some variables to make plotting better
  strat_value <- ifelse(strat_value2 == "", "All", strat_value) 
  if (strat == "Age"){
    strat_value <- recode(strat_value, "'0' = '18 - 64';1='65+'")
  }
  
  plot <- state_hexmap(statefile, varname = "Measure \n Result", this_title = "")
  plot +
    labs(title = paste0("Measure Result (", status," Case ", case, "):\n Stratified by ", strat, " = ", strat_value))
}


# Create leaflet map ------------------------------------------------------

# city_leaflet reads in a city file with 2 columns: city and value,
# it returns a leaflet plot. 

# cityfile <- dat_city
# 
# city_leaflet <- function(cityfile){
#   
#   # Read in the cities shapefile
#   shp <- readOGR("//mathematica.net/ndrive/Project/50171_QM_DualsLTSSIAPs/Restricted/DC1/Task 6 Testing/measure-testing-template/cities/citiesx020.shp", "citiesx020")
#   
#   # extract the city centroids with name and state
#   
#   geo <- 
#     gCentroid(shp, byid=TRUE) %>%
#     data.frame() %>%
#     dplyr::rename(lon=x, lat=y) %>%
#     mutate(city=shp@data$NAME, state=shp@data$STATE)
#   
#   cities <- cityfile %>%
#     left_join(geo, by = "state")
#   
#   pal <- colorNumeric("Spectral", domain = cities$value)
#   
#   leaflet(cities) %>% 
#     addTiles() %>%
#     addCircleMarkers(lng = ~lon, lat = ~lat, 
#                      color = ~pal(Frequency),
#                      # radius = ~sqrt(Frequency)*3,
#                      popup = ~paste0(City, ", ", State, " (", Frequency, ")")) %>%
#     addLegend("bottomright", pal = pal, values = ~Frequency,
#               title = "Beneficiaries",
#               opacity = 0.8)
# }



# Create state-level US map using ggplot ----------------------------------

# map_usa reads in a state-level file with values 
# and returns a ggplot
map_usa <- function(statefile, varname = "Variable", this_title="Title"){
  
  # state.abb2 is a vector of all state abbreviations, plus
  # Washington, DC
  state.abb2 <- c(state.abb, "DC")

  colnames(statefile) = c("state", "value")
  statefile <- merge(statefile, state_info, by.x = "state", by.y = "abbr") %>%
    select(state = long, value)
  
  # Geographical data about states 
  # NOTE: as of 9/14/2016 map_data("state") no longer works. this is a bug with ggplot2
  states_map <-map_data(map = "state")
  states_names <- select(states_map, state = region)[,1] %>% unique()
  
  statefile2 <- states_names[!states_names %in% statefile$state] %>%
    cbind.data.frame(NA) %>%
    `colnames<-`(c("state", "value")) %>%
    rbind.data.frame(statefile)
  
  statefile2 <- statefile %>%
    rbind.data.frame(state.abb2[!state.abb2 %in% statefile$iso3166_2] %>% 
                       data.frame() %>%
                       mutate(value = NA) %>%
                       `colnames<-`(c("iso3166_2", "value")))
  
  
  gg <- ggplot(statefile2, aes(map_id = state)) +
    geom_map(aes(fill = value), map = states_map) +
    borders("state", colour = "gray95") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map() + 
    labs(x=NULL, y=NULL, title = this_title) +
    theme_bw() +
    scale_fill_distiller(palette = "RdYlGn", direction = 1, name = varname, na.value = "#D3D3D3") +
    theme(panel.border=element_blank()) + 
    theme(panel.grid=element_blank()) + 
    theme(axis.ticks=element_blank()) + 
    theme(axis.text=element_blank()) 
  gg
}


# Generate frequency table of a table by specified subgroup ---------------

# var_list is an input for the subgroup list.
# e.g. freq_table(ggplot2::mpg, c("manufacturer", "model"))

freq_table <- function(data, var_list){
  output <- data %>% group_by_(.dots = var_list) %>%
    dplyr::summarise(freq = n()) %>%
    ungroup() %>%
    arrange(desc(freq)) %>%
    mutate(
      prop = round(freq/sum(freq), 3),
      cum_prop = round(cumsum(freq/sum(freq)), 3)
    )
  return(output)
}