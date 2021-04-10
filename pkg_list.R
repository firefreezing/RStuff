# load pkgs that used in the data visualization


# Packages for rmarkdown/rshiny -------------------------------------------

p_load(knitr)
p_load(shiny)
p_load(shinydashboard)


# A subset of Hadley's packages -------------------------------------------

p_load(tidyverse)
p_load(tidyr)        # tidy and reshape data
p_load(dplyr)        # data cleaning, processing, manipualtion
p_load(stringr)      # string/text processing
p_load(purrr)        # functional programming tools
p_load(lubridate)    # work on date/time-like data


# Packages for creating plots ---------------------------------------------

p_load(ggplot2)
p_load(ggthemes)
p_load(RColorBrewer)
p_load(lattice)
p_load(gridExtra)
p_load(grid)
p_load(rbokeh)       # an R interface for Bokeh
p_load(UpSetR)       # creating UpSet plot
p_load(plotly)       # interactive plots
p_load(scales)       # scale functions for visualizations

# Packages for importing/exporting data of various formats ----------------

p_load(readr)
p_load(readxl)

# Packages for creating maps ----------------------------------------------

p_load(maps)
p_load(leaflet)
p_load(d3heatmap)
p_load(rgdal)
p_load(rgeos)
p_load(mapproj)
p_load(maptools)


# Packages for creating tables --------------------------------------------

p_load(broom)      # convert statistical analysis objects into tidy data frame
p_load(pander)     # R Pandoc writer, create beautiful tables in rmarkdown
p_load(DT)         # interactive data table
