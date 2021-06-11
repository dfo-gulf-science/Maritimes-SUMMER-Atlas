library(sf)
library(tidyverse)

boundaries <- read_sf(file.path("AC/AC_1M_BoundaryPolygons_shp/AC_1M_BoundaryPolygons.shp"))

boundaries_simple <- boundaries %>%
  filter(
    POL_DIV %in% c(
      "Quebec", "Newfoundland and Labrador" ,
      #"New York", "New Hampshire", "Vermont",
      "Maine",
      "New Brunswick", "Nova Scotia",
      "Prince Edward Island"
    ),
    SELECTION == "sparse" #"dense"
  ) %>%
  st_transform(4326)
