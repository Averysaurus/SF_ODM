getwd()
setwd("/Users/Avery/Desktop/R Study")
library(ggplot2)
library(tmap)
library(sf)

call_center_data <- read.csv("oakland_serv_req.csv", stringsAsFactors = F)

homeless_camp <- call_center_data[call_center_data$DESCRIPTION == "Homeless Encampment",]

View(homeless_camp)

homeless_camp_sf = st_as_sf(homeless_camp, coords = c("LAT", "LON"), crs = 4326)

ggplot() +
geom_sf(data = homeless_camp_sf)

tmap_mode('plot')
tmap_mode('view')
qtm(homeless_camp_sf)

tm_shape(homeless_camp_sf) +
  tm_dots(col = "source", size = .025) 
  