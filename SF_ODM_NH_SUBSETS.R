### 3.23.2020
#Making regional San_Fran maps for ODM:
#Subsetting: line - 933 01-core_concepts, bounding boxes. 

library(ggplot2)
library(dplyr)
library(sf)
library(units)
library(tmap)
library(raster)
library(nngeo)


setwd("/Users/Avery/Desktop/SF_ODM/data")
#loading root dataset
sanfran_calls <- read.csv("311_calls.mid_march2020.csv", stringsAsFactors = FALSE)

###odm coordinate subsetting ###
waste_calls <- sanfran_calls[grep("Waste", sanfran_calls$service_subtype), ]

#filtering out medical waste calls. 
not_medical_waste <- waste_calls[waste_calls$service_subtype != "Medical Waste", ]

#date range of , just plug in the dates for a range. IF YOU CHANGE THIS DATE BELOW, CHANGE THE OTHER DATE RANGE CALL AS WELL..
recent_waste_calls <- subset(not_medical_waste, 
                             as.data.frame.Date(requested_datetime) >= "2020-02-17" & 
                               as.data.frame.Date(requested_datetime)< "2020-03-17")


#futureproofing for out of bounds coordinate bugs.
sanfran_range_recent_calls <- subset(recent_waste_calls[recent_waste_calls$lat > 37, ])

###to filter out NA values in lat/lon columns ### only works if all the way clean..
sanfran_calls_no_na = filter(sanfran_range_recent_calls, lat != "NA")

#filter out "duplicate calls" Note: many duplicates documented through SFC 311 (inverted syntax, weird.)
sanfran_clean_no_na_1 <- sanfran_calls_no_na[grep("Duplicate", sanfran_calls_no_na$status_notes, invert = TRUE), ]

sanfran_clean_no_na_2 <- sanfran_clean_no_na_1[grep("Duplicate", sanfran_clean_no_na_1$agency_responsible, invert = TRUE), ]

#calls with "nothing" in response narrative. Reflecting false alarm or duplicate frequency counts.
sanfran_clean_calls <- sanfran_clean_no_na_2[grep("nothing", sanfran_clean_no_na_2$status_notes, invert = TRUE), ]

#to clear out duplicates based on address, (one incidence, multiple calls situation).
sanfran_map_these = sanfran_clean_calls %>% distinct(address, .keep_all = TRUE)

#setting ODM calls as Simple Features object.
#311 has their lat long inverted, brruh..
odm_coords = st_as_sf(sanfran_map_these, coords = c('long',
                                                    'lat'), crs = 4326)

#pulling in shape file San Fran neighborhoods. 
setwd("/Users/Avery/Desktop/SF_ODM")
dir("data", pattern = "sf_nhoods.")
nhoods = st_read(dsn = './data', layer = 'sf_nhoods')

#aligning CRS. ESPG for projected. not 4326
sf_nhoods = st_transform(nhoods, crs = 4326)

st_crs(odm_coords) == st_crs(sf_nhoods)

#pulls out tenderloin geom??
odm_tendo = sf_nhoods[sf_nhoods$name == 'Tenderloin', ]

#pulls Tenderloin incidence
tendo_coords <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == "Tenderloin", ]

#map for Tenderloin n_hood with odm coords. 
tmap_mode('view')
  tm_shape(odm_tendo) +
  tm_polygons(border.col = 'dark blue', alpha = 0) +
  tm_shape(tendo_coords) +
  tm_dots(size = 0.01, col = 'orange')