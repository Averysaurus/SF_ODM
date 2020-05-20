### 4.8.2020
#Sample test before/after sanitary intervention.

library(rspatial)
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
                             as.data.frame.Date(requested_datetime) >= "2020-02-01" & 
                               as.data.frame.Date(requested_datetime)< "2020-03-01")


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

View(sanfran_map_these)
# Proportion of calls removed with "distinct" function. Could distort if used over time:
# When same place is reported. Trade off: scrutiny vs. overeporting. 
1 - nrow(sanfran_map_these) / nrow(sanfran_clean_calls)



# Setting ODM calls as Simple Features object.
odm_coords = st_as_sf(sanfran_map_these, coords = c('long',
                                                    'lat'), crs = 4326)

#pulling in shape file San Fran neighborhoods. 
setwd("/Users/Avery/Desktop/SF_ODM")
dir("data", pattern = "sf_nhoods.")
nhoods = st_read(dsn = './data', layer = 'sf_nhoods')

#aligning CRS. ESPG for projected. not 4326
sf_nhoods = st_transform(nhoods, crs = 4326)

st_crs(odm_coords) == st_crs(sf_nhoods)


qtm(odm_coords)

tm_shape(sf_nhoods) +
  tm_polygons(col="beige", border.col="blue", alpha=0.2) +
  tm_shape(odm_coords) + 
  tm_dots(col="red",size=.01)



tmap_mode('plot')

st_crs(odm_coords)
st_bbox(odm_tendo)

# Pulls out South of market geom.
odm_soma <- sf_nhoods[sf_nhoods$name == 'South of Market', ]
# incideence
odm_in_soma <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == 'South of Market', ]

# Maps them
tmap_mode('view')
tm_shape(odm_soma) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(odm_in_soma) +
  tm_dots(col = 'red', size = 0.01) 

nrow(odm_in_soma)
View(odm_in_soma)

View(sanfran_map_these)

# Pulls out South of market geom.
odm_mission <- sf_nhoods[sf_nhoods$name == 'Mission', ]
# incideence
odm_in_mission <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == 'Mission', ]

# Maps them
tmap_mode('view')
tm_shape(odm_mission) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(odm_in_mission) +
  tm_dots(col = 'red', size = 0.01) 

nrow(odm_in_mission)
View(odm_in_mission)



#pulls out tenderloin geom.
odm_tendo = sf_nhoods[sf_nhoods$name == 'Tenderloin', ]
#pulls incidence. 
odm_in_tendo <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == 'Tenderloin', ]

# quick map of TL with 1 month of incidence. 
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(odm_in_tendo) +
  tm_dots(col = 'red', size = 0.01)

nrow(odm_in_tendo)
View(odm_in_tendo)

### 3.23.2020
#Making regional San_Fran maps for ODM:


library(rspatial)
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
                             as.data.frame.Date(requested_datetime) >= "2020-02-01" & 
                               as.data.frame.Date(requested_datetime)< "2020-03-01")


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

View(sanfran_map_these)
# Proportion of calls removed with "distinct" function. Could distort if used over time:
# When same place is reported. Trade off: scrutiny vs. overeporting. 
1 - nrow(sanfran_map_these) / nrow(sanfran_clean_calls)



# Setting ODM calls as Simple Features object.
odm_coords = st_as_sf(sanfran_map_these, coords = c('long',
                                                    'lat'), crs = 4326)

#pulling in shape file San Fran neighborhoods. 
setwd("/Users/Avery/Desktop/SF_ODM")
dir("data", pattern = "sf_nhoods.")
nhoods = st_read(dsn = './data', layer = 'sf_nhoods')

#aligning CRS. ESPG for projected. not 4326
sf_nhoods = st_transform(nhoods, crs = 4326)

st_crs(odm_coords) == st_crs(sf_nhoods)


qtm(odm_coords)

tm_shape(sf_nhoods) +
  tm_polygons(col="beige", border.col="blue", alpha=0.2) +
  tm_shape(odm_coords) + 
  tm_dots(col="red",size=.01)



tmap_mode('plot')

st_crs(odm_coords)
st_bbox(odm_tendo)

# Pulls out South of market geom.
odm_soma <- sf_nhoods[sf_nhoods$name == 'South of Market', ]
# incideence
odm_in_soma <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == 'South of Market', ]



# Maps them
tmap_mode('view')
tm_shape(odm_soma) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(odm_in_soma) +
  tm_dots(col = 'red', size = 0.01) 

nrow(odm_in_soma)
View(odm_in_soma)

View(sanfran_map_these)

# Pulls out South of market geom.
odm_mission <- sf_nhoods[sf_nhoods$name == 'Mission', ]
# incideence
odm_in_mission <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == 'Mission', ]

# Maps them
tmap_mode('view')
tm_shape(odm_mission) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(odm_in_mission) +
  tm_dots(col = 'red', size = 0.01) 

nrow(odm_in_mission)
View(odm_in_mission)

### Sampling before and after intervention test ###

#pulls out tenderloin geom.
odm_tendo = sf_nhoods[sf_nhoods$name == 'Tenderloin', ]

View(odm_in_tendo)

#logic works, should tweak coordinates.
tendo_sample_box <- subset(odm_in_tendo, point.latitude >= 37.78122 & point.latitude <= 37.78593 &
                             point.longitude >= -122.4196 &  point.longitude <= -122.4121)

#  box coordinates 
# -122.419693,37.781222,-122.412172,37.785935   
# https://boundingbox.klokantech.com/


# maps box for cross validation.  
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(tendo_sample_box) +
  tm_dots(col = 'red', size = 0.01)

                    
                           
#pulls incidence. 
odm_in_tendo <- odm_coords[odm_coords$neighborhoods_sffind_boundaries == 'Tenderloin', ]




# quick map of TL with 1 month of incidence. 
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(odm_in_tendo) +
  tm_dots(col = 'red', size = 0.01)

nrow(odm_in_tendo)
View(odm_in_tendo)



#box coordinates 
-122.419693,37.781222,-122.412172,37.785935

