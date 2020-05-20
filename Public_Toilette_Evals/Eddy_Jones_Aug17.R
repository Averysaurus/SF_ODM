### 4.24.20 ###
# odm toilette intervention chunk.
# Eddy & Jones, August 1st, 2017 
# type listed as JC Decaux

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
                             as.data.frame.Date(requested_datetime) >= "2016-01-01" & 
                               as.data.frame.Date(requested_datetime)< "2020-01-01")


#futureproofing for out of bounds coordinate bugs.
sanfran_range_recent_calls <- subset(recent_waste_calls[recent_waste_calls$lat > 37, ])

###to filter out NA values in lat/lon columns ### only works if all the way clean..
sanfran_calls_no_na = filter(sanfran_range_recent_calls, lat != "NA")

# Dictionary of narrative report values to filter out "duplicate calls" Note: many duplicates documented through SFC 311. 
sanfran_calls_no_na_1 <- sanfran_calls_no_na[grep("Duplicate", 
                                                  sanfran_calls_no_na$status_notes, invert = TRUE), ]
sanfran_calls_no_na_2 <- sanfran_calls_no_na_1[grep("nothing", 
                                                    sanfran_calls_no_na_1$status_notes, invert = TRUE), ]
sanfran_calls_no_na_3 <- sanfran_calls_no_na_2[grep("Nothing", 
                                                    sanfran_calls_no_na_2$status_notes, invert = TRUE), ]
sanfran_calls_no_na_4 <- sanfran_calls_no_na_3[grep("Case Transferred", 
                                                    sanfran_calls_no_na_3$status_notes, invert = TRUE), ]
sanfran_calls_no_na_5 <- sanfran_calls_no_na_4[grep("Insufficient Information",                              sanfran_calls_no_na_4$status_notes, invert = TRUE), ]
sanfran_calls_no_na_6 <- sanfran_calls_no_na_5[grep("gone", 
                                                    sanfran_calls_no_na_5$status_notes, invert = TRUE), ]
sanfran_calls_no_na_7 <- sanfran_calls_no_na_6[grep("no work needed ", 
                                                    sanfran_calls_no_na_6$status_notes, invert = TRUE), ]
sanfran_calls_no_na_8 <- sanfran_calls_no_na_7[grep("Unable to locate", 
                                                    sanfran_calls_no_na_7$status_notes, invert = TRUE), ]
sanfran_calls_no_na_9 <- sanfran_calls_no_na_8[grep("animal control", 
                                                    sanfran_calls_no_na_8$status_notes, invert = TRUE), ]
sanfran_calls_no_na_10 <- sanfran_calls_no_na_9[grep("not thing", 
                                                     sanfran_calls_no_na_9$status_notes, invert = TRUE), ]
sanfran_calls_no_na_11 <- sanfran_calls_no_na_10[grep("does not match", 
                                                      sanfran_calls_no_na_10$status_notes, invert = TRUE), ]
sanfran_calls_no_na_12 <- sanfran_calls_no_na_11[grep("not see any", 
                                                      sanfran_calls_no_na_11$status_notes, invert = TRUE), ]
sanfran_calls_no_na_13 <- sanfran_calls_no_na_12[grep("Unable to Locate",
                                                      sanfran_calls_no_na_12$status_notes, invert = TRUE), ]
sanfran_calls_no_na_14 <- sanfran_calls_no_na_13[grep("Case is Invalid", sanfran_calls_no_na_13$status_notes, invert = TRUE), ]
sanfran_calls_no_na_15 <- sanfran_calls_no_na_14[grep("noting", sanfran_calls_no_na_14$status_notes, invert = TRUE), ]
sanfran_calls_no_na_16 <- sanfran_calls_no_na_15[grep("see anything", sanfran_calls_no_na_15$status_notes, invert = TRUE), ]
sanfran_calls_no_na_17 <- sanfran_calls_no_na_16[grep("dont see any", sanfran_calls_no_na_16$status_notes, invert = TRUE), ]
sanfran_calls_no_na_18 <- sanfran_calls_no_na_17[grep("Not thing", sanfran_calls_no_na_17$status_notes, invert = TRUE), ]
sanfran_calls_no_na_19 <- sanfran_calls_no_na_18[grep("not at", sanfran_calls_no_na_18$status_notes, invert = TRUE), ]
sanfran_calls_no_na_20 <- sanfran_calls_no_na_19[grep("no poop", sanfran_calls_no_na_19$status_notes, invert = TRUE), ]
sanfran_calls_no_na_21 <- sanfran_calls_no_na_20[grep("see this", sanfran_calls_no_na_20$status_notes, invert = TRUE), ]
sanfran_calls_no_na_22 <- sanfran_calls_no_na_21[grep("wasnt there", sanfran_calls_no_na_21$status_notes, invert = TRUE), ]
sanfran_calls_no_na_23 <- sanfran_calls_no_na_22[grep("looked both", sanfran_calls_no_na_22$status_notes, invert = TRUE), ]

#from agency responsible column
sanfran_calls_no_na_24 <- sanfran_calls_no_na_23[grep("Duplicate", sanfran_calls_no_na_23$agency_responsible, invert = TRUE), ]
sanfran_calls_no_na_25 <- sanfran_calls_no_na_24[grep("Animal Care", sanfran_calls_no_na_24$agency_responsible, invert = TRUE), ]

# appended status notes: regional narrative patterns.
sanfran_calls_no_na_26 <- sanfran_calls_no_na_25[grep("no feces", sanfran_calls_no_na_25$status_notes, invert = TRUE), ]
sanfran_calls_no_na_27 <- sanfran_calls_no_na_26[grep("No feces", sanfran_calls_no_na_26$status_notes, invert = TRUE), ]
sanfran_calls_no_na_28 <- sanfran_calls_no_na_27[grep("insufficient information", sanfran_calls_no_na_27$status_notes, invert = TRUE), ]
sanfran_calls_no_na_29 <- sanfran_calls_no_na_28[grep("does not exist", sanfran_calls_no_na_28$status_notes, invert = TRUE), ]
sanfran_calls_no_na_30 <- sanfran_calls_no_na_29[grep("didnt see any", sanfran_calls_no_na_29$status_notes, invert = TRUE), ]
sanfran_calls_no_na_31 <- sanfran_calls_no_na_30[grep("nothng", sanfran_calls_no_na_30$status_notes, invert = TRUE), ]
sanfran_calls_no_na_32 <- sanfran_calls_no_na_31[grep("WASTE NOT FOUND", sanfran_calls_no_na_31$status_notes, invert = TRUE), ]
sanfran_calls_no_na_33 <- sanfran_calls_no_na_32[grep("not sure where", sanfran_calls_no_na_32$status_notes, invert = TRUE), ]
sanfran_calls_no_na_34 <- sanfran_calls_no_na_33[grep("there is not", sanfran_calls_no_na_33$status_notes, invert = TRUE), ]
sanfran_calls_no_na_35 <- sanfran_calls_no_na_34[grep("did not find", sanfran_calls_no_na_34$status_notes, invert = TRUE), ]
sanfran_calls_no_na_36 <- sanfran_calls_no_na_35[grep("DUPLICATE", sanfran_calls_no_na_35$status_notes, invert = TRUE), ]
sanfran_calls_no_na_37 <- sanfran_calls_no_na_36[grep("already removed", sanfran_calls_no_na_36$status_notes, invert = TRUE), ]
sanfran_calls_no_na_38 <- sanfran_calls_no_na_37[grep("No encampments", sanfran_calls_no_na_37$status_notes, invert = TRUE), ]
sanfran_calls_no_na_39 <- sanfran_calls_no_na_38[grep("nohing here", sanfran_calls_no_na_38$status_notes, invert = TRUE), ]
sanfran_calls_no_na_40 <- sanfran_calls_no_na_39[grep("Cancelled", sanfran_calls_no_na_39$status_notes, invert = TRUE), ]
sanfran_calls_no_na_41 <- sanfran_calls_no_na_40[grep("dup", sanfran_calls_no_na_40$status_notes, invert = TRUE), ]
sanfran_calls_no_na_42 <- sanfran_calls_no_na_41[grep("duplicate", sanfran_calls_no_na_41$status_notes, invert = TRUE), ]
sanfran_calls_no_na_43 <- sanfran_calls_no_na_42[grep("incomplete address", sanfran_calls_no_na_42$status_notes, invert = TRUE), ]
sanfran_calls_no_na_44 <- sanfran_calls_no_na_43[grep("no human waste", sanfran_calls_no_na_43$status_notes, invert = TRUE), ]
sanfran_calls_no_na_45 <- sanfran_calls_no_na_44[grep("no bird found", sanfran_calls_no_na_44$status_notes, invert = TRUE), ]
# list of dictionary terms. 


#to clear out duplicates based on address, (one incidence, multiple calls situation). CONTROL ON LAT/LONG? 
sanfran_mostly_clean = sanfran_calls_no_na_45 %>% 
  distinct(address, .keep_all = TRUE)

#pulling in shape file San Fran neighborhoods. 
setwd("/Users/Avery/Desktop/SF_ODM")
dir("data", pattern = "sf_nhoods.")
nhoods = st_read(dsn = './data', layer = 'sf_nhoods')

# aligning CRS. ESPG for projected. not 4326
sf_nhoods = st_transform(nhoods, crs = 4326)


#### EVALUATION SECTION ##### Before after counts.
# matching date ranges before and after. 

# Setting time frame before/after August 1st, 2017: 
sanfran_map_these_before <- subset(sanfran_mostly_clean, 
                                   as.data.frame.Date(requested_datetime) 
                                   >= "2016-07-01"& 
                                     as.data.frame.Date(requested_datetime)
                                   < "2016-08-01")

sanfran_map_these_after <- subset(sanfran_mostly_clean, 
                                  as.data.frame.Date(requested_datetime) 
                                  >= "2016-08-01" & 
                                    as.data.frame.Date(requested_datetime)
                                  < "2016-09-01")

# Setting ODM waste calls before and after into simp feat object.
odm_coords_bef = st_as_sf(sanfran_map_these_before, coords = c('long',
                                                               'lat'), crs = 4326)

odm_coords_aft = st_as_sf(sanfran_map_these_after, coords = c('long',
                                                              'lat'), crs = 4326)


## SOMA Pulls out South of market neighborhood geom.
odm_tendo <- sf_nhoods[sf_nhoods$name == 'Tenderloin', ]


#bounding box for toilet before (1 month). 
eddyjones_box_bef <- subset(odm_coords_bef, 
                          point.latitude >= 37.7816243316 &    # lower lat bound
                          point.latitude <= 37.786307499 &    # upper lat bound
                          point.longitude >= -122.4164071523 &  # upper long bound
                          point.longitude <= -122.408782204)   # lower long bound

# 8 cumulative incidence one month before. 
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(eddyjones_box_bef) +
  tm_dots(col = 'red', size = 0.01) 


eddyjones_box_aft <- subset(odm_coords_aft, 
                        point.latitude >= 37.7816243316 &    # lower lat bound
                        point.latitude <= 37.786307499 &    # upper lat bound
                        point.longitude >= -122.4164071523 &  # upper long bound
                        point.longitude <= -122.408782204)   # lower long bound


# 11 cumulative incidence 1 month after.
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(eddyjones_box_aft) +
  tm_dots(col = 'red', size = 0.01) 

## Two week frames ####

# match date range before and after, 2 weeks. 
sanfran_map_these_before_twowk <- subset(sanfran_mostly_clean, 
                                         as.data.frame.Date(requested_datetime) 
                                         >= "2017-07-16"& 
                                           as.data.frame.Date(requested_datetime)
                                         < "2017-08-01")

sanfran_map_these_after_twowk <- subset(sanfran_mostly_clean, 
                                        as.data.frame.Date(requested_datetime) 
                                        >= "2017-08-01" & 
                                          as.data.frame.Date(requested_datetime)
                                        < "2017-08-15")


# Setting ODM waste calls before and after object.
odm_coords_bef_twowk = st_as_sf(sanfran_map_these_before_twowk, 
                                coords = c('long', 'lat'), crs = 4326)

odm_coords_aft_twowk = st_as_sf(sanfran_map_these_after_twowk, 
                                coords = c('long','lat'), crs = 4326)


eddyjones_box_bef_twowk <- subset(odm_coords_bef_twowk, 
                          point.latitude >= 37.7816243316 &    # lower lat bound
                          point.latitude <= 37.786307499 &    # upper lat bound
                          point.longitude >= -122.4164071523 &  # upper long bound
                          point.longitude <= -122.408782204)   # lower long bound


# 4 cumulative incidence (2 weeks)
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(eddyjones_box_bef_twowk) +
  tm_dots(col = 'red', size = 0.01) 


eddyjones_box_aft_twowk <- subset(odm_coords_aft_twowk, 
                          point.latitude >= 37.7816243316 &    # lower lat bound
                          point.latitude <= 37.786307499 &    # upper lat bound
                          point.longitude >= -122.4164071523 &  # upper long bound
                          point.longitude <= -122.408782204)   # lower long bound


# 4 cumulative incidence after, (2 wweks).
tmap_mode('view')
tm_shape(odm_tendo) +
  tm_polygons(border.col = 'blue', alpha = 0) +
  tm_shape(eddyjones_box_aft_twowk) +
  tm_dots(col = 'red', size = 0.01) 


### Visualization: histogram and density functions of incidence 
### plotted over time.
raw_incidence_bef <- st_drop_geometry(eddyjones_box_bef)
raw_incidence_aft <- st_drop_geometry(eddyjones_box_aft)

# merging before and after dataframes.
vis_incidence <- base::merge(raw_incidence_bef, raw_incidence_aft, all = TRUE)

# processing datetimeinit for ggplot
vis_incidence$date_only <- gsub('[[:upper:]][0-9][^z][[:punct:]][0-9][^z][[:punct:]][0-9][^z][[:upper:]]', '', vis_incidence$requested_datetime)

# formatting as date structure for ggplot. 
vis_incidence$date_only <- as.Date(vis_incidence$date_only)

# histogram.
ggplot(vis_incidence, aes(date_only)) + 
  geom_histogram(bins = 30) +
  ggtitle("incidence counts before / after")

# density plot.   
ggplot(vis_incidence, aes(date_only)) + 
  geom_density(fill = "orange", alpha = .5) +
  xlab("dates before & after install") +
ggtitle("density function of incidence over time")


