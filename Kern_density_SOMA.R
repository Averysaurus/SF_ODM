# library(rspatial)
library(ggplot2)
library(dplyr)
library(sf)
library(units)
library(tmap)
library(raster)
library(nngeo)
library(maptools)
library(spatstat)


setwd("/Users/Avery/Desktop/SF_ODM/data")
#loading root dataset
sanfran_calls <- read.csv("311_calls.mid_march2020.csv", stringsAsFactors = FALSE)

###odm coordinate subsetting ###
waste_calls <- sanfran_calls[grep("Waste", sanfran_calls$service_subtype), ]

#filtering out medical waste calls. 
not_medical_waste <- waste_calls[waste_calls$service_subtype != "Medical Waste", ]

#date range of , just plug in the dates for a range. IF YOU CHANGE THIS DATE BELOW, CHANGE THE OTHER DATE RANGE CALL AS WELL..
recent_waste_calls <- subset(not_medical_waste, 
                             as.data.frame.Date(requested_datetime) >= "2019-01-01" & 
                               as.data.frame.Date(requested_datetime)< "2020-01-01")


#futureproofing for out of bounds coordinate bugs.
sanfran_range_recent_calls <- subset(recent_waste_calls[recent_waste_calls$lat > 37 
                                                        & recent_waste_calls$lat < 38 
                                                       , ])

###to filter out NA values in lat/lon columns ### only works if all the way clean..
sanfran_calls_no_na = filter(recent_waste_calls, lat != "NA")

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
sanfran_calls_no_na_46 <- sanfran_calls_no_na_45[grep("in progress", sanfran_calls_no_na_45$status_notes, invert = TRUE), ]
# list of dictionary terms. 


#to clear out duplicates based on address, (one incidence, multiple calls situation). CONTROL ON LAT/LONG? 
sanfran_mostly_clean = sanfran_calls_no_na_46 %>% 
  distinct(address, .keep_all = TRUE)

# filter soma spots
sanfran_mostly_clean_2 = sanfran_mostly_clean %>% 
  filter(neighborhoods_sffind_boundaries == "Mission")

sanfran_mostly_clean_3 <- subset(sanfran_mostly_clean_2, 
                                 select = c('long', 'lat'))



###---------- Point Pattern experiment -------------- ####

setwd("/Users/Avery/Desktop/SF_ODM/data")
# imports shp file
nhoods = st_read('sf_nhoods.shp')

# needs projected crs transform.
nhoods_2 <- st_transform(nhoods, 26910)
nhoods_3 <- as(as_Spatial(nhoods_2[nhoods_2$name == 'Mission',]), "owin")

sanfran_points = st_as_sf(sanfran_mostly_clean_3, coords 
                          = c('long','lat'), crs = 26910)

# points as ppp class 
sanfran_ppp <- as(as_Spatial(sanfran_points), "ppp") 


marks(sanfran_ppp) <- NULL
Window(sanfran_ppp) <- nhoods_3

plot(sanfran_ppp, main=NULL, cols=rgb(0,0,0,.2), pch=20)

Q <- quadratcount(sanfran_ppp, nx= 8, ny=8)

plot(sanfran_ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid

Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(sanfran_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# Compute the density for each quadrat (in counts per km2)
Q   <- quadratcount(sanfran_ppp, nx= 8, ny=8)
Q.d <- intensity(Q)

plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(sanfran_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points









