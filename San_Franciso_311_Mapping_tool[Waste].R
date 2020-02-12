#Decemeber 2019: avery.richards@berkeley.edu
###testing socrata API inputs for tmap ### SAN_FRANCISCO "Human or Animal Waste" complaints.
## may need to intstall a few of these packages, "sf" and "tmap" have dependencies. 
#install.package("name_of_package_in_quotes"), etc.

library(RSocrata)
library(ggplot2)
library(tmap)
library(sf) #simple features.
library(dplyr)
library(tidyverse) #may not be neccessary.


###San Francisco open data 311 dataset### Very large dataset, allow time to load. Longer than get a cup of coffee time, the object has 4x10^6-ish rows. Chill though, it gets there.
sanfran_calls <- read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json", 
                              stringsAsFactors = FALSE)

#looking at the sub categories to identify fecal contaminant calls
View(unique(sanfran_calls$service_subtype))
#potential categories from this call:
#"Human or Animal Waste"
#Human/Animal Waste#
View(unique(sanfran_calls$service_details))

waste_calls <- sanfran_calls[grep("Waste", sanfran_calls$service_subtype), ]

#filtering out medical waste calls. 
not_medical_waste <- waste_calls[waste_calls$service_subtype != "Medical Waste", ]

#date range of Janurary 2020 here, just plug in the dates for a range. 
recent_waste_calls <- subset(not_medical_waste, 
                               as.data.frame.Date(requested_datetime) >= "2019-01-01" & 
                                 as.data.frame.Date(requested_datetime)< "2019-01-02")

#futureproofing for out of bounds coordinate bugs.
sanfran_range_recent_calls <- subset(recent_waste_calls[recent_waste_calls$lat > 37, ])

###to filter out NA values in lat/lon columns ### only works if all the way clean..
sanfran_calls_no_na = filter(sanfran_range_recent_calls, lat != "NA")

#filter out "duplicate calls" Note: many duplicates docuemented through SFC 311 (inverted syntax)
sanfran_clean_no_na_1 <- sanfran_calls_no_na[grep("Duplicate", sanfran_calls_no_na$status_notes, invert = TRUE), ]

sanfran_clean_calls <- sanfran_clean_no_na_1[grep("Duplicate", sanfran_clean_no_na_1$agency_responsible, invert = TRUE), ]

#mapping data frame. NOTE: inverted x and y coords, may have had it backwards at API endpoint, mabye I have it backwards. It works this way, lol.
sanfran_mapped_calls <- st_as_sf(sanfran_clean_calls, coords = c("point.longitude", "point.latitude"), crs = 4326)

#sets the mode of tmap to view on 
tmap_mode("view")
#this line should load for a sec and then plot map. 
qtm(sanfran_mapped_calls)
#okay, there we are.

