#March 2020 avery.richards@berkeley.edu
###building out SFODM 
library(RSocrata)
library(ggplot2)
library(tmap)
library(sf) #simple features.
library(dplyr)
library(tidyverse) #may not be neccessary.


###San Francisco open data 311 dataset### Very large dataset, allow time to load. Longer than get a cup of coffee time, the object has 4x10^6-ish rows. Chill though, it gets there.
sanfran_calls <- read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json", 
                              stringsAsFactors = FALSE)
#download as csv 
#write_csv(sanfran_calls, "311_calls.mid_march2020")
#looking at the sub categories to identify fecal contaminant calls
#View(unique(sanfran_calls$service_subtype))

#potential categories from this call:
#"Human or Animal Waste"
#Human/Animal Waste#

#Big call, may crash. Use with caution
#View(unique(sanfran_calls$service_details))

#this call looks for rows with "Waste" keyword in service subtype column. 
waste_calls <- sanfran_calls[grep("Waste", sanfran_calls$service_subtype), ]

#filtering out medical waste calls. 
not_medical_waste <- waste_calls[waste_calls$service_subtype != "Medical Waste", ]

#date range of , just plug in the dates for a range. IF YOU CHANGE THIS DATE BELOW, CHANGE THE OTHER DATE RANGE CALL AS WELL..
recent_waste_calls <- subset(not_medical_waste, 
                             as.data.frame.Date(requested_datetime) >= "2015-03-17" & 
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

###encampments -
homeless_calls <- sanfran_calls[grep("Encampment", sanfran_calls$service_subtype), ]

#date range of , just plug in the dates for a range. IF YOU CHANGE THIS DATE BELOW, CHANGE THE OTHER DATE RANGE CALL AS WELL..
recent_homeless_calls <- subset(homeless_calls, 
                             as.data.frame.Date(requested_datetime) >= "2015-03-17" & 
                               as.data.frame.Date(requested_datetime)< "2020-03-17")

View(recent_homeless_calls)

#futureproofing for out of bounds coordinate bugs.
sanfran_range_recent_calls_homeless <- subset(recent_homeless_calls[recent_homeless_calls$lat > 37, ])

###to filter out NA values in lat/lon columns ### only works if all the way clean..
sanfran_calls_no_na_homeless = filter(sanfran_range_recent_calls_homeless, lat != "NA")

#filter out "duplicate calls" Note: many duplicates documented through SFC 311 (inverted syntax, weird.)
sanfran_clean_no_na_1_homeless <- sanfran_calls_no_na_homeless[grep("Duplicate", 
                                  sanfran_calls_no_na_homeless$status_notes, invert = TRUE), ]

sanfran_clean_no_na_2_homeless <- sanfran_clean_no_na_1_homeless[grep("Duplicate",
                                  sanfran_clean_no_na_1_homeless$agency_responsible, invert = TRUE), ]

#calls with "nothing" in response narrative. Reflecting false alarm or duplicate frequency counts.
sanfran_clean_calls_homeless <- sanfran_clean_no_na_2_homeless[grep("nothing", sanfran_clean_no_na_2_homeless$status_notes, invert = TRUE), ]

sanfran_map_homeless = sanfran_clean_calls_homeless %>% distinct(address, .keep_all = TRUE)

#graphs for cleaned call data over time: encampment and Open Defecation. 
ggplot()+
geom_density(data= sanfran_clean_calls_homeless, 
             aes(x = requested_datetime), fill = 'blue', alpha = .2) +
geom_density(data = sanfran_map_these, 
             aes(requested_datetime), fill = "orange", alpha = .5) +
  xlab("March 2010 - March 2020") +
  ggtitle("Density Plot of 311 calls over time", subtitle = "Homeless encampments = Blue, Open Defecation = Orange.")
  

ggplot()+
  geom_histogram(data= sanfran_clean_calls_homeless, 
               aes(x = requested_datetime), bins = 60, fill = 'blue', alpha = .2) +
  geom_histogram(data = sanfran_map_these, 
               aes(requested_datetime), bins = 60, fill = "orange", alpha = .8) +
  xlab("March 2015 - March 2020") +
  ggtitle("Histogram frequency count of 311 calls over time", subtitle = "Homeless encampments = Blue, Open Defecation = Orange.")


