### Geospatial methods: example from Data Scienece Fellow presentation
### Avery Richards, UC Berkeley School of Public Health, D-lab 
### avery.richards@berkeley.edu


# Required packages,
# run install.package('') as needed to load. 
library(RSocrata)
library(ggplot2)
library(dplyr)
library(tmap)
library(sf)

# Here is the "Rsocrata" function that brings in the 311 data:
# Could take a while to load the API, so chill when running these lines.
oak_calls <- 
read.socrata("https://data.oaklandnet.com/resource/quth-gb8e.json", 
                         stringsAsFactors = FALSE)
 
#Exploring data, dimensions and unique paramenters in columns.
dim(oak_calls)
range(oak_calls$datetimeinit)
unique(oak_calls$reqcategory)
unique(oak_calls$description)
 
 #Subetting for a day of calls.
oak_calls_day <- subset(oak_calls, 
                         as.data.frame.Date(datetimeinit)
                         >= "2020-04-03" & 
                           as.data.frame.Date(datetimeinit)
                         < "2020-04-04")

 
home_camps_today <- oak_calls_day[oak_calls_day$description ==
                                     "Homeless Encampment",]
 
#Mapping with sf package.
map_home_camps <- st_as_sf(true_home_camps_today, coords = 
                              c("reqaddress.longitude", 
                                "reqaddress.latitude"), 
                            crs = 4326)
library(tmap)
# toggling the map mode to interactive.
tmap_mode("view")
 
#plotting map.
qtm(map_home_camps) +
   tm_dots()
 
# a random week in March.
oak_calls_week <- subset(oak_calls, 
                          as.data.frame.Date(datetimeinit)
                          >= "2020-03-15" & 
                            as.data.frame.Date(datetimeinit)
                          < "2020-03-23")
 # subsetting.
 home_camps_week <- oak_calls_week[oak_calls_week$description == 
                                     "Homeless Encampment",]
 
 #Filtering CANCEL status.
 true_home_camps_week <- home_camps_week[grep("CANCEL", 
                                              home_camps_week$status, invert = TRUE), ]
 
 #Formatting to map.
 map_home_camps_week <- st_as_sf(true_home_camps_week, coords = 
                                   c("reqaddress.longitude",
                                     "reqaddress.latitude" ), 
                                 crs = 4326)
 
 #plotting map.
 qtm(map_home_camps_week) + tm_dots()
 
 #Monthly scale.
 oak_calls_month <- subset(oak_calls, 
                           as.data.frame.Date(datetimeinit) 
                           >= "2020-03-01" & 
                             as.data.frame.Date(datetimeinit)
                           < "2020-04-02")

# Subsetting, and grepping out those CANCEL.
home_camps_month <- oak_calls_month[oak_calls_month$description 
                                     == "Homeless Encampment",]
true_home_camps_month <- home_camps_month[grep("CANCEL", 
                                                home_camps_month$status, invert = TRUE), ]
 
# Distinct calls, no 2+ reports per incidence.
uni_home_camps_month = true_home_camps_month %>% 
   distinct(reqaddress.latitude,
            .keep_all = TRUE) %>% 
   distinct(reqaddress.longitude,
            .keep_all = TRUE)
 
 #the difference with distinct().
 dim(true_home_camps_month)
 dim(uni_home_camps_month)
 
 # Purge NA values for map to work.
 clean_home_camps_month <- filter(uni_home_camps_month, 
                                  reqaddress.latitude != "NA" |
                                    reqaddress.longitude != "NA")
 
 map_home_camps_month <- st_as_sf(clean_home_camps_month, coords = 
                                    c("reqaddress.longitude", 
                                      "reqaddress.latitude" ), 
                                  crs = 4326)
 
 #Map with status dimension.
 map_home_camps_month$status <- as.factor(map_home_camps_month$status)
 tm_basemap()+ 
   tm_shape(map_home_camps_month) +
   tm_dots(col = 'status', palette = "-plasma", size = .01)
 
 #Reformat for histogram.
 map_home_camps_month$dates <- 
   gsub('[[:blank:]]\\d.[[:punct:]]\\d.[[:punct:]]\\d.', '', 
        map_home_camps_month$datetimeinit)
 

#Plotting Histogram. 
ggplot(map_home_camps_month, aes(dates, fill = status)) + 
  geom_histogram(bins = 30) + ggtitle("Homeless Encampment complaints, without CANCEL.")
 
