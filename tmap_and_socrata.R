getwd()
setwd("/Users/Avery/Desktop/R Study")
###testing socrata API inputs for tmap ### 12/20/2019
install.packages("RSocrata")
install.packages("sf")
library(RSocrata)
library(ggplot2)
library(tmap)
library(sf)
library(dplyr)

### reading in API endpoint from URL. IS LARGE, MAY TAKE A MOMENT ###
oak_calls <- read.socrata("https://data.oaklandnet.com/resource/quth-gb8e.json", 
                          stringsAsFactors = FALSE)

###subsetting calls to homeless encampment category ###
###NOTE: how to automate date to maintain certain range over time? ###
homeless_calls <- oak_calls[oak_calls$description == "Homeless Encampment",]
recent_calls <- subset(homeless_calls, 
                        as.data.frame.Date(datetimeinit) > "2019-12-25")

#futureproofing for out of bounds coordinate bugs.
range_recent_calls <- subset(recent_calls[recent_calls$reqaddress.latitude > 37, ])


###to filter out NA values in lat/lon columns ###
clean_calls = filter(range_recent_calls, reqaddress.latitude != "NA")

###NOTE: coerce lat/lon as integers??
#selecting and mutuating chr to num for coordinates. DO NOT NEED.
#clean_calls_two <- clean_calls %>%
  #select(datetimeinit,reqaddress.latitude, reqaddress.longitude) %>%
  #mutate_if(is.character, as.numeric)

#mapping data frame. NOTE: inverted x and y coords, may have had it backwards at API endpoint, lol.
mapped_calls <- st_as_sf(clean_calls, coords = c( "reqaddress.longitude", "reqaddress.latitude"), crs = 4326)

#checking CRS
st_crs(mapped_calls)
class(mapped_calls)
View(mapped_calls)

#basic plots
plot(mapped_calls)
ggplot() +
  geom_sf(data = mapped_calls)

#tmapped works.
tmap_mode("plot")
qtm(mapped_calls)
tmap_mode("view")

tm_shape(mapped_calls)


tm_shape(mapped_calls) +
  tm_dots(size = .025) 


View(mapped_calls)
str(mapped_calls)



