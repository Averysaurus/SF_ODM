### Sourcing Geospatial Data from 311 ###
# D-Lab Data Fellows Talk: April 2020 ##
## avery.richards@berkeley.edu 

### Mapping Tool for Public Health Intervention with R-studio ###

# The following is a protoype rapid-response tool to support
###     Street Medicine / Street Psychiatry    ###
# for residents of homeless encampments in Oakland, Ca. 

# This tool uses of R-studio geospatial packages to plot
# crowd-sourced reports of Homeless Encampments in Oakland, Ca.

# relevant libaries for this walkthrough
library(RSocrata)
library(ggplot2)
library(dplyr)
library(tmap)
library(sf)


library(RSocrata)
# Fairly simple pulling in data from a socrata API, just one function here:
oak_calls <- read.socrata("https://data.oaklandnet.com/resource/quth-gb8e.json", 
                          stringsAsFactors = FALSE)

# A good next step would be to save as a csv on your local drive.
setwd("_YOUR FILEPATH HERE_")
write.csv(oak_calls, "311_calls.oakland.csv")


# Let's look at the number of rows we have here: 
nrow(oak_calls)
# Wow, that's a lot of rows! 

# Have a look at the structure, yeah?
str(oak_calls)

# How far back does this data go? 
range(oak_calls$datetimeinit)
# Dang that's far.  

# Seems like there's a lot of stuff here, too. Let's have a look at the unique categories of these rows.
View(unique(oak_calls$reqcategory))
# These categories can look pretty cryptic on the surface. 

# Descriptions are more specific: there are hundreds of "description" variables here.
View(unique(oak_calls$description))

# We can subset this big dataframe via a date ranges: let's start with a "one day" slice.
oak_calls_day <- subset(oak_calls, 
                        as.data.frame.Date(datetimeinit) >= "2020-04-03" & 
                          as.data.frame.Date(datetimeinit)< "2020-04-04")


dim(oak_calls_day)
# Seems more human freindly now, yeah?


## Okay, so say we are a medical non-profit that has a mobile clinic to help
## homeless communities access services. 

# If we scroll around here for a minute, we notice a "Homeless Encampment" descriptor. 
View(unique(oak_calls_day$description))

# Let's subset for rows with "Homeless Encampment" in the description:
home_camps_today <- oak_calls_day[oak_calls_day$description == "Homeless Encampment",]

# This will make more sense later, but here is function that can remove duplicate coordinate points. 
# Consider that in the long-run we have to adjust for 2+ reports on 1 incident. 
# Scrutiny is the watchword with open-data like this,  
# especially if we are to run counts of incidence or quantitative statistcal stuff 
uni_home_camps = home_camps_today %>% distinct(reqaddress.latitude, .keep_all = TRUE)


View(uni_home_camps)
## Notice how one of the rows has "CANCEL" under the status column? 

## This is a report someone made to the City that could not be verified.
## A pretty common occurance, you'll see.
## Discovering what systematic language is
## used to express false alarms, cancelled calls, and duplicates 
## in 311 data sources is helpful because we can incorporate that languge 
## into our analysis to control for confounding.

## grep functions are good at this.

# This grep function removes rows with the CANCEL string in the "status" column:
uni_home_camps <- uni_home_camps[grep("CANCEL", uni_home_camps$status, invert = TRUE), ]

View(uni_home_camps)
# Notice how our new list did not include the row with "CANCEL"?

# Also notice how there are what looks like lat/long coordinates in this data frame?
# Lets map those points!

library(sf)
# Simple features is part of a geospatial package native to R. It's very cool! 
# D-lab has an outstanding workshop series on how to use it. IOKN2K! :) 

# Here I'm adding "mappable" functionality to the dataframe. 
map_home_camps <- st_as_sf(uni_home_camps, coords = 
                             c("reqaddress.longitude", "reqaddress.latitude" ), 
                           crs = 4326)

# A package for making awesome looking maps. 
library(tmap)

# A quick tmap.
qtm(map_home_camps) +
  tm_dots()
# toggling the map mode.
tmap_mode("view")

### So that's a start, we have some points to work with. 
### We could dispatch a case worker to outreach there if we had the resouces to. ###

# Let's zoom out on the time scale a little:
oak_calls_week <- subset(oak_calls, 
                         as.data.frame.Date(datetimeinit) >= "2020-03-15" & 
                           as.data.frame.Date(datetimeinit)< "2020-03-22")

# Subsetting..
home_camps_week <- oak_calls_week[oak_calls_week$description == "Homeless Encampment",]

# Again let's have a look.
View(home_camps_week)

# Cancels seem like a thing in here. Let's clean those up:
nocanc_home_camps_week <- home_camps_week[grep("CANCEL", home_camps_week$status, invert = TRUE), ]

# Making a simple features object with Coordinate Reference System = 4326
map_home_camps_week <- st_as_sf(nocanc_home_camps_week, coords = 
                                  c("reqaddress.longitude", "reqaddress.latitude" ), 
                                crs = 4326)


#set the map to view mode.
tmap_mode("view")

# Another "quick tmap".
qtm(map_home_camps_week) +
  tm_dots()

# A little dimensionality is showing in our geospatial data now. 
# We might want to take these points into account when looking to deploy 
# our mobile clinic in a strategic way for our monthly outreach. 

# Let's look at the month of March. 
oak_calls_month <- subset(oak_calls, 
                          as.data.frame.Date(datetimeinit) >= "2020-03-01" & 
                            as.data.frame.Date(datetimeinit)< "2020-04-01")

# Subsetting, and grepping out the CANCELs.
home_camps_month <- oak_calls_month[oak_calls_month$description == "Homeless Encampment",]
nocanc_home_camps_month <- home_camps_month[grep("CANCEL", home_camps_month$status, invert = TRUE), ]

# This time we are going to filter out those "extras": 2+ reports of a single incidence. 
uni_home_camps_month = nocanc_home_camps_month %>% distinct(reqaddress.latitude, .keep_all = TRUE)


dim(nocanc_home_camps_month)
dim(uni_home_camps_month)
# Looking at the dimensions, there is a small difference, but you'll find this 
# scales up a lot when we look at bigger datasets over time.

# Setting up for mapping.
map_home_camps_month <- st_as_sf(uni_home_camps_month, coords = 
                                   c("reqaddress.longitude", "reqaddress.latitude" ), 
                                 crs = 4326)

#Wait, what? Let's View this data frame.
View(uni_home_camps_month)

# There's an NA value somewhere in the lat/long columns, 
# not as subtle as the duplicate coordinates but still a problem.
# Here we are filtering out NA values:
clean_home_camps_month <- filter(uni_home_camps_month, reqaddress.latitude != "NA" |
                                   reqaddress.longitude != "NA")

# Try again!
map_home_camps_month <- st_as_sf(clean_home_camps_month, coords = 
                                   c("reqaddress.longitude", "reqaddress.latitude" ), 
                                 crs = 4326)
# No errors now.

tmap_mode("view")

# quick tmap.
qtm(map_home_camps_month) +
  tm_dots()
# Wow that's a lot of dots! 

# Maybe we want to step back a do a quick visualization of these.
# Let's make a quick histogram:

ggplot(map_home_camps_month, aes(datetimeinit)) +
  geom_histogram(bins = 30)
# ggplot is not feeling the "organic" data formats. 


# This time gsub and R-regex will help us fix this: 
map_home_camps_month$dates <- gsub('[[:blank:]]\\d.[[:punct:]]\\d.[[:punct:]]\\d.', '', 
                                   map_home_camps_month$datetimeinit)

### How I learned to navigate R-regex: ###
# https://spannbaueradam.shinyapps.io/r_regex_tester/ # 

#We made a new vector on line 204, here we restructure that with "as.Date"
map_home_camps_month$dates <- as.Date(map_home_camps_month$dates)

# Now ggplot should be into it:
ggplot(map_home_camps_month, aes(dates, fill = status)) +
  geom_histogram(bins = 30)


# Many Cities in the Bay Area have open-data portals with crowdsourced geospatial coordinates. 
# As in life, there is heterogenity in how these systems operate:
# Data is organized uniquely, systems overlap, levels of transperancy are different. 
# One thing to note is the wide variance of data quality.
# We are looking through the eyes of the crowd, and crowds are noisy places to be!
# I feel like open date portals are awesome resources, 
# but it's important we question what we see there with scrutiny.

# Thanks for your time and attention! 

# FIN
