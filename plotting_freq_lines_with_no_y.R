### Consultation Vignette : DPCS & D-Lab ### R-studio, ggplot2, geom_line(), geom_smooth()
## 2020-03-30 ## avery.richards@berkeley.edu

# Documented here are the results for solving: 
# - "How to plot a line geom with ggplot() when we only have a frequency count of (x) ?" - 
# Answer: it was done by creating a "table" object from a vector of dates,
# and plotting a 'y=' as the frequency count of unique dates in the table.  

# Note: answering this question is part of a larger project, seems like 
# oversharing is better than under. SEE LINES 65:92 for solution. 

# relevant packages used.
library(ggplot2)
library(dplyr)

# Bringing in the dataset. These are crowdsourced community 
# and public health reports from mobile devices. 
# nrow here is around 4 * 10^6 and ncol = 23.
sanfran_calls <- read.csv("311_calls.mid_march2020.csv", stringsAsFactors = FALSE)

###--- The next few lines are subsetting from the larger dataset above. ---###

# Use of grep function to locate certain "waste" related reports. 
waste_calls <- sanfran_calls[grep("Waste", sanfran_calls$service_subtype), ]

# Filtering out medical waste calls (IV needles and other waste like that). 
not_medical_waste <- waste_calls[waste_calls$service_subtype != "Medical Waste", ]

# Specific date ranges: this dataset goes back to 2008. 
# The ask in this situation was for 2016 - 2020.
recent_waste_calls <- subset(not_medical_waste, 
                             as.data.frame.Date(requested_datetime) >= "2016-01-01" & 
                               as.data.frame.Date(requested_datetime)< "2020-01-01")



# Note: these lines apply to the R-geospatial component of this project 
# (need clean coordinates within a range or the map breaks.) 
sanfran_range_recent_calls <- subset(recent_waste_calls[recent_waste_calls$lat > 37, ])

# filtering out NA values in lat/lon columns with dplyr. 
sanfran_calls_no_na = filter(sanfran_range_recent_calls, lat != "NA")


### The next few lines are methods to control for confounding in the data. ###
#   Mostly duplicates of where a Public Health Worker
#   shows up and nothing is there to fix. This is some noisy data, folks! 
#   Bear with me...

# Filtering out "duplicate calls" Note: many duplicates documented through 
# the San Francisco 311 dataset.
sanfran_clean_no_na_1 <- sanfran_calls_no_na[grep("Duplicate", 
                                            sanfran_calls_no_na$status_notes, invert = TRUE), ]

# Duplicates are documented on different columns in the dataset, 
# see how "status_notes" and "agency_responsible" are called on seperately? 
# There is systemic overlap (2+ agencies respond to one report).
sanfran_clean_no_na_2 <- sanfran_clean_no_na_1[grep("Duplicate", sanfran_clean_no_na_1$agency_responsible, invert = TRUE), ]

# Calls with "nothing" in response narrative,
# reflecting false alarm or duplicate frequency counts.
sanfran_clean_calls <- sanfran_clean_no_na_2[grep("nothing", sanfran_clean_no_na_2$status_notes, invert = TRUE), ]


# Okay, now these lines use a gsub() function to make a new vector 
# in the dataframe with a cleaned up and ggplot2 recognizable 
# "date" format: YYYY-MM-DD without timestamp. 
# That colossal argument in the parantheses is R-regex code.
sanfran_clean_calls$date_only <- gsub('[[:upper:]][0-9][^z][[:punct:]][0-9][^z][[:punct:]][0-9][^z][[:upper:]]', '', 
                                      sanfran_clean_calls$requested_datetime)

# And finally, a dplyr function to clear out duplicates
# based on address, (those one incidence, multiple reports situations 
# where 2+ people see something and report it at the same time).
sanfran_map_these = sanfran_clean_calls %>% distinct(address, .keep_all = TRUE)

# Notice the "date_only" vector added on line 69 is just that: a single column 
# of dates reflecting each incidence where a user reported human or animal waste
# to San Francisco's 311 system between 2016 - 2020 (nrow is about 3.03 * 10^4).

# Building a table out of our new "date_only" vector. 
y_dates <- as.data.frame(table(sanfran_map_these$date_only))


# Frequency plot with geom_line(). Works with table "Freq" parameters!
ggplot(y_dates, aes(x= Var1, y = Freq, group = 1))+
  geom_line(color = 'steelblue', size = .5, alpha = .75)+
  scale_x_discrete("January 2016 - January 2020", 
                   breaks = c('2017-01-01', '2018-01-01', "2019-01-01", "2020-01-01"), 
                   labels = c('2017', '2018', "2019", "2020")) +
  geom_smooth(method = 'lm', color = 'red', size = .5, alpha = .75) +
  ggtitle("311: Human or Animal Waste calls, San Francisco.") +
  ylab("Frequency of Calls")

# ~FIN
