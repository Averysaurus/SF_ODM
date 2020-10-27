# Supplemental code files for  RPub: Mapping prevalence of open defecation.
# Avery Richards, UC Berkeley School of Public Health, June 2020
# Questions? email: avery.richards [at] berkeley [dot] edu

### NOTE TO FUTURE SELF: I put this project together as I was learning how 
### to code with R. These are not the cleanest, most graceful or concise lines, 
### but they get the job done. You should be proud of them!  ~Avery


# Load Required Packages.
library(RSocrata)
library(ggplot2)
library(dplyr)
library(sf)
library(units)
library(tmap)
library(raster)
library(nngeo)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(maptools)
library(spatstat)


# Load root dataset. This make take a while, 30-40 minutes even.
sanfran_calls_july <-  
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json", 
               stringsAsFactors = FALSE)

# Subset on waste reports. 
waste_calls <- sanfran_calls[grep("Waste", sanfran_calls$service_subtype), ]

#filter out medical waste calls. 
not_medical_waste <- waste_calls[waste_calls$service_subtype != "Medical Waste", ]

# Set a date range interval. 
recent_waste_calls <- subset(not_medical_waste, 
                        as.data.frame.Date(requested_datetime) >= "2010-01-01" & 
                        as.data.frame.Date(requested_datetime)< "2020-07-01")


# Subet NA values in lat/lon column that may distort mapping.
sanfran_range_recent_calls <- 
  subset(recent_waste_calls[recent_waste_calls$point.latitude > 37, ])

### NOTE FROM PAST SELF: apologies for this long list of repetious grep calls. :o
### Today I am wiser and stronger and should have written a function to handle 
### all these. Next time I will, promise... ~Avery

# Remove missclassified  reports from agency_responsible column. 
agency_1 <- sanfran_range_recent_calls[grep('Animal Care and Control - G',
              sanfran_range_recent_calls$agency_responsible, invert = TRUE), ]
agency_2 <- agency_1[grep('Animal Care and Control - G - Hold',
                          agency_1$agency_responsible, invert = TRUE), ]
agency_3 <- agency_2[grep('AT and T - Graffiti Queue',
                          agency_2$agency_responsible, invert = TRUE), ]
agency_4 <- agency_3[grep('Clear Channel - Transit Queue',
                          agency_3$agency_responsible, invert = TRUE), ]
agency_5 <- agency_4[grep('DBI Building Inspection Queue',
                          agency_4$agency_responsible, invert = TRUE), ]
agency_6 <- agency_5[grep('DPH - Environmental Health - HazWaste Queue',
                          agency_5$agency_responsible, invert = TRUE), ]
agency_7 <- agency_6[grep('DBI Building Inspection Queue',
                          agency_6$agency_responsible, invert = TRUE), ]
agency_8 <- agency_7[grep('DPT - Meters - G',
                          agency_7$agency_responsible, invert = TRUE), ]
agency_9 <- agency_8[grep('DPT Abandoned Vehicles Work Queue',
                          agency_8$agency_responsible, invert = TRUE), ]
agency_10 <- agency_9[grep('DPT Meter_Bike Queue',
                           agency_9$agency_responsible, invert = TRUE), ]
agency_11 <- agency_10[grep('DPT SignShop 06 Queue',
                            agency_10$agency_responsible, invert = TRUE), ]
agency_12 <- agency_11[grep('DPT SignShop 06 Queue',
                            agency_11$agency_responsible, invert = TRUE), ]
agency_13 <- agency_12[grep('DPW - Bureau of Street Use and Mapping - G',
                            agency_12$agency_responsible, invert = TRUE), ]
agency_14 <- agency_13[grep('DPW - Bureau of Urban Forestry - G',
                            agency_13$agency_responsible, invert = TRUE), ]
agency_15 <- agency_14[grep('DPW BSM Queue',
                            agency_14$agency_responsible, invert = TRUE), ]
agency_16 <- agency_15[grep('HSOC Individual Queue',
                            agency_15$agency_responsible, invert = TRUE), ]
#sewer maintenance.
agency_17 <- agency_16[grep('PUC - Sewer - G',
                            agency_16$agency_responsible, invert = TRUE), ]
agency_18 <- agency_17[grep('HSOC Queue',
                            agency_17$agency_responsible, invert = TRUE), ]
agency_19 <- agency_18[grep('PUC - Water - Graffiti Queue',
                            agency_18$agency_responsible, invert = TRUE), ]
agency_20 <- agency_19[grep('PUC Sewer Ops',
                            agency_19$agency_responsible, invert = TRUE), ]
agency_21 <- agency_20[grep('PUC Streetlights Queue',
                            agency_20$agency_responsible, invert = TRUE), ]
agency_22 <- agency_21[grep('Recology_Abandoned',
                            agency_21$agency_responsible, invert = TRUE), ]
agency_23 <- agency_22[grep('Recology_Litter',
                            agency_22$agency_responsible, invert = TRUE), ]
agency_24 <- agency_23[grep('Recology_Overflowing',
                            agency_23$agency_responsible, invert = TRUE), ]
agency_25 <- agency_24[grep('SFMTA - Parking Enforcement - G',
                            agency_24$agency_responsible, invert = TRUE), ]
agency_26 <- agency_25[grep('SFMTA - Residential Parking Permit Program Queue',
                            agency_25$agency_responsible, invert = TRUE), ]
agency_27 <- agency_26[grep('SSP - MTA General Complaints Queue',
                            agency_26$agency_responsible, invert = TRUE), ]
agency_28 <- agency_27[grep('	US Postal Service Maintenance Queue',
                            agency_27$agency_responsible, invert = TRUE), ]

sanfran_status_notes <- agency_28[grep('HSOC Queue - Other Jurisdiction Hold',
                                       agency_28$agency_responsible, invert = TRUE), ]


# Filter out NA values in lat/lon columns. Will break downstream if not clean.
sanfran_calls_no_na = filter(sanfran_status_notes, point.latitude != "NA")

# Filter out missclassification from narrative info in the status notes.  
sanfran_calls_no_na_1 <- sanfran_calls_no_na[grep("Duplicate", 
                                                  sanfran_calls_no_na$status_notes, invert = TRUE), ]
sanfran_calls_no_na_2 <- sanfran_calls_no_na_1[grep("nothing", 
                                                    sanfran_calls_no_na_1$status_notes, invert = TRUE), ]
sanfran_calls_no_na_3 <- sanfran_calls_no_na_2[grep("Nothing", 
                                                    sanfran_calls_no_na_2$status_notes, invert = TRUE), ]
sanfran_calls_no_na_4 <- sanfran_calls_no_na_3[grep("Case Transferred", 
                                          sanfran_calls_no_na_3$status_notes, invert = TRUE), ]
sanfran_calls_no_na_5 <- sanfran_calls_no_na_4[grep("Insufficient Information",sanfran_calls_no_na_4$status_notes, invert = TRUE), ]
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
sanfran_calls_no_na_14 <- sanfran_calls_no_na_13[grep("Case is Invalid", 
                                                      sanfran_calls_no_na_13$status_notes, invert = TRUE), ]
sanfran_calls_no_na_15 <- sanfran_calls_no_na_14[grep("noting", sanfran_calls_no_na_14$status_notes, invert = TRUE), ]
sanfran_calls_no_na_16 <- sanfran_calls_no_na_15[grep("see anything",
                                                      sanfran_calls_no_na_15$status_notes, invert = TRUE), ]
sanfran_calls_no_na_17 <- sanfran_calls_no_na_16[grep("dont see any", 
                                                      sanfran_calls_no_na_16$status_notes, invert = TRUE), ]
sanfran_calls_no_na_18 <- sanfran_calls_no_na_17[grep("Not thing", 
                                                      sanfran_calls_no_na_17$status_notes, invert = TRUE), ]
sanfran_calls_no_na_19 <- sanfran_calls_no_na_18[grep("not at", sanfran_calls_no_na_18$status_notes, invert = TRUE), ]
sanfran_calls_no_na_20 <- sanfran_calls_no_na_19[grep("no poop", sanfran_calls_no_na_19$status_notes, invert = TRUE), ]
sanfran_calls_no_na_21 <- sanfran_calls_no_na_20[grep("see this", sanfran_calls_no_na_20$status_notes, invert = TRUE), ]
sanfran_calls_no_na_22 <- sanfran_calls_no_na_21[grep("wasnt there", sanfran_calls_no_na_21$status_notes, invert = TRUE), ]
sanfran_calls_no_na_23 <- sanfran_calls_no_na_22[grep("looked both", sanfran_calls_no_na_22$status_notes, invert = TRUE), ]

# more from agency responsible column.
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
sanfran_calls_no_na_47 <- sanfran_calls_no_na_46[grep("no dead rat", sanfran_calls_no_na_46$status_notes, invert = TRUE), ]
sanfran_calls_no_na_48 <- sanfran_calls_no_na_47[grep("no human feces", sanfran_calls_no_na_47$status_notes, invert = TRUE), ]
sanfran_calls_no_na_49 <- sanfran_calls_no_na_48[grep('invalid address', sanfran_calls_no_na_48$status_notes, invert = TRUE), ]
sanfran_calls_no_na_50 <- sanfran_calls_no_na_49[grep( 'no debris in the area', sanfran_calls_no_na_49$status_notes, invert = TRUE), ]
sanfran_calls_no_na_51 <- sanfran_calls_no_na_50[grep('NOTHING FOUND', sanfran_calls_no_na_50$status_notes, invert = TRUE), ]
sanfran_calls_no_na_52 <- sanfran_calls_no_na_51[grep( 'TRANS TO RECO',sanfran_calls_no_na_51$status_notes, invert = TRUE), ]
sanfran_calls_no_na_53 <- sanfran_calls_no_na_52[grep("Cancel per 311",sanfran_calls_no_na_52$status_notes, invert = TRUE), ]
sanfran_calls_no_na_54 <- sanfran_calls_no_na_53[grep("not remove homeless",sanfran_calls_no_na_53$status_notes, invert = TRUE), ]
sanfran_calls_no_na_55 <- sanfran_calls_no_na_54[grep("INCORRECT CATEGORY",sanfran_calls_no_na_54$status_notes, invert = TRUE), ]
sanfran_calls_no_na_56 <- sanfran_calls_no_na_55[grep("Location not entered",sanfran_calls_no_na_55$status_notes, invert = TRUE), ]
sanfran_calls_no_na_57 <- sanfran_calls_no_na_56[grep('No human waste found',sanfran_calls_no_na_55$status_notes, invert = TRUE), ]
sanfran_calls_no_na_58 <- sanfran_calls_no_na_57[grep('NO HUMAN WASTE',sanfran_calls_no_na_57$status_notes, invert = TRUE), ]
sanfran_calls_no_na_59 <- sanfran_calls_no_na_58[grep( 'not there',sanfran_calls_no_na_58$status_notes, invert = TRUE), ]
sanfran_calls_no_na_60 <- sanfran_calls_no_na_59[grep(  'no items visable',sanfran_calls_no_na_59$status_notes, invert = TRUE), ]
sanfran_calls_no_na_61 <- sanfran_calls_no_na_60[grep(  'GRAFFITI',sanfran_calls_no_na_60$status_notes, invert = TRUE), ]
sanfran_calls_no_na_62 <- sanfran_calls_no_na_61[grep(  "graffiti",sanfran_calls_no_na_61$status_notes, invert = TRUE), ]
sanfran_calls_no_na_63 <- sanfran_calls_no_na_62[grep( "didnt see piles",sanfran_calls_no_na_62$status_notes, invert = TRUE), ]
sanfran_calls_no_na_64 <- sanfran_calls_no_na_63[grep( 'recology',sanfran_calls_no_na_63$status_notes, invert = TRUE), ]
sanfran_calls_no_na_65 <- sanfran_calls_no_na_64[grep( 'Recology',sanfran_calls_no_na_64$status_notes, invert = TRUE), ]
sanfran_calls_no_na_66 <- sanfran_calls_no_na_65[grep('theres birds',sanfran_calls_no_na_65$status_notes, invert = TRUE), ]
sanfran_calls_no_na_67 <- sanfran_calls_no_na_66[grep( 'this stuff',sanfran_calls_no_na_66$status_notes, invert = TRUE), ]
sanfran_calls_no_na_68 <- sanfran_calls_no_na_67[grep( 'Animal Care',sanfran_calls_no_na_67$status_notes, invert = TRUE), ]
sanfran_calls_no_na_69 <- sanfran_calls_no_na_68[grep(  'nothin here',sanfran_calls_no_na_68$status_notes, invert = TRUE), ]
sanfran_calls_no_na_70 <- sanfran_calls_no_na_69[grep(  'debris',sanfran_calls_no_na_69$status_notes, invert = TRUE), ]
sanfran_calls_no_na_71 <- sanfran_calls_no_na_70[grep(  'thats garbage',sanfran_calls_no_na_70$status_notes, invert = TRUE), ]
sanfran_calls_no_na_72 <- sanfran_calls_no_na_71[grep(  'does not have any feces',sanfran_calls_no_na_71$status_notes, invert = TRUE), ]
sanfran_calls_no_na_73 <- sanfran_calls_no_na_72[grep(  'loose garbage',sanfran_calls_no_na_72$status_notes, invert = TRUE), ]
sanfran_calls_no_na_74 <- sanfran_calls_no_na_73[grep(  'rat removed',sanfran_calls_no_na_73$status_notes, invert = TRUE), ]
sanfran_calls_no_na_75 <- sanfran_calls_no_na_74[grep(  'no waste back',sanfran_calls_no_na_74$status_notes, invert = TRUE), ]
sanfran_calls_no_na_76 <- sanfran_calls_no_na_75[grep(  'NOTHING BUT TRASH',sanfran_calls_no_na_75$status_notes, invert = TRUE), ]
sanfran_calls_no_na_77 <- sanfran_calls_no_na_76[grep(  'unable to find',sanfran_calls_no_na_76$status_notes, invert = TRUE), ]
sanfran_calls_no_na_78 <- sanfran_calls_no_na_77[grep(  'not find any',sanfran_calls_no_na_77$status_notes, invert = TRUE), ]
sanfran_calls_no_na_79 <- sanfran_calls_no_na_78[grep(  'nor did i see',sanfran_calls_no_na_78$status_notes, invert = TRUE), ]
sanfran_calls_no_na_80 <- sanfran_calls_no_na_79[grep(  'any feces',sanfran_calls_no_na_79$status_notes, invert = TRUE), ]
sanfran_calls_no_na_81 <- sanfran_calls_no_na_80[grep(  'and nothim',sanfran_calls_no_na_80$status_notes, invert = TRUE), ]
sanfran_calls_no_na_82 <- sanfran_calls_no_na_81[grep(  "couldn't find",sanfran_calls_no_na_81$status_notes, invert = TRUE), ]
sanfran_calls_no_na_83 <- sanfran_calls_no_na_82[grep(  'could not find',sanfran_calls_no_na_82$status_notes, invert = TRUE), ]
sanfran_calls_no_na_84 <- sanfran_calls_no_na_83[grep(  'wrong address',sanfran_calls_no_na_83$status_notes, invert = TRUE), ]
sanfran_calls_no_na_85 <- sanfran_calls_no_na_84[grep(   'Abandon Vehicles',sanfran_calls_no_na_84$status_notes, invert = TRUE), ]
sanfran_calls_no_na_86 <- sanfran_calls_no_na_85[grep(   'ntohing found',sanfran_calls_no_na_85$status_notes, invert = TRUE), ]
sanfran_calls_no_na_87 <- sanfran_calls_no_na_86[grep(   'Unable To Locate', sanfran_calls_no_na_86$status_notes, invert = TRUE), ]
sanfran_calls_no_na_88 <- sanfran_calls_no_na_87[grep(  'not locate feces', sanfran_calls_no_na_87$status_notes, invert = TRUE), ]
sanfran_calls_no_na_89 <- sanfran_calls_no_na_88[grep( 'no poo', sanfran_calls_no_na_88$status_notes, invert = TRUE), ]
sanfran_calls_no_na_90 <- sanfran_calls_no_na_89[grep( 'vomit', sanfran_calls_no_na_89$status_notes, invert = TRUE), ]
sanfran_calls_no_na_91 <- sanfran_calls_no_na_90[grep( 'no pile of poo', sanfran_calls_no_na_90$status_notes, invert = TRUE), ]
sanfran_calls_no_na_92 <- sanfran_calls_no_na_91[grep( 'personal belonging', sanfran_calls_no_na_91$status_notes, invert = TRUE), ]
sanfran_calls_no_na_93 <- sanfran_calls_no_na_92[grep( 'claimed', sanfran_calls_no_na_92$status_notes, invert = TRUE), ]
sanfran_calls_no_na_94 <- sanfran_calls_no_na_93[grep( 'needles', sanfran_calls_no_na_93$status_notes, invert = TRUE), ]
sanfran_calls_no_na_95 <- sanfran_calls_no_na_94[grep( 'cant locate', sanfran_calls_no_na_94$status_notes, invert = TRUE), ]
sanfran_calls_no_na_96 <- sanfran_calls_no_na_95[grep( 'Trash', sanfran_calls_no_na_95$status_notes, invert = TRUE), ]
sanfran_calls_no_na_97 <- sanfran_calls_no_na_96[grep( 'dog poop', sanfran_calls_no_na_96$status_notes, invert = TRUE), ]
sanfran_calls_no_na_98 <- sanfran_calls_no_na_97[grep( 'trash', sanfran_calls_no_na_97$status_notes, invert = TRUE), ]
sanfran_calls_no_na_99 <- sanfran_calls_no_na_98[grep( 'items', sanfran_calls_no_na_98$status_notes, invert = TRUE), ]
sanfran_calls_no_na_100 <- sanfran_calls_no_na_99[grep( 'glass', sanfran_calls_no_na_99$status_notes, invert = TRUE), ]
sanfran_calls_no_na_101 <- sanfran_calls_no_na_100[grep( 'Dup', sanfran_calls_no_na_100$status_notes, invert = TRUE), ]
sanfran_calls_no_na_102 <- sanfran_calls_no_na_101[grep( 'Paint', sanfran_calls_no_na_101$status_notes, invert = TRUE), ]
sanfran_clean_calls <- sanfran_calls_no_na_102[grep("nothing", sanfran_calls_no_na_102$status_notes, invert = TRUE), ]

# Reformat new lat/long vector to preserve numeric length (may be excessive)
sanfran_clean_calls$lat_new <- as.double(sanfran_clean_calls$lat)
sanfran_clean_calls$long_new <- as.double(sanfran_clean_calls$long)

# Select duplicated lat/long coordinates. 
dupes_lat <- duplicated(sanfran_clean_calls$lat_new)
lat_select <- sanfran_clean_calls[dupes_lat, ]
dupes_long <- duplicated(lat_select$long_new)
latlong_dupes <- lat_select[dupes_long, ]

# Set interval of time to map.
map_these_2019 <- subset(latlong_dupes, 
                         as.data.frame.Date(requested_datetime) 
                         >= "2019-01-01"& 
                           as.data.frame.Date(requested_datetime)
                         < "2020-01-01")


# Download shapefile for density window. Change file name to 'sf_nhoods.shp'
# https://data.sfgov.org/Geographic-Locations-and-Boundaries/SF-Find-Neighborhoods/pty2-tcw4

# Load shapefile for PPA density window
nhoods <- readShapeSpatial('sf_nhoods.shp')

# Check class to verify.
class(nhoods)

# Bind neighborhood shape. 
nhoods_asone <- unionSpatialPolygons(nhoods, nhoods@data[["name"]])
zone_kern <- as(nhoods_asone, "SpatialPolygons")

# Restructure to owin format.
zone_owin_2019 <- as(zone_kern, "owin")

# detach metadata from coords data.frame 
zone_raw_coords_2019 <- subset(map_these_2019, 
                               select = c('long', 'lat'))

# set needed Coordinate Reference System.
zone_points_2019 = st_as_sf(zone_raw_coords_2019, coords 
                            = c('long','lat'), crs = 26910)

# Restructure points as ppp class.
zone_ppp_2019 <- as(as_Spatial(zone_points_2019), "ppp") 


# set window 
Window(zone_ppp_2019) <- zone_owin_2019

# plot validity check
plot(zone_ppp_2019, main=NULL, cols=rgb(0,0,0,.2), pch=20)

# create quadrats.
Q19 <- quadratcount(zone_ppp_2019, nx= 50, ny=50)

# Test plot.
plot(zone_ppp_2019, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q19, add=TRUE)  # Add quadrat grid

# Set density.
Q19.d <- intensity(Q19)

# Plot the density
plot(intensity(Q19, image=TRUE), main=NULL, las=1) 


# Rasterize density layer
zone_rast_2019 <- raster(intensity(Q19, image=TRUE)) 

# Get in the weeds setting the CRS with an old fashion proj4 string. :p
crs(zone_rast_2019) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Set a pallette for your raster file. 
pal <- colorNumeric(c("blue", "orange", "red"), values(zone_rast_2019),
                    na.color = "transparent")

# Make leaflet map with dplyr pipes. 
citywide_incid_2019 =
  leaflet() %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addRasterImage(zone_rast_2019, colors = pal, opacity = 0.6) %>% 
  addResetMapButton()
citywide_incid_2019

# fin. 