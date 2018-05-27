#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  generate variables for flood and routes dataset                                        |
#   |  assign each route with whether or not they encountered a flood                         |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#   |  Updated on 04/12/2018 to consolidate script based on updated version of project        |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Edited on 05/27/2018 to use generate flood variables for crawled trips (all modes)

# Clear workspace

rm(list=ls())

# Set working directory

setwd("/home/bdeep/share/projects/Congestion/")

# Load function to install and load packages

source("intermediate/floods/environment.R")

# Load required packages

packages <- c("sp", 
              "maptools", 
              "rgeos", 
              "rgdal", 
              "raster", 
              "foreach",
              "lubridate",
              "dplyr", 
              "data.table")
lapply(packages, pkgTest)

# Input

# 2016 - 2017 Floods 
flood.path <- "intermediate/floods/floods.rds"

# 2012 survey trip routes (all modes) via private car
routes.dsn <- "stores/floods/"
routes.path <- "routes buffer"

# 2012 survey data 
master.path <- "intermediate/floods/2012-trips.rds"

# 2016 - 2017 crawled trips data (car trips only)
# analysis.path <- "intermediate/floods/analysis data.rds"

# 2016 - 2017 crawled trips data (all modes)
analysis.path <- "intermediate/floods/analysis data - all modes.rds"

# Hourly average rain data (INMET)
rain.path <- "stores/floods/rain/rain.rds"

# Output 

out.path <- "intermediate/floods/floods-model.rds"

# Clean floods data ----------------------------------------------------------------------------

# Read floods data (FD = Floods Data)
FD <- read.csv(flood.path, header = TRUE)

# Replace 'none' with NA

FD[FD == "None"] <- NA

# Removing flood events that have NAs for all three 'min_dist' columns
# i.e. exclude flood events that we were not able to geocode

FD <- FD[which(!(is.na(FD$min_dist_ref | FD$min_dist_local | FD$min_dist_inter))),]

# Finding minimum value of distance from reverse verification for each flood event
# i.e. use the most accurate set of coordinates 

FD1 <- FD[,c("min_dist_local", "min_dist_ref","min_dist_inter")]
FD$min_dist <- colnames(FD1)[apply(FD1,1,which.min)]

# Generate lat/longs for each flood event by selecting lat/longs from address that gives us the lowest value for 'min_dist'

FD$lat <- ifelse(FD$min_dist == "min_dist_local", as.character(FD$lat_local),
                 ifelse(FD$min_dist == "min_dist_ref", as.character(FD$lat_referencia),
                        ifelse(FD$min_dist == "min_dist_inter", as.character(FD$lat_intersection), NA)))

FD$long <- ifelse(FD$min_dist == "min_dist_local", as.character(FD$lng_local),
                  ifelse(FD$min_dist == "min_dist_ref", as.character(FD$lng_referencia),
                         ifelse(FD$min_dist == "min_dist_inter", as.character(FD$lng_intersection), NA)))

# ----------------------------------------------------------------------------------------------
# Generate new end date for flood events that cross into the next day

# Format date variables

FD$DATA <- format(as.POSIXct(strptime(FD$DATA,"%d-%b-%y",tz="")) ,format = "%Y-%m-%d")
FD$DATA <- as.Date(FD$DATA)

# Generate timestamp variables for flood start time and flood end time

FD$flood.time <- format(as.POSIXct(strptime(paste(FD$DATA, FD$INICIO),"%Y-%m-%d %H:%M",tz="")) ,format = "%Y-%m-%d %H:%M")
FD$flood.fim <- format(as.POSIXct(strptime(paste(FD$DATA, FD$FIM),"%Y-%m-%d %H:%M",tz="")) ,format = "%Y-%m-%d %H:%M")

# Generate variable for duration of flood (minutes)

FD$DURACAO <- difftime(FD$flood.fim, FD$flood.time, tz = "", units = "mins")
FD$DURACAO <- as.numeric(FD$DURACAO)
summary(FD$DURACAO)

# Create variable for end date of flood if end time of flood event crosses into next day
# If generated duration is negative (which is impossible), we assume that the end time (FIM) refers to the time on the following day
# Hence, we create another variable to reflect the corrected end date 


# If flood duration is negative, add one day to reported flood date

FD$DATA.FIM <- ifelse(FD$DURACAO < 0, as.Date(ymd(FD$DATA) + days(1)), FD$DATA)
FD$DATA.FIM <- as.Date(FD$DATA.FIM, origin = "1970-01-01")

# Generate corrected flood end timestamp

FD$flood.fim <- as.POSIXct(paste(FD$DATA.FIM, FD$FIM), format = "%Y-%m-%d %H:%M")

# Generate variable for duration of flood (minutes) using corrected flood end timestamp

FD$DURACAO <- difftime(FD$flood.fim, FD$flood.time, tz = "", units = "mins")
FD$DURACAO <- as.numeric(FD$DURACAO)

# Generate summary statistics to check that all flood durations are positive

summary(FD$DURACAO)

# Exclude NAs
FD <- FD[which(!is.na(FD$DURACAO)),]

# Create ID variable 

FD$FID <- seq.int(nrow(FD))

# Floods SpatialPointsDataFrame ---------------------------------------------------------------

FD$long <- as.numeric(as.character(FD$long))
FD$lat <- as.numeric(as.character(FD$lat))

# Generate vector for flood coordinates

coords <- cbind(FD$long,FD$lat)

# Create SpatialPointsDataFrame 

floods.spdf <- SpatialPointsDataFrame(coords, FD, proj4string = CRS("+proj=longlat +ellps=WGS84"))

# Ensure that floods layer has the right coordinate reference system (WGS84)

floods.spdf <- spTransform(floods.spdf, CRS("+proj=longlat +ellps=WGS84"))

# Spatial intersect ----------------------------------------------------------------------------

# Read routes shapefile (200 m buffer of survey trip routes crawled using OSRM API)

RD200 <- readOGR(dsn = routes.dsn, layer = routes.path)
RD200 <- spTransform(RD200, CRS("+proj=longlat +ellps=WGS84"))

# Intersect trip routes and floods 

intersect <- RD200[floods.spdf,]
intersect.df <- as.data.frame(intersect@data)

# intersect.df gives us the trip ids and trip lengths of trips that intersected with flood events

# Assign each trip the attributes of the flood event it intersected with

intersect <- over(RD200, floods.spdf)
RD200.df <- as.data.frame(RD200@data)

# Attach intersect dataset to routes dataset
RD200.df <- cbind(RD200.df, intersect)

# Merge routes + floods dataset with crawled trips dataset ---------------------------------------

# Read survey trips dataset (TD = Trips Data)

TD <- readRDS(master.path)
TD <- as.data.table(TD[,c("ID_ORDEM")])

names(TD)[names(TD) == "V1"] <- "ID_ORDEM"


# Merge floods + trips dataset with master trips dataset
# This gives us the set of survey trips that intersect spatially with blocks / floods 

TD <- merge(RD200.df, TD, by = "ID_ORDEM", all = TRUE)

# read crawled trips dataset 

AD <- readRDS(analysis.path)

# Convert into data table to make merge more efficient

TD <- as.data.table(TD)
AD <- as.data.table(AD)

# Merge survey trips dataset with crawled trips dataset
# N.B. Each trip may intersect with more than one flood event, this merge creates repeated rows of each crawled trip

names(AD)[which(names(AD) == "trip_id")] <- "ID_ORDEM"
AD <- merge(TD, AD, by = "ID_ORDEM", all = TRUE)

# Remove unwanted columns

names(AD)
AD <- AD[,-c(285:304)]

# Collapse dataset into one with unique rows for each crawled trip -----------------------------

# Generate 'flooded' variable that takes 1 if route experienced a flood event and 0 otherwise.

AD$flooded <- ifelse(AD$SITUACAO == "transitavel",1, 0)
AD$flooded[which(is.na(AD$flooded))] <- 0

# Generate 'blocked' variable that takes 1 if the route experienced a flood event and was blocked
# and 0 otherwise. 

AD$blocked <- ifelse(AD$SITUACAO == "intransitavel", 1, 0)
AD$blocked[which(is.na(AD$blocked))] <- 0

# Generate variable for trips that do not intersect with trips 

AD$no.flood <- ifelse(AD$flooded == 0 & AD$blocked == 0, 1, 0)
summary(AD$no.flood)

# Generate variable for crawled trip start time

hour_minute <- as.data.table(str_split_fixed(AD$hour_minute, "_", 2))
names(hour_minute)[names(hour_minute) == "V2"] <- "minute"
AD <- cbind(AD, hour_minute$minute)

AD$start.time <- ISOdatetime(year = AD$year,
                              month = AD$month,
                              day = AD$day,
                              hour = AD$hour,
                              min = AD$minute,
                              sec = 0,
                              tz ="")

AD$start.time <- format(as.POSIXct(strptime(AD$start.time,"%Y-%m-%d %H:%M",tz=""), format = "%Y-%m-%d %H:%M"))


# Generate variable for AD that occur in same time as AD and blocked

AD$blocks <- ifelse((difftime(AD$start.time, AD$flood.time,tz = "", units = "mins") >= 0 &
                          difftime(AD$flood.fim, AD$start.time,tz = "", units = "mins") >= 0) &
                         AD$blocked == 1, 1, 0)
AD$blocks[which(is.na(AD$blocks))] <- 0


# Generate variable for AD that occur in same time as AD and flooded

AD$floods <- ifelse(difftime(AD$start.time, AD$flood.time) >= 0 &
                         difftime(AD$flood.fim, AD$start.time) >= 0 &
                         AD$flooded == 1, 1, 0)
AD$floods[which(is.na(AD$floods))] <- 0

# Subset to observations which encounter blocks

trips.blocks <- trips[which(trips$blocks == 1),]

# Add a variable which indicates a number for each block encountered by each TID

trips.blocks <- trips.blocks %>%
  add_count(TID)

# Subset to observations which encounter floods

trips.floods <- trips[which(trips$floods == 1),]

# Add a variable which indicates a number for each flood encountered by each TID

trips.floods <- trips.floods %>%
  add_count(TID)

# The maximum number of blocks / floods each trip encounters is 13
# Create up to 13 variables for block / flood duration

trips.blocks$duration1 <- as.numeric(ifelse(trips.blocks$n == 1, trips.blocks$DURACAO, NA))
trips.blocks$duration2 <- as.numeric(ifelse(trips.blocks$n == 2, trips.blocks$DURACAO, NA))
trips.blocks$duration3 <- as.numeric(ifelse(trips.blocks$n == 3, trips.blocks$DURACAO, NA))
trips.blocks$duration4 <- as.numeric(ifelse(trips.blocks$n == 4, trips.blocks$DURACAO, NA))
trips.blocks$duration5 <- as.numeric(ifelse(trips.blocks$n == 5, trips.blocks$DURACAO, NA))
trips.blocks$duration6 <- as.numeric(ifelse(trips.blocks$n == 6, trips.blocks$DURACAO, NA))
trips.blocks$duration7 <- as.numeric(ifelse(trips.blocks$n == 7, trips.blocks$DURACAO, NA))
trips.blocks$duration8 <- as.numeric(ifelse(trips.blocks$n == 8, trips.blocks$DURACAO, NA))
trips.blocks$duration9 <- as.numeric(ifelse(trips.blocks$n == 9, trips.blocks$DURACAO, NA))
trips.blocks$duration10 <- as.numeric(ifelse(trips.blocks$n == 10, trips.blocks$DURACAO, NA))
trips.blocks$duration11 <- as.numeric(ifelse(trips.blocks$n == 11, trips.blocks$DURACAO, NA))
trips.blocks$duration12 <- as.numeric(ifelse(trips.blocks$n == 12, trips.blocks$DURACAO, NA))
trips.blocks$duration13 <- as.numeric(ifelse(trips.blocks$n == 13, trips.blocks$DURACAO, NA))

trips.floods$fduration1 <- as.numeric(ifelse(trips.floods$n == 1, trips.floods$DURACAO, NA))
trips.floods$fduration2 <- as.numeric(ifelse(trips.floods$n == 2, trips.floods$DURACAO, NA))
trips.floods$fduration3 <- as.numeric(ifelse(trips.floods$n == 3, trips.floods$DURACAO, NA))
trips.floods$fduration4 <- as.numeric(ifelse(trips.floods$n == 4, trips.floods$DURACAO, NA))
trips.floods$fduration5 <- as.numeric(ifelse(trips.floods$n == 5, trips.floods$DURACAO, NA))
trips.floods$fduration6 <- as.numeric(ifelse(trips.floods$n == 6, trips.floods$DURACAO, NA))
trips.floods$fduration7 <- as.numeric(ifelse(trips.floods$n == 7, trips.floods$DURACAO, NA))
trips.floods$fduration8 <- as.numeric(ifelse(trips.floods$n == 8, trips.floods$DURACAO, NA))
trips.floods$fduration9 <- as.numeric(ifelse(trips.floods$n == 9, trips.floods$DURACAO, NA))
trips.floods$fduration10 <- as.numeric(ifelse(trips.floods$n == 10, trips.floods$DURACAO, NA))
trips.floods$fduration11 <- as.numeric(ifelse(trips.floods$n == 11, trips.floods$DURACAO, NA))
trips.floods$fduration12 <- as.numeric(ifelse(trips.floods$n == 12, trips.floods$DURACAO, NA))
trips.floods$fduration13 <- as.numeric(ifelse(trips.floods$n == 13, trips.floods$DURACAO, NA))

# Generate dataset which has unique rows for each TID for trips with blocks
# Dataset has a column for the duration of each flood event if the trip encounters multiple events

trips.blocks <- group_by(trips.blocks, TID)

trips.blocks <- summarize(trips.blocks,
                          duration1 = mean(duration1, na.rm = TRUE),
                          duration2 = mean(duration2, na.rm = TRUE),
                          duration3 = mean(duration3, na.rm = TRUE),
                          duration4 = mean(duration4, na.rm = TRUE),
                          duration5 = mean(duration5, na.rm = TRUE),
                          duration6 = mean(duration6, na.rm = TRUE),
                          duration7 = mean(duration7, na.rm = TRUE),
                          duration8 = mean(duration8, na.rm = TRUE),
                          duration9 = mean(duration9, na.rm = TRUE),
                          duration10 = mean(duration10, na.rm = TRUE),
                          duration11 = mean(duration11, na.rm = TRUE),
                          duration12 = mean(duration12, na.rm = TRUE),
                          duration13 = mean(duration13, na.rm = TRUE))

# Generate dataset which has unique rows for each TID for trips with floods
# Dataset has a column for the duration of each flood event if the trip encounters multiple events

trips.floods <- group_by(trips.floods, TID)

trips.floods <- summarize(trips.floods,
                          fduration1 = mean(fduration1, na.rm = TRUE),
                          fduration2 = mean(fduration2, na.rm = TRUE),
                          fduration3 = mean(fduration3, na.rm = TRUE),
                          fduration4 = mean(fduration4, na.rm = TRUE),
                          fduration5 = mean(fduration5, na.rm = TRUE),
                          fduration6 = mean(fduration6, na.rm = TRUE),
                          fduration7 = mean(fduration7, na.rm = TRUE),
                          fduration8 = mean(fduration8, na.rm = TRUE),
                          fduration9 = mean(fduration9, na.rm = TRUE),
                          fduration10 = mean(fduration10, na.rm = TRUE),
                          fduration11 = mean(fduration11, na.rm = TRUE),
                          fduration12 = mean(fduration12, na.rm = TRUE),
                          fduration13 = mean(fduration13, na.rm = TRUE))

# Merge block and flood durations to dataset with unique observations for crawled trips

AD <- readRDS(analysis.path)

AD <- merge(AD, trips.blocks, by = "TID", all.x = TRUE)
AD <- merge(AD, trips.floods, by = "TID", all.x = TRUE)

# Create indicator variable for blocks and floods

AD$blocks <- ifelse(!is.na(AD$duration1), 1, 0)
AD$floods <- ifelse(!is.na(AD$fduration1), 1, 0)

# Generate mean block / flood duration

# Generate row means

AD$duration.mean <- rowMeans(AD[,c("duration1",
                                           "duration2",
                                           "duration3",
                                           "duration4",
                                           "duration5",
                                           "duration6",
                                           "duration7",
                                           "duration8",
                                           "duration9",
                                           "duration10",
                                           "duration11",
                                           "duration12",
                                           "duration13")],
                                 na.rm = TRUE)


AD$fduration.mean <- rowMeans(AD[,c("fduration1",
                                            "fduration2",
                                            "fduration3",
                                            "fduration4",
                                            "fduration5",
                                            "fduration6",
                                            "fduration7",
                                            "fduration8",
                                            "fduration9",
                                            "fduration10",
                                            "fduration11",
                                            "fduration12",
                                            "fduration13")],
                                  na.rm = TRUE)

# Replace NAs with 0s for merging with main dataset

AD$duration.mean[which(is.na(AD$duration.mean))] <- 0
AD$duration.mean <- as.numeric(as.character(AD$duration.mean))

AD$fduration.mean[which(is.na(AD$fduration.mean))] <- 0
AD$fduration.mean <- as.numeric(as.character(AD$fduration.mean))

# Rain variables -------------------------------------------------------------------------------

# Generate rain bins

AD$rain[which(is.na(AD$rain))] <- 0

AD$rain.bins <- ifelse(AD$rain == 0, "0",
                          ifelse(AD$rain <= 2.5, "1",
                                 ifelse(AD$rain <= 7.6, "2",
                                        ifelse(AD$rain > 7.6, "3", NA))))


AD$rain.bins1 <- as.numeric(ifelse(AD$rain.bins == "1", 1, 0))
AD$rain.bins2 <- as.numeric(ifelse(AD$rain.bins == "2", 1, 0))
AD$rain.bins3 <- as.numeric(ifelse(AD$rain.bins == "3", 1, 0))

# Generate lagged rain variables

# Read file

rain <- readRDS(rain.path)

# Format hour variable for rain

rain$hour <- as.numeric(rain$hour)

# Format date variable for rain 

rain$date <- as.Date(rain$date)

rain$year <- year(rain$date)
rain$month <- month(rain$date)
rain$day <- day(rain$date)

# Create timestamp variable for rain with year, month, day and hour

rain$rain.date <- ISOdatetime(year = rain$year,
                              month = rain$month,
                              day = rain$day,
                              hour = rain$hour,
                              min = 0,
                              sec = 0,
                              tz ="")

rain$rain.date <- format(as.POSIXct(strptime(rain$rain.date,"%Y-%m-%d %H:%M",tz=""), format = "%Y-%m-%d %H:%M"))

# Define moving average function that does not include the current observation

mavback <- function(x,n){ stats::filter(x, c(0, rep(1/n,n)), sides=1) }

# Generating moving average of rainfall from the previous 12 hours

rain$mav12 <- mavback(rain$rain, 12)

# Generating moving average of rainfall from the previous 9 hours

rain$mav9 <- mavback(rain$rain, 9)

# Generating moving average of rainfall from the previous 6 hours 

rain$mav6 <- mavback(rain$rain, 6)

# Generating moving average of rainfall from the previous 6 hours 

rain$mav5 <- mavback(rain$rain, 5)

# Generating moving average of rainfall from the previous 3 hours 

rain$mav3 <- mavback(rain$rain, 3)

# Generating lags of hourly rainfall

rain$lag1 <- sapply(1:nrow(rain), function(x)rain$rain[x-1])
rain$lag2 <- sapply(1:nrow(rain), function(x)rain$rain[x-2])
rain$lag3 <- sapply(1:nrow(rain), function(x)rain$rain[x-3])
rain$lag4 <- sapply(1:nrow(rain), function(x)rain$rain[x-4])
rain$lag5 <- sapply(1:nrow(rain), function(x)rain$rain[x-5])
rain$lag6 <- sapply(1:nrow(rain), function(x)rain$rain[x-6])
rain$lag7 <- sapply(1:nrow(rain), function(x)rain$rain[x-7])
rain$lag8 <- sapply(1:nrow(rain), function(x)rain$rain[x-8])
rain$lag9 <- sapply(1:nrow(rain), function(x)rain$rain[x-9])
rain$lag10 <- sapply(1:nrow(rain), function(x)rain$rain[x-10])
rain$lag11 <- sapply(1:nrow(rain), function(x)rain$rain[x-11])
rain$lag12 <- sapply(1:nrow(rain), function(x)rain$rain[x-12])

# Converting first observations into NAs

rain$lag1[1] <- NA
rain$lag2[1:2] <- NA
rain$lag3[1:3] <- NA
rain$lag4[1:4] <- NA
rain$lag5[1:5] <- NA
rain$lag6[1:6] <- NA
rain$lag7[1:7] <- NA
rain$lag8[1:8] <- NA
rain$lag9[1:9] <- NA
rain$lag10[1:10] <- NA
rain$lag11[1:11] <- NA
rain$lag12[1:12] <- NA

# Create timestamp variable in crawled trips dataset for merge
# Subtract one day from rain.date because interviews were conducted based on trips taken the day before

AD$rain.date <- ISOdatetime(year = AD$year,
                                month = AD$month,
                                day = AD$day - 1,
                                hour = AD$hour,
                                min = 0,
                                sec = 0,
                                tz = "")

AD$rain.date <- format(as.POSIXct(strptime(AD$rain.date,"%Y-%m-%d %H:%M",tz=""), format = "%Y-%m-%d %H:%M"))


# Merge rain variables with crawled trips dataset

AD <- merge(AD, rain, by = "rain.date")


# Save file ------------------------------------------------------------------------------------

saveRDS(AD, out.path)

rm(list = ls())
gc()



