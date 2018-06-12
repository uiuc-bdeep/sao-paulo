# Generate the average flood duration per spillover trip
# Author: Amanda Ang
# Created on: 06/12/2018

# Load dataset with trip - flood observations 
# i.e. each observation is the trip + the flood that it intersects with 
# There can be multiple observations for the same crawled trip if it intersects with multiple floods

trips.path <- "intermediate/floods/date-merge.rds"
trips <- readRDS(trips.path)

# Generate dataset of spillover trips with all the floods it coincides with

trips <- trips[trips$spillovers == 1,]

# For each crawled trip, generate average flood duration for spillover trips

trips <- group_by(trips, TID)
spillovers <- summarise(trips, mean = mean(delay))

# Merge with main dataset 

rm(trips)
gc()

trips.path <- "intermediate/floods/floods-model.rds"
trips <- readRDS(trips.path) 

trips$mean <- NULL

trips <- as.data.table(trips)
spillovers <- as.data.table(spillovers)

trips <- merge(trips, spillovers, by = "TID", all.x = TRUE)
trips$mean[is.na(trips$mean)] <- 0
