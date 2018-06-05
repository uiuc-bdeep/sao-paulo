# --------------------------------------------------------------------------------------- #
#   Plot difference between average travel time with blocks and average travel time       #
#                                                                                         #
#   Created by: Amanda Ang                                                                #
#               Big Data Environmental Economics and Policy                               #
#                                                                                         #
#   Created on: 06/04/2018                                                                #
# --------------------------------------------------------------------------------------- #

# clear workspace

rm(list = ls())

# set working directory

setwd("/home/bdeep/share/projects/Congestion/")

# function for loading packages

pkgTest <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}


# load required packages

packages <- c("dplyr",
              "ggplot2")

lapply(packages, pkgTest)

# input

trips.path <- "intermediate/floods/floods-model.rds"

# output

out.path <- "views/floods/"

# read file

trips <- readRDS(trips.path)

# generate the set of trip IDs which encounter blocks / floods 

trips <- group_by (trips, ID_ORDEM)
trips1 <- summarise(trips, blocks = sum(blocks))

block.trips <- trips1[trips1$blocks > 0,]

trips1 <- summarise(trips, floods = sum(floods))

flood.trips <- trips1[trips1$floods > 0,] 

rm(trips1)

# generate the average travel time by time of day according to blocks / floods status

trips$blocks <- as.factor(trips$blocks)
trips$floods <- as.factor(trips$floods)

trips <- group_by(trips, ID_ORDEM, hour.f, blocks)

block.trips.1 <- summarise(trips, tr.time = mean(tr.time))

trips <- group_by(trips, ID_ORDEM, hour.f, floods)

flood.trips.1 <- summarise(trips, tr.time = mean(tr.time))

# subset to trips which encounter blocks or floods

block.trips.2 <- block.trips.1[block.trips.1$blocks == 1,]
names(block.trips.2)[names(block.trips.2) == "tr.time"] <- "block.time"

flood.trips.2 <- flood.trips.1[flood.trips.1$floods == 1,]
names(flood.trips.2)[names(flood.trips.2) == "tr.time"] <- "flood.time"

# subset to trips which do not encounter blocks or floods 

block.trips.3 <- block.trips.1[block.trips.1$blocks == 0,]
names(block.trips.3)[names(block.trips.3) == "tr.time"] <- "no.block"

flood.trips.3 <- flood.trips.1[flood.trips.1$floods == 0,]
names(flood.trips.3)[names(flood.trips.3) == "tr.time"] <- "no.flood"

# merge with trip IDs

block.trips <- merge(block.trips, block.trips.2, by = "ID_ORDEM", all.x = TRUE)
block.trips <- merge(block.trips, block.trips.3, by = "ID_ORDEM", all.x = TRUE)

flood.trips <- merge(flood.trips, flood.trips.2, by = "ID_ORDEM", all.x = TRUE)
flood.trips <- merge(flood.trips, flood.trips.3, by = "ID_ORDEM", all.x = TRUE)

# calculate difference in travel times

block.trips$diff <- block.trips$block.time - block.trips$no.block
flood.trips$diff <- flood.trips$flood.time - flood.trips$no.flood

# plot 

ggplot(block.trips) +
  geom_histogram(aes(diff), binwidth = 1) +
  xlab("Difference in Travel Time /n (Minutes)") +
  ylab("Number of Trips") + 
  ggtitle("Difference in Travel Time",
          subtitle = "Blocked Trips, Grouped by Departure Hour") + 
  theme_bw() 

ggplot(flood.trips) + 
  geom_histogram(aes(diff), binwidth = 1) + 
  xlab("Difference in Travel Time /n (Minutes)") +
  ylab("Number of Trips") +
  ggtitle("Difference in Travel Time", 
          subtitle = "Flooded Trips, Grouped by Departure Hour") +
  theme_bw()



