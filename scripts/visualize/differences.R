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
              "lubridate",
              "ggplot2",
              "ggpubr")

lapply(packages, pkgTest)

# input

trips.path <- "intermediate/floods/floods-model.rds"

# output

out.path <- "views/floods/"

# read file

trips <- readRDS(trips.path)

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

block.trips <- merge(block.trips.2, block.trips.3, by = c("ID_ORDEM","hour.f"), all.x = TRUE)

flood.trips <- merge(flood.trips.2, flood.trips.3, by = c("ID_ORDEM","hour.f"), all.x = TRUE)

# calculate difference in travel times

block.trips$diff <- block.trips$block.time - block.trips$no.block
flood.trips$diff <- flood.trips$flood.time - flood.trips$no.flood

# plot 

ggplot() +
      geom_histogram(aes(diff), 
                     binwidth = 1, data = block.trips) +
      ylab("Number of Trips") +
      xlab("Difference in Travel Time \n (Minutes)") +
      ggtitle("Blocked Trips") + 
      theme_bw() 

ggsave(paste0(out.path, "diff-time-blocks.png"),
       width = 8, height = 5, dpi = 300)

ggplot() +
      geom_histogram(aes(diff), 
                     binwidth = 1, data = flood.trips) +
      ylab("Number of Trips") +
      xlab("Difference in Travel Time \n (Minutes)") +
      ggtitle("Flooded Trips") + 
      theme_bw() 

ggsave(paste0(out.path, "diff-time-floods.png"),
       width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------------------------------

merged.trips <- readRDS("intermediate/floods/date-merge.rds") 

# generate trip end time

merged.trips$start.time <- format(as.POSIXct(strptime(merged.trips$start.time,"%Y-%m-%d %H:%M",tz="")) ,format = "%Y-%m-%d %H:%M")

merged.trips$end.time <- merged.trips$start.time + as.difftime(merged.trips$tr.time, units = "mins")
merged.trips$end.time <- format(as.POSIXct(strptime(merged.trips$end.time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d %H:%M:%S")

# subset to blocked trips 

block.trips <- merged.trips[merged.trips$blocks == 1,]

# generate remaining flood duration

block.trips$rem.time <- difftime(block.trips$end.time, block.trips$flood.fim, tz = "", units = "mins")
block.trips$rem.time <- as.numeric(as.character(block.trips$rem.time))


# add to trip time

merged.trips$tr.time1 <- merged.trips$tr.time + merged.trips$rem.time

# generate the average travel time by time of day according to blocks status

trips$blocks <- as.factor(trips$blocks)

trips <- group_by(trips, ID_ORDEM, hour.f, blocks)

block.trips.1 <- summarise(trips, tr.time = mean(tr.time1))

# subset to trips which encounter blocks or floods

block.trips.2 <- block.trips.1[block.trips.1$blocks == 1,]
names(block.trips.2)[names(block.trips.2) == "tr.time"] <- "block.time"

# subset to trips which do not encounter blocks or floods 

block.trips.3 <- block.trips.1[block.trips.1$blocks == 0,]
names(block.trips.3)[names(block.trips.3) == "tr.time"] <- "no.block"

# merge with trip IDs

block.trips <- merge(block.trips.2, block.trips.3, by = c("ID_ORDEM","hour.f"), all.x = TRUE)

# calculate difference in travel times

block.trips$diff <- block.trips$block.time - block.trips$no.block


ggplot() +
      geom_histogram(aes(diff), 
                     binwidth = 1, data = block.trips) +
      ylab("Number of Trips") +
      xlab("Difference in Travel Time \n (Minutes)") +
      ggtitle("Blocked Trips") + 
      coord_cartesian(ylim = c(0, 225)) +
      theme_bw() 

ggsave(paste0(out.path, "diff-time-blocks-rem.png"),
       width = 8, height = 5, dpi = 300)



