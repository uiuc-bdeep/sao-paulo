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

# generate the average travel time by time of day

trips <- group_by(trips, ID_ORDEM, hour.f)

trips1 <- summarise(trips, tr.time = mean(tr.time))

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


# merge with trip IDs

block.trips <- merge(block.trips.2, trips1, by = c("ID_ORDEM","hour.f"), all.x = TRUE)

flood.trips <- merge(flood.trips.2, trips1, by = c("ID_ORDEM","hour.f"), all.x = TRUE)

# calculate difference in travel times

block.trips$diff <- block.trips$block.time - block.trips$tr.time
flood.trips$diff <- flood.trips$flood.time - flood.trips$tr.time

# plot 

ggplot() +
      geom_histogram(aes(diff), 
                     binwidth = 1, data = block.trips) +
      geom_vline(aes(xintercept = mean(block.trips$diff)), color = "red") + 
      ylab("Number of Trips") +
      xlab("Observed Difference in Travel Time \n (Minutes)") +
      ggtitle("Blocked Trips") + 
      coord_cartesian(xlim = c(-12,12)) +
      annotate("text", x = 4, y = 900, label = "Mean = 0.74 minutes", size = 3) + 
      theme_bw() 

ggsave(paste0(out.path, "diff-time-blocks.png"),
       width = 8, height = 5, dpi = 300)

ggplot() +
      geom_histogram(aes(diff), 
                     binwidth = 1, data = flood.trips) +
      geom_vline(aes(xintercept = mean(flood.trips$diff)), color = "red") + 
      ylab("Number of Trips") +
      xlab("Observed Difference in Travel Time \n (Minutes)") +
      ggtitle("Flooded Trips") + 
      coord_cartesian(xlim = c(-20,15)) +
      annotate("text", x = 6, y = 900, label = "Mean = 0.0009 minutes", size = 3) + 
      theme_bw() 

ggsave(paste0(out.path, "diff-time-floods.png"),
       width = 8, height = 5, dpi = 300)

# --------------------------------------------------------------------------------------------------

merged.trips <- readRDS("intermediate/floods/date-merge.rds") 

# generate trip end time

merged.trips$start.time <- format(as.POSIXct(strptime(merged.trips$start.time,"%Y-%m-%d %H:%M",tz="")), 
                                  format = "%Y-%m-%d %H:%M")

merged.trips$end.time <- ymd_hm(merged.trips$start.time) + dminutes(merged.trips$tr.time)

# subset to blocked trips 

block.trips <- merged.trips[merged.trips$blocks == 1,]

# generate remaining flood duration

block.trips$rem.time <- difftime(block.trips$flood.fim, block.trips$end.time, 
                                 tz = "", units = "mins")
block.trips$rem.time <- as.numeric(as.character(block.trips$rem.time))

# some floods may have ended before the trip was completed so assign those 0 for remaining time

block.trips$rem.time[block.trips$rem.time < 0] <- 0

# add to trip time

block.trips$tr.time1 <- block.trips$tr.time + block.trips$rem.time

# collapse to the trip and hour level

block.trips <- group_by(block.trips, ID_ORDEM, hour.f)
block.trips1 <- summarise(block.trips, tr.time1 = mean(tr.time1))


ggplot() +
      geom_histogram(aes(tr.time1), 
                     binwidth = 20, data = block.trips1) +
      geom_vline(aes(xintercept = mean(block.trips1$tr.time1)), color = "red") + 
      ylab("Number of Trips") +
      xlab("Maximum Difference in Travel Time \n (Minutes)") +
      ggtitle("Blocked Trips") + 
      annotate("text", x = 420, y = 450, label = "Mean = 222 minutes", size = 3) + 
      theme_bw() 

ggsave(paste0(out.path, "diff-time-blocks-rem.png"),
       width = 8, height = 5, dpi = 300)



