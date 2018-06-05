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
merged.trips <- merged.trips[,c("TID",
                                "FID",
                                "flood.time",
                                "flood.fim")]
merged.trips <- unique(merged.trips)

# take only the first flood event each trip encounters

merged.trips <- merged.trips %>% 
                add_count(TID)

merged.trips <- merged.trips[merged.trips$n == 1,]

merged.trips$n <- NULL

merged.trips <- as.data.table(merged.trips)
trips <- as.data.table(trips)

trips <- merge(trips, merged.trips, by = "TID", all.x = TRUE)

saveRDS(trips, trips.path)





