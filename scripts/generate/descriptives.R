#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Generate descriptive statistics                                                        |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# edited on 05/31/2018 to create table for districts with most floods and most travel

# clear workspace

rm(list=ls())

# set working directory

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

# load packages 

packages <- c("dplyr", "tidyr", "xtable")
lapply(packages, pkgTest)

# inputs

floods.path <- "intermediate/floods/floods.rds"
HH.path <- "intermediate/floods/floods-intersect.rds"
trips.path <- "intermediate/floods/floods-model.rds"
  
# output

out.path <- "views/floods/"
  
# descriptive statistics for floods dataset ----------------------------------------------------

# read files

floods <- readRDS(floods.path)
names(floods)

# whole dataset
var <- as.data.frame(floods$DURACAO)
sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
sum <- as.data.frame(sum[-1,])

# format names
names(sum)[which(names(sum) == "sum[-1, ]")] <- "Flood Events"

# separate strings from numerical values
sum <- separate(sum, `Flood Events`, into = c("stat", "Flood Events"), sep = ":", extra = "merge")

# convert to numeric
sum$`Flood Events` <- as.numeric(as.character(sum$`Flood Events`))

# transpose
rownames(sum) <- sum$stat
sum$stat <- NULL

sum <- as.data.frame(t(sum))

# add variable for number of observations
sum$Count <- as.numeric(nrow(nofloods))

# add variable for share of total
sum$Share <- nrow(floods) / nrow(floods)

# output
out <- as.data.frame(sum)

# blocks ---------------------------------------------------------------------------------------

blocks <- subset(floods, SITUACAO == "intransitavel")
var <- as.data.frame(blocks$DURACAO)

# save output as data frame
blocks.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
blocks.sum <- as.data.frame(blocks.sum[-1,])

# format names
names(blocks.sum)[which(names(blocks.sum) == "blocks.sum[-1, ]")] <- "Blocks"

# separate strings from numerical values
blocks.sum <- separate(blocks.sum, Blocks, into = c("stat", "Blocks"), sep = ":", extra = "merge")

# convert to numeric
blocks.sum$Blocks <- as.numeric(as.character(blocks.sum$Blocks))

# transpose
rownames(blocks.sum) <- blocks.sum$stat
blocks.sum$stat <- NULL

blocks.sum <- as.data.frame(t(blocks.sum))

# add variable for number of observations
blocks.sum$Count <- as.numeric(nrow(blocks))

# add variable for share of total
blocks.sum$Share <- nrow(blocks) / nrow(floods)

# floods ---------------------------------------------------------------------------------------

floods <- subset(floods, SITUACAO == "transitavel")
var <- as.data.frame(floods$DURACAO)

# save output as data frame
floods.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
floods.sum <- as.data.frame(floods.sum[-1,])

# format names
names(floods.sum)[which(names(floods.sum) == "floods.sum[-1, ]")] <- "Floods"

# separate strings from numerical values
floods.sum <- separate(floods.sum, floods, into = c("stat", "Floods"), sep = ":", extra = "merge")

# convert to numeric
floods.sum$Floods <- as.numeric(as.character(floods.sum$Floods))

# transpose
rownames(floods.sum) <- floods.sum$stat
floods.sum$stat <- NULL

floods.sum <- as.data.frame(t(floods.sum))

# add variable for number of observations
floods.sum$Count <- as.numeric(nrow(floods))

# add variable for share of total
floods.sum$Share <- nrow(floods) / 740

# append to output

out <- rbind(blocks.sum, floods.sum)

rm(blocks, floods)

# survey trips ---------------------------------------------------------------------------------

HH <- readRDS(HH.path)
names(HH)

# subset to trips taken by car

HH <- subset(HH, TIPOVG == 2)

# keeping only relevant variables

HH <- HH[,c("ID_ORDEM",
            "SITUACAO", 
            "DURACAO")]

# whole sample 
var <- as.data.frame(HH$DURACAO)
sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
sum <- as.data.frame(sum[-1,])

# format names
names(sum)[which(names(sum) == "sum[-1, ]")] <- "Survey Trips"

# separate strings from numerical values
sum <- separate(sum, `Survey Trips`, into = c("stat", "Survey Trips"), sep = ":", extra = "merge")

# convert to numeric
sum$`Survey Trips` <- as.numeric(as.character(sum$`Survey Trips`))

# transpose
rownames(sum) <- sum$stat
sum$stat <- NULL

sum <- as.data.frame(t(sum))

# add variable for number of observations
sum$Count <- as.numeric(nrow(HH))

# add variable for share of total
sum$Share <- nrow(HH) / nrow(HH)

# append to output
out <- rbind(out, sum)

# blocks ---------------------------------------------------------------------------------------

blocks <- subset(HH, SITUACAO == "intransitavel")
var <- as.data.frame(blocks$DURACAO)

# save output as data frame
blocks.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
blocks.sum <- as.data.frame(blocks.sum[-1,])

# format names
names(blocks.sum)[which(names(blocks.sum) == "blocks.sum[-1, ]")] <- "Blocks"

# separate strings from numerical values
blocks.sum <- separate(blocks.sum, Blocks, into = c("stat", "Blocks"), sep = ":", extra = "merge")

# transpose
rownames(blocks.sum) <- blocks.sum$stat
blocks.sum$stat <- NULL

blocks.sum <- as.data.frame(t(blocks.sum))

# add variable for number of observations
blocks.sum$Count <- as.numeric(nrow(blocks))

# add variable for share of total
blocks.sum$Share <- nrow(blocks) / 15483

# append to output

out <- rbind(out, blocks.sum)

# floods ---------------------------------------------------------------------------------------

floods <- subset(HH, SITUACAO == "transitavel")
var <- as.data.frame(floods$DURACAO)

# save output as data frame
floods.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
floods.sum <- as.data.frame(floods.sum[-1,])

# format names
names(floods.sum)[which(names(floods.sum) == "floods.sum[-1, ]")] <- "floods"

# separate strings from numerical values
floods.sum <- separate(floods.sum, floods, into = c("stat", "floods"), sep = ":", extra = "merge")

# transpose
rownames(floods.sum) <- floods.sum$stat
floods.sum$stat <- NULL

floods.sum <- as.data.frame(t(floods.sum))

# add variable for number of observations
floods.sum$Count <- as.numeric(nrow(floods))

# add variable for share of total
floods.sum$Share <- nrow(floods) / 15483

# append to output

out <- rbind(out, floods.sum)

rm(blocks,
   blocks.sum, 
   floods,
   floods.sum, 
   var, 
   HH)

gc()

# no blocks or floods --------------------------------------------------------------------------

nofloods <- subset(HH, is.na(SITUACAO))
var <- as.data.frame(nofloods$DURACAO)

# save output as data frame
nofloods.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
nofloods.sum <- as.data.frame(nofloods.sum[-1,])

# format names
names(nofloods.sum)[which(names(nofloods.sum) == "nofloods.sum[-1, ]")] <- "No Blocks or Floods"

# separate strings from numerical values
nofloods.sum <- separate(nofloods.sum, `No Blocks or Floods`, into = c("stat", "No Blocks or Floods"), sep = ":", extra = "merge")

# transpose
rownames(nofloods.sum) <- nofloods.sum$stat
nofloods.sum$stat <- NULL

nofloods.sum <- as.data.frame(t(nofloods.sum))

# add variable for number of observations
nofloods.sum$Count <- as.numeric(nrow(nofloods))

# add variable for share of total
nofloods.sum$Share <- nrow(nofloods) / 15483

# append to output

out <- rbind(out, nofloods.sum)

# crawled trips --------------------------------------------------------------------------------

trips <- readRDS(trips.path)
names(trips)

# exclude NA's 

trips <- trips[which(!is.na(trips$tr.time)),]

# whole sample 
var <- as.data.frame(trips$tr.time)
sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
sum <- as.data.frame(sum[-1,])

# format names
names(sum)[which(names(sum) == "sum[-1, ]")] <- "Crawled Trips"

# separate strings from numerical values
sum <- separate(sum, `Crawled Trips`, into = c("stat", "Crawled Trips"), sep = ":", extra = "merge")

# convert to numeric
sum$`Crawled Trips` <- as.numeric(as.character(sum$`Crawled Trips`))

# transpose
rownames(sum) <- sum$stat
sum$stat <- NULL

sum <- as.data.frame(t(sum))

# add variable for number of observations
sum$Count <- as.numeric(nrow(trips))

# add variable for share of total
sum$Share <- nrow(trips) / nrow(trips)

# append to output
out <- rbind(out, sum)

# No Rain --------------------------------------------------------------------------------------

Rain0 <- subset(trips, rain.bins == 0)
var <- as.data.frame(Rain0$tr.time)
count <- nrow(Rain0) 
share <- nrow(Rain0) / nrow(trips)

# remove dataset to save computing power
rm(Rain0)
gc()

# save output
rain0.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
rain0.sum <- as.data.frame(rain0.sum[-1,])

# format names
names(rain0.sum)[which(names(rain0.sum) == "rain0.sum[-1, ]")] <- "No Rain"

# separate strings from numerical values
rain0.sum <- separate(rain0.sum, `No Rain`, into = c("stat", "No Rain"), sep = ":", extra = "merge")

# transpose
rownames(rain0.sum) <- rain0.sum$stat
rain0.sum$stat <- NULL

rain0.sum <- as.data.frame(t(rain0.sum))

# add variable for number of observations
rain0.sum$Count <- count

# add variable for share of total
rain0.sum$Share <- share

# append to output

out <- rbind(out, rain0.sum)

rm(rain0.sum)

# Low Rain -------------------------------------------------------------------------------------

Rain1 <- subset(trips, rain.bins == 1)
var <- as.data.frame(Rain1$tr.time)
count <- nrow(Rain1) 
share <- nrow(Rain1) / nrow(trips)

# remove dataset to save computing power
rm(Rain1)
gc()

# save output
rain1.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
rain1.sum <- as.data.frame(rain1.sum[-1,])

# format names
names(rain1.sum)[which(names(rain1.sum) == "rain1.sum[-1, ]")] <- "Low Rain"

# separate strings from numerical values
rain1.sum <- separate(rain1.sum, `Low Rain`, into = c("stat", "Low Rain"), sep = ":", extra = "merge")

# transpose
rownames(rain1.sum) <- rain1.sum$stat
rain1.sum$stat <- NULL

rain1.sum <- as.data.frame(t(rain1.sum))

# add variable for number of observations
rain1.sum$Count <- count

# add variable for share of total
rain1.sum$Share <- share

# append to output

out <- rbind(out, rain1.sum)

rm(rain1.sum)

# Medium Rain ----------------------------------------------------------------------------------

Rain2 <- subset(trips, rain.bins == 2)
var <- as.data.frame(Rain2$tr.time)
count <- nrow(Rain2) 
share <- nrow(Rain2) / nrow(trips)

# remove dataset to save computing power
rm(Rain2)
gc()

# save output
rain2.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
rain2.sum <- as.data.frame(rain2.sum[-1,])

# format names
names(rain2.sum)[which(names(rain2.sum) == "rain2.sum[-1, ]")] <- "Medium Rain"

# separate strings from numerical values
rain2.sum <- separate(rain2.sum, `Medium Rain`, into = c("stat", "Medium Rain"), sep = ":", extra = "merge")

# transpose
rownames(rain2.sum) <- rain2.sum$stat
rain2.sum$stat <- NULL

rain2.sum <- as.data.frame(t(rain2.sum))

# add variable for number of observations
rain2.sum$Count <- count

# add variable for share of total
rain2.sum$Share <- share

# append to output

out <- rbind(out, rain2.sum)

rm(rain2.sum)

# Heavy Rain -----------------------------------------------------------------------------------

Rain3 <- subset(trips, rain.bins == 3)
var <- as.data.frame(Rain3$tr.time)
count <- nrow(Rain3) 
share <- nrow(Rain3) / nrow(trips)

# remove dataset to save computing power
rm(Rain3)
gc()

# save output
rain3.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
rain3.sum <- as.data.frame(rain3.sum[-1,])

# format names
names(rain3.sum)[which(names(rain3.sum) == "rain3.sum[-1, ]")] <- "Heavy Rain"

# separate strings from numerical values
rain3.sum <- separate(rain3.sum, `Heavy Rain`, into = c("stat", "Heavy Rain"), sep = ":", extra = "merge")

# transpose
rownames(rain3.sum) <- rain3.sum$stat
rain3.sum$stat <- NULL

rain3.sum <- as.data.frame(t(rain3.sum))

# add variable for number of observations
rain3.sum$Count <- count

# add variable for share of total
rain3.sum$Share <- share

# append to output

out <- rbind(out, rain3.sum)

rm(rain3.sum)

# Blocks ---------------------------------------------------------------------------------------

Blocks <- subset(trips, blocks == 1)
var <- as.data.frame(Blocks$tr.time)
count <- nrow(Blocks) 
share <- nrow(Blocks) / nrow(trips)

# remove dataset to save computing power
rm(Blocks)
gc()


# save output as data frame
blocks.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
blocks.sum <- as.data.frame(blocks.sum[-1,])

# format names
names(blocks.sum)[which(names(blocks.sum) == "blocks.sum[-1, ]")] <- "Blocks"

# separate strings from numerical values
blocks.sum <- separate(blocks.sum, Blocks, into = c("stat", "Blocks"), sep = ":", extra = "merge")

# transpose
rownames(blocks.sum) <- blocks.sum$stat
blocks.sum$stat <- NULL

blocks.sum <- as.data.frame(t(blocks.sum))

# add variable for number of observations
blocks.sum$Count <- count

# add variable for share of total
blocks.sum$Share <- share

# append to output

out <- rbind(out, blocks.sum)

rm(blocks.sum)

# Floods ---------------------------------------------------------------------------------------

Floods <- subset(trips, floods == 1)
var <- as.data.frame(Floods$tr.time)
count <- nrow(Floods) 
share <- nrow(Floods) / nrow(trips)

# remove dataset to save computing power
rm(Floods)
gc()

# save output as data frame
floods.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
floods.sum <- as.data.frame(floods.sum[-1,])

# format names
names(floods.sum)[which(names(floods.sum) == "floods.sum[-1, ]")] <- "Floods"

# separate strings from numerical values
floods.sum <- separate(floods.sum, Floods, into = c("stat", "Floods"), sep = ":", extra = "merge")

# transpose
rownames(floods.sum) <- floods.sum$stat
floods.sum$stat <- NULL

floods.sum <- as.data.frame(t(floods.sum))

# add variable for number of observations
floods.sum$Count <- count

# add variable for share of total
floods.sum$Share <- share


# append to output

out <- rbind(out, floods.sum)

rm(floods.sum)

# Spillovers -----------------------------------------------------------------------------------

Spillovers <- subset(trips, spillovers == 1)
var <- as.data.frame(Spillovers$tr.time)
count <- nrow(Spillovers) 
share <- nrow(Spillovers) / nrow(trips)

# remove dataset to save computing power
rm(Spillovers)
gc()


# save output as data frame
spillovers.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
spillovers.sum <- as.data.frame(spillovers.sum[-1,])

# format names
names(spillovers.sum)[which(names(spillovers.sum) == "spillovers.sum[-1, ]")] <- "Spillovers"

# separate strings from numerical values
spillovers.sum <- separate(spillovers.sum, Spillovers, into = c("stat", "Spillovers"), sep = ":", extra = "merge")

# transpose
rownames(spillovers.sum) <- spillovers.sum$stat
spillovers.sum$stat <- NULL

spillovers.sum <- as.data.frame(t(spillovers.sum))

# add variable for number of observations
spillovers.sum$Count <- count

# add variable for share of total
spillovers.sum$Share <- share


# append to output

out <- rbind(out, spillovers.sum)

rm(spillovers.sum)


# No Blocks or floods --------------------------------------------------------------------------

NoFloods <- subset(trips, no.flood == 1)
var <- as.data.frame(NoFloods$tr.time)
count <- nrow(NoFloods) 
share <- nrow(NoFloods) / nrow(trips)

# remove dataset to save computing power
rm(NoFloods)
gc()

# save output as data frame
nofloods.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
nofloods.sum <- as.data.frame(nofloods.sum[-1,])

# format names
names(nofloods.sum)[which(names(nofloods.sum) == "nofloods.sum[-1, ]")] <- "No Floods"

# separate strings from numerical values
nofloods.sum <- separate(nofloods.sum, `No Floods`, into = c("stat", "No Floods"), sep = ":", extra = "merge")

# transpose
rownames(nofloods.sum) <- nofloods.sum$stat
nofloods.sum$stat <- NULL

nofloods.sum <- as.data.frame(t(nofloods.sum))

# add variable for number of observations
nofloods.sum$Count <- count

# add variable for share of total
nofloods.sum$Share <- share


# append to output

out <- rbind(out, nofloods.sum)

rm(nofloods.sum)


# output ---------------------------------------------------------------------------------------

saveRDS(out, "analysis/desc.rds")

print(xtable(out, 
             digits = c(2, 2, 2, 2, 2, 2, 2, 0, 2),
             type = "latex",
             caption = "Descriptive Statistics",
             floating = TRUE), 
      file = paste0(out.path,
                    "desc-stats.tex"))

