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

HH.path <- "intermediate/floods/2012-floods-trips.rds"
trips.path <- "intermediate/floods/floods-model.rds"
  
# output

out.path <- "views/floods/"
  
# survey trips ---------------------------------------------------------------------------------

HH <- readRDS(HH.path)
names(HH)

# Household.Income
HH$income_pc <- (HH$RENDA_FA/HH$NO_MORAF)/2.04

# keeping only relevant variables

HH <- HH[,c("ID_ORDEM",
            "SITUACAO", 
            "tr.time",
            "income_pc")]

# whole sample 
var <- as.data.frame(HH$tr.time)
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

# append to output
out <- sum 

# blocks ---------------------------------------------------------------------------------------

blocks <- subset(HH, SITUACAO == "intransitavel")
var <- as.data.frame(blocks$tr.time)

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

# append to output

out <- rbind(out, blocks.sum)

# floods ---------------------------------------------------------------------------------------

floods <- subset(HH, SITUACAO == "transitavel")
var <- as.data.frame(floods$tr.time)

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

# append to output

out <- rbind(out, floods.sum)

# no blocks or floods --------------------------------------------------------------------------

nofloods <- subset(HH, is.na(SITUACAO))
var <- as.data.frame(nofloods$tr.time)

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

# append to output

out <- rbind(out, nofloods.sum)

# income bins ----------------------------------------------------------------------------------


# generate income bins

HH$inc1 <- ifelse(HH$income_pc <= quantile(HH$income_pc, 0.25), 1, 0)
HH$inc2 <- ifelse(HH$income_pc > quantile(HH$income_pc, 0.25) & 
                  HH$income_pc <= quantile(HH$income_pc, 0.5), 1, 0)
HH$inc3 <- ifelse(HH$income_pc > quantile(HH$income_pc, 0.5) &
                  HH$income_pc <= quantile(HH$income_pc, 0.75), 1, 0)
HH$inc4 <- ifelse(HH$income_pc > quantile(HH$income_pc, 0.75), 1, 0)

# 1st Quartile: Income Per Capita <= 248.89

inc1 <- HH[HH$inc1 == 1,]
var <- as.data.frame(inc1$tr.time)

# save output as data frame
inc1.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
inc1.sum <- as.data.frame(inc1.sum[-1,])

# format names
names(inc1.sum)[which(names(inc1.sum) == "inc1.sum[-1, ]")] <- "1st Quartile"

# separate strings from numerical values
inc1.sum <- separate(inc1.sum, `1st Quartile`, into = c("stat", "1st Quartile"), sep = ":", extra = "merge")

# transpose
rownames(inc1.sum) <- inc1.sum$stat
inc1.sum$stat <- NULL

inc1.sum <- as.data.frame(t(inc1.sum))

# append to output

out <- rbind(out, inc1.sum)


# 2nd Quartile: Income Per Capita > 248.89 & Income Per Capita <= 412.82

inc2 <- HH[HH$inc2 == 1,]

var <- as.data.frame(inc2$tr.time)

# save output as data frame
inc2.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
inc2.sum <- as.data.frame(inc2.sum[-1,])

# format names
names(inc2.sum)[which(names(inc2.sum) == "inc2.sum[-1, ]")] <- "2nd Quartile"

# separate strings from numerical values
inc2.sum <- separate(inc2.sum, `2nd Quartile`, into = c("stat", "2nd Quartile"), sep = ":", extra = "merge")

# transpose
rownames(inc2.sum) <- inc2.sum$stat
inc2.sum$stat <- NULL

inc2.sum <- as.data.frame(t(inc2.sum))

# append to output

out <- rbind(out, inc2.sum)


# 3rd Quartile: Income Per Capita > 412.82 & Income Per Capita <= 735.29

inc3 <- HH[HH$inc3 == 1,]

var <- as.data.frame(inc3$tr.time)

# save output as data frame
inc3.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
inc3.sum <- as.data.frame(inc3.sum[-1,])

# format names
names(inc3.sum)[which(names(inc3.sum) == "inc3.sum[-1, ]")] <- "3rd Quartile"

# separate strings from numerical values
inc3.sum <- separate(inc3.sum, `3rd Quartile`, into = c("stat", "3rd Quartile"), sep = ":", extra = "merge")

# transpose
rownames(inc3.sum) <- inc3.sum$stat
inc3.sum$stat <- NULL

inc3.sum <- as.data.frame(t(inc3.sum))

# append to output

out <- rbind(out, inc3.sum)

# 4th Quartile: Income Per Capita > 735.29

inc4 <- HH[HH$inc4 == 1,]
var <- as.data.frame(inc4$tr.time)

# save output as data frame
inc4.sum <- as.data.frame(capture.output(summary(var)))

# format data frame
# remove first row
inc4.sum <- as.data.frame(inc4.sum[-1,])

# format names
names(inc4.sum)[which(names(inc4.sum) == "inc4.sum[-1, ]")] <- "4th Quartile"

# separate strings from numerical values
inc4.sum <- separate(inc4.sum, `4th Quartile`, into = c("stat", "4th Quartile"), sep = ":", extra = "merge")

# transpose
rownames(inc4.sum) <- inc4.sum$stat
inc4.sum$stat <- NULL

inc4.sum <- as.data.frame(t(inc4.sum))

# append to output

out <- rbind(out, inc4.sum)

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

trips$no.flood <- ifelse(trips$blocks == 0 & trips$floods == 0, 1, 0)

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




# output ---------------------------------------------------------------------------------------

# remove max and min

out$`Max.` <- NULL
out$`Min.` <- NULL

saveRDS(out, "analysis/desc.rds")

print(xtable(out, 
             digits = c(2, 2, 2, 2),
             type = "latex",
             caption = "Descriptive Statistics",
             floating = TRUE), 
      file = paste0(out.path,
                    "desc-stats.tex"))

