#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Social cost estimation for Blocks, Floods and Spillovers                               |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

rm(list=ls())
setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

# required packages
packages <- c("dplyr", "data.table")
lapply(packages, pkgTest)

# input
HH.path <- "analysis/HH.rds"
floods.path <- "stores/floods/Alag 1617 (1).csv"

coef.path <- "intermediate/floods/iv1-coef.rds"
coef2.path <- "intermediate/floods/iv2-coef.rds"

peak.path <- "intermediate/floods/iv2-coef (peak).rds" 
dir.path <- "intermediate/floods/iv2-coef(traffic).rds"

# ----------------------------------------------------------------------------------------------
# generating predicted values for blocks, floods and spillovers

# read dataset of IV 1st stage coefficients
# average values of a flood duration given accumulated rain 

coef <- readRDS(coef.path)

# assign names to each coefficient

blocks.rain <- coef$Estimate[[4]]
floods.rain <- coef$Estimate[[8]]

# read dataset of IV 2nd stage coefficients
# effect of predicted flood duration on travel time (per minute)

coef2 <- readRDS(coef2.path)

# assign names to each coefficient

coef2 <- coef2[which(coef2$model == "iv.4"),] # taking model that includes the most FEs

pr.blocks <- coef2$Estimate[[4]]
pr.floods <- coef2$Estimate[[5]]

# ----------------------------------------------------------------------------------------------

# Generate number of flood days in a year

# read files 

floods <- read.csv(floods.path, header = TRUE)
floods <- as.data.frame(floods)

# generate variable for days of the week

floods$DATA <- format(as.POSIXct(strptime(floods$DATA,"%d-%b-%y",tz="")) ,format = "%Y-%m-%d")
floods$DATA <- as.Date(floods$DATA)
floods$wd <- as.factor(as.character(weekdays(floods$DATA)))

# exclude Saturdays and Sundays

floods <- floods[which(floods$wd != "Saturday" & floods$wd != "Sunday"),]

# exclude 2017 to get estimates for the year 2016

floods <- floods[which(floods$DATA < as.Date("2017-01-01")),]

# aggregate from flood level to day level

floods <- group_by(floods, DATA, SITUACAO)
floods1 <- summarise(floods, count = n())

# generate number of days in a year with blocks, floods and spillovers 

block.days <- floods1[floods1$SITUACAO == "intransitavel",]
flood.days <- floods1[floods1$SITUACAO == "transitavel",]

block.days <- 47
flood.days <- 87

# ----------------------------------------------------------------------------------------------

# Generate added travel time per year using regression coefficients

# Blocks

Blocks <- blocks.rain * pr.blocks * block.days

# Floods

Floods <- floods.rain * pr.blocks * block.days

# Output

Blocks
Floods

# ----------------------------------------------------------------------------------------------

HH <- readRDS(HH.path)

# Value of Time = 0.5 hourly income per working age adult

# GDP of the city of Sao Paulo (Brookings Institute, 2013) 

SP.GDP <- 473000000000

# Blocks

HH$CS1 <- 0.5 * HH$hourly.income.pwaa * (Blocks / 60)

CSD <- sum(HH$CS1 * HH$FE_PESS, na.rm = TRUE)
CSD
(CSD / SP.GDP) * 100

# Floods

HH$CS2 <- 0.5 * HH$hourly.income.pwaa * (Floods / 60) 

CSD <- sum(HH$CS2 * HH$FE_PESS, na.rm = TRUE)
CSD
(CSD / SP.GDP) * 100

# ----------------------------------------------------------------------------------------------

# Social Cost with Peak Hours

peak.coef <- readRDS(peak.path)

blocks.early <- peak.coef$Estimate[[4]] * blocks.rain * block.days
blocks.late <- peak.coef$Estimate[[6]] * blocks.rain * block.days
blocks.off <- peak.coef$Estimate[[8]] * blocks.rain * block.days

floods.early <- peak.coef$Estimate[[5]] * floods.rain * flood.days
floods.late <- peak.coef$Estimate[[7]] * floods.rain * flood.days
floods.off <- peak.coef$Estimate[[9]] * floods.rain * flood.days

# Morning Peak
HH$MP <- as.numeric(ifelse(HH$dep.hour >= 7 & 
                             HH$dep.hour < 11, 1, 0))
summary(HH$MP)

# Evening Peak
HH$EP <- as.numeric(ifelse(HH$hour >= 17 &
                             HH$hour < 21, 1, 0))
summary(HH$EP)

# Off-Peak
HH$OP <- as.numeric(ifelse(HH$MP == 0 & HH$EP == 0, 1, 0))
summary(HH$OP)

# ----------------------------------------------------------------------------------------------

# Blocks
HH$CS1 <- as.numeric(ifelse(HH$MP == 1, 0.5 * HH$hourly.income.pwaa * (blocks.early / 60),
                            ifelse(HH$EP == 1, 0.5 * HH$hourly.income.pwaa * (blocks.late / 60),
                                   ifelse(HH$OP == 1, 0.5 * HH$hourly.income.pwaa * (blocks.off / 60), NA))))

CSD <- sum(HH$CS1 * HH$FE_PESS, na.rm = TRUE)
CSD

# Floods

HH$CS2 <- as.numeric(ifelse(HH$MP == 1, 0.5 * HH$hourly.income.pwaa * (floods.early / 60),
                            ifelse(HH$EP == 1, 0.5 * HH$hourly.income.pwaa * (floods.late / 60),
                                   ifelse(HH$OP == 1, 0.5 * HH$hourly.income.pwaa * (floods.off / 60), NA))))

CSD <- sum(HH$CS2 * HH$FE_PESS, na.rm = TRUE)
CSD

# ----------------------------------------------------------------------------------------------

# Social Cost with Traffic Directions 

dir.coef <- readRDS(dir.path)

blocks.with <- dir.coef$Estimate[[1]] * blocks.rain * block.days
blocks.against <- dir.coef$Estimate[[3]] * blocks.rain * block.days
blocks.off <- dir.coef$Estimate[[5]] * blocks.rain * block.days

floods.with <- dir.coef$Estimate[[2]] * floods.rain * flood.days
floods.against <- dir.coef$Estimate[[4]] * floods.rain * flood.days
floods.off <- dir.coef$Estimate[[6]] * floods.rain * flood.days

# create indicator for trips that end in the downtown area -------------------------------------
# downtown defined as 2012 Survey Traffic Zones 1, 14, 15, 16 and 23

HH$traffic_d <- as.numeric(ifelse(HH$ZONA_D == 1  |
                                    HH$ZONA_D == 14 |
                                    HH$ZONA_D == 15 |
                                    HH$ZONA_D == 16 |
                                    HH$ZONA_D == 23 , 1, 0))
summary(HH$traffic_d)

# create indicator for trips that begin in downtown area --------------------------------------

HH$traffic_o <- as.numeric(ifelse(HH$ZONA_O == 1  |
                                    HH$ZONA_O == 14 |
                                    HH$ZONA_O == 15 |
                                    HH$ZONA_O == 16 |
                                    HH$ZONA_O == 23 , 1, 0))
summary(HH$traffic_o)

# create indicator for peak hours --------------------------------------------------------------
# Morning Peak
HH$MP <- as.numeric(ifelse(HH$dep.hour >= 7 & 
                             HH$dep.hour < 11, 1, 0))
summary(HH$MP)

# Evening Peak
HH$EP <- as.numeric(ifelse(HH$hour >= 17 &
                             HH$hour < 21, 1, 0))
summary(HH$EP)

# Off-Peak
HH$OP <- as.numeric(ifelse(HH$MP == 0 & HH$EP == 0, 1, 0))
summary(HH$OP)

# create indicators for traffic direction -------------------------------------------------------

HH$with.traffic <- as.numeric(ifelse((HH$traffic_d == 1 & HH$MP == 1) |
                                     (HH$traffic_o == 1 & HH$EP == 1), 1, 0))
summary(HH$with.traffic)

HH$against.traffic <- as.numeric(ifelse(((HH$traffic_o == 1 & HH$MP == 1) |
                                        (HH$traffic_d == 1 & HH$EP == 1)) 
                                        & HH$OP == 0, 1, 0))
summary(HH$against.traffic)

# Value of Time = 0.5 hourly income per working age adult 

# Blocks
HH$CS1 <- as.numeric(ifelse(HH$with.traffic == 1, 0.5 * HH$hourly.income.pwaa * (blocks.with / 60),
                            ifelse(HH$against.traffic == 1, 0.5 * HH$hourly.income.pwaa * (blocks.against / 60),
                                   ifelse(HH$OP == 1, 0.5 * HH$hourly.income.pwaa * (blocks.off / 60), NA))))

CSD <- sum(HH$CS1 * HH$FE_PESS, na.rm = TRUE)
CSD

# Floods

HH$CS2 <- as.numeric(ifelse(HH$with.traffic == 1, 0.5 * HH$hourly.income.pwaa * (floods.with / 60),
                            ifelse(HH$against.traffic == 1, 0.5 * HH$hourly.income.pwaa * (floods.against / 60),
                                   ifelse(HH$OP == 1, 0.5 * HH$hourly.income.pwaa * (floods.off / 60), NA))))

CSD <- sum(HH$CS2 * HH$FE_PESS, na.rm = TRUE)
CSD


