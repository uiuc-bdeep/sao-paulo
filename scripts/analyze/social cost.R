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
floods.path <- "intermediate/floods/floods.rds"

coef.path <- "intermediate/floods/iv1-coef.rds"
coef2.path <- "intermediate/floods/iv2-coef.rds"

# ----------------------------------------------------------------------------------------------
# generating predicted values for blocks, floods and spillovers

# read dataset of IV 1st stage coefficients
# average values of a flood duration for different rain bins

coef <- readRDS(coef.path)

# assign names to each coefficient

blocks.rain <- coef$Estimate[[1]]
blocks.low <- coef$Estimate[[2]]
blocks.med <- coef$Estimate[[3]]
blocks.high <- coef$Estimate[[4]]

floods.rain <- coef$Estimate[[5]]
floods.low <- coef$Estimate[[6]]
floods.med <- coef$Estimate[[7]]
floods.high <- coef$Estimate[[8]]

spill.rain <- coef$Estimate[[9]]
spill.low <- coef$Estimate[[10]]
spill.med <- coef$Estimate[[11]]
spill.high <- coef$Estimate[[12]]

# read dataset of IV 2nd stage coefficients
coef2 <- readRDS(coef2.path)

# assign names to each coefficient
coef2 <- coef2[which(coef2$model == "iv.4"),] # taking model that includes the most FEs

pr.blocks <- coef2$Estimate[[1]]
pr.floods <- coef2$Estimate[[2]]
pr.spill <- coef2$Estimate[[3]]


# ----------------------------------------------------------------------------------------------

# Generate number of flood days in a year

# read files 

floods <- readRDS(floods.path)

# generate variable for days of the week

floods$wd <- as.factor(as.character(weekdays(floods$DATA)))

# exclude Saturdays and Sundays

floods <- floods[which(floods$wd != "Saturday" & floods$wd != "Sunday"),]

# exclude 2017

floods <- floods[which(floods$DATA < as.Date("2017-01-01")),]

# generate indicators for blocks and floods

floods$blocks <- ifelse(floods$SITUACAO == "intransitavel", 1, 0)
floods$floods <- ifelse(floods$SITUACAO == "transitavel", 1, 0)

# aggregate from flood level to day level

floods <- group_by(floods, DATA)

floods1 <- summarize(floods, blocks = max(blocks), floods = max(floods))

# generate number of days in a year with blocks, floods and spillovers 

block.days <- sum(floods1$blocks, na.rm = TRUE)
flood.days <- sum(floods1$floods, na.rm = TRUE)
spill.days <- block.days + flood.days 


# ----------------------------------------------------------------------------------------------

# Generate added travel time using regression coefficients

# Blocks

LR.Blocks <- blocks.low * pr.blocks * block.days
MR.Blocks <- blocks.med * pr.blocks * block.days
HR.Blocks <- blocks.high * pr.blocks * block.days

Blocks <- LR.Blocks + MR.Blocks + HR.Blocks

# Floods

LR.Floods <- floods.low * pr.floods * flood.days
MR.Floods <- floods.med * pr.floods * flood.days
HR.Floods <- floods.high * pr.floods * flood.days

Floods <- LR.Floods + MR.Floods + HR.Floods

# Spillovers

LR.Spill <- spill.low * pr.spill * spill.days
MR.Spill <- spill.med * pr.spill * spill.days
HR.Spill <- spill.high * pr.spill * spill.days

Spill <- LR.Spill + MR.Spill + HR.Spill

# Output

Blocks
Floods
Spill

# read files

HH <- readRDS(HH.path)

# ----------------------------------------------------------------------------------------------

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

# Spillovers

HH$CS3 <- 0.5 * HH$hourly.income.pwaa * (Spill / 60) 

CSD <- sum(HH$CS3 * HH$FE_PESS, na.rm = TRUE)
CSD
(CSD / SP.GDP) * 100

