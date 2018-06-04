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

pr.blocks <- coef2$Estimate[[1]]
pr.floods <- coef2$Estimate[[2]]

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

