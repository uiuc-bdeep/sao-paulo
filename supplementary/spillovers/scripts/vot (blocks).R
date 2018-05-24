#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  VOT for Blocks, Floods and Spillovers                                                  |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Notes:

# Archived on 05/24/2018 
# Removed spillovers and clustered standard errors at trip level

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
# average values of a flood duration for different y bins
coef <- readRDS(coef.path)

# assign names to each coefficient
blocks.low <- coef$Estimate[[1]]
blocks.med <- coef$Estimate[[2]]
blocks.high <- coef$Estimate[[3]]

floods.low <- coef$Estimate[[4]]
floods.med <- coef$Estimate[[5]]
floods.high <- coef$Estimate[[6]]

spill.low <- coef$Estimate[[7]]
spill.med <- coef$Estimate[[8]]
spill.high <- coef$Estimate[[9]]

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

# Blocks
HH$CS1 <- 0.5 * HH$hourly.income.pwaa * (Blocks / 60)

CSD <- sum(HH$CS1 * HH$FE_PESS, na.rm = TRUE)
CSD

# Floods

HH$CS2 <- 0.5 * HH$hourly.income.pwaa * (Floods / 60)

CSD <- sum(HH$CS2 * HH$FE_PESS, na.rm = TRUE)
CSD

# Spillovers

HH$CS3 <- 0.5 * HH$hourly.income.pwaa * (Spill / 60)

CSD <- sum(HH$CS3 * HH$FE_PESS, na.rm = TRUE)
CSD

# ----------------------------------------------------------------------------------------------

# Value of Time = 0.5 mean wage

# No Low Rain, No Medium Rain, No Heavy Rain

HH$CS0 <- 0.5 * (1800 / (21 * 8)) * (HH$Y0 / 60)
HH$CS0[which(is.na(HH$CS0))] <- "0"
HH$CS0 <- as.numeric(HH$CS0)

summary(HH$CS0)

# Low Rain

HH$CS1 <- 0.5 * (1800 / (21 * 8)) * (HH$Y1 / 60)
HH$CS1 <- as.numeric(ifelse(is.na(HH$CS1), HH$CS0, HH$CS1))
summary(HH$CS1)

# Estimate change in consumer surplus
HH$CSD1 <- HH$CS0 - HH$CS1 
summary(HH$CSD1)

CSD <- sum(HH$CSD1 * HH$FE_PESS, na.rm = TRUE)
CSD


# Medium Rain

HH$CS2 <- 0.5 * (1800 / (21 * 8)) * (HH$Y2 / 60)
HH$CS2 <- as.numeric(ifelse(is.na(HH$CS2), HH$CS0, HH$CS2))
summary(HH$CS2)

# Estimate change in consumer surplus
HH$CSD2 <- HH$CS0 - HH$CS2 
summary(HH$CSD2)

CSD <- sum(HH$CSD2 * HH$FE_PESS, na.rm = TRUE)
CSD

# Heavy Rain

HH$CS3 <- 0.5 * (1800 / (21 * 8)) * (HH$Y3 / 60)
HH$CS3 <- as.numeric(ifelse(is.na(HH$CS3), HH$CS0, HH$CS3))
summary(HH$CS3)

# Estimate change in consumer surplus
HH$CSD3 <- HH$CS0 - HH$CS3 
summary(HH$CSD3)

CSD <- sum(HH$CSD3 * HH$FE_PESS, na.rm = TRUE)
CSD

# ----------------------------------------------------------------------------------------------

# Value of Time = 0.5 reported wage

# No Low Rain, No Medium Rain, No Heavy Rain

HH$CS0 <- 0.5 * (HH$VL_REN_I / (21 * 8)) * (HH$Y0 / 60)
HH$CS0[which(is.na(HH$CS0))] <- "0"
HH$CS0 <- as.numeric(HH$CS0)

summary(HH$CS0)

# Low Rain

HH$CS1 <- 0.5 * (HH$VL_REN_I / (21 * 8)) * (HH$Y1 / 60)
HH$CS1 <- as.numeric(ifelse(is.na(HH$CS1), HH$CS0, HH$CS1))
summary(HH$CS1)

# Estimate change in consumer surplus
HH$CSD1 <- HH$CS0 - HH$CS1 
summary(HH$CSD1)

CSD <- sum(HH$CSD1 * HH$FE_PESS, na.rm = TRUE)
CSD


# Medium Rain

HH$CS2 <- 0.5 * (HH$VL_REN_I / (21 * 8)) * (HH$Y2 / 60)
HH$CS2 <- as.numeric(ifelse(is.na(HH$CS2), HH$CS0, HH$CS2))
summary(HH$CS2)

# Estimate change in consumer surplus
HH$CSD2 <- HH$CS0 - HH$CS2 
summary(HH$CSD2)

CSD <- sum(HH$CSD2 * HH$FE_PESS, na.rm = TRUE)
CSD

# Heavy Rain

HH$CS3 <- 0.5 * (HH$VL_REN_I / (21 * 8))* (HH$Y3 / 60)
HH$CS3 <- as.numeric(ifelse(is.na(HH$CS3), HH$CS0, HH$CS3))
summary(HH$CS3)

# Estimate change in consumer surplus
HH$CSD3 <- HH$CS0 - HH$CS3 
summary(HH$CSD3)

CSD <- sum(HH$CSD3 * HH$FE_PESS, na.rm = TRUE)
CSD

# ----------------------------------------------------------------------------------------------

HH.wage <- HH[which(HH$VL_REN_I > 0),]

# Value of Time = 0.5 hourly income per working age adult

# No Low Rain, No Medium Rain, No Heavy Rain

HH.wage$CS0 <- 0.5 * HH.wage$hourly.income.pwaa * (HH.wage$Y0 / 60)
HH.wage$CS0[which(is.na(HH.wage$CS0))] <- "0"
HH.wage$CS0 <- as.numeric(HH.wage$CS0)

summary(HH.wage$CS0)

# Low Rain

HH.wage$CS1 <- 0.5 * HH.wage$hourly.income.pwaa * (HH.wage$Y1 / 60)
HH.wage$CS1 <- as.numeric(ifelse(is.na(HH.wage$CS1), HH.wage$CS0, HH.wage$CS1))
summary(HH.wage$CS1)

# Estimate change in consumer surplus
HH.wage$CSD1 <- HH.wage$CS0 - HH.wage$CS1 
summary(HH.wage$CSD1)

CSD <- sum(HH.wage$CSD1 * HH.wage$FE_PESS, na.rm = TRUE)
CSD


# Medium Rain

HH.wage$CS2 <- 0.5 * HH.wage$hourly.income.pwaa * (HH.wage$Y2 / 60)
HH.wage$CS2 <- as.numeric(ifelse(is.na(HH.wage$CS2), HH.wage$CS0, HH.wage$CS2))
summary(HH.wage$CS2)

# Estimate change in consumer surplus
HH.wage$CSD2 <- HH.wage$CS0 - HH.wage$CS2 
summary(HH.wage$CSD2)

CSD <- sum(HH.wage$CSD2 * HH.wage$FE_PESS, na.rm = TRUE)
CSD

# Heavy Rain

HH.wage$CS3 <- 0.5 * HH.wage$hourly.income.pwaa * (HH.wage$Y3 / 60)
HH.wage$CS3 <- as.numeric(ifelse(is.na(HH.wage$CS3), HH.wage$CS0, HH.wage$CS3))
summary(HH.wage$CS3)

# Estimate change in consumer surplus
HH.wage$CSD3 <- HH.wage$CS0 - HH.wage$CS3 
summary(HH.wage$CSD3)

CSD <- sum(HH.wage$CSD3 * HH.wage$FE_PESS, na.rm = TRUE)
CSD
