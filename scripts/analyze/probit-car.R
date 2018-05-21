
# clear memory

rm(list=ls())

# set working directory

setwd("//141.142.208.117/share/projects/Congestion/floods/")

# required packages

library(mlogit) # package for multinomial logit regressions
library(stargazer) # package for generating tables in LaTeX 
library(dplyr) # package for manipulating data
library(caret) # package for generating dummy variables in bulk
library(lubridate) # package for manipulating dates
library(lfe) # package for running fixed effects models

# input
# data from 2007 & 2012 survey

HD.path <- "data/combined.rds"

# output
# table with regression results

out.path <- "views/"

# --------------------------------------------------------------------------------------------------------- #

# Combined 2007 & 2012 Survey

HD <- readRDS(HD.path)

# convert NAs to 0s 

HD$rain[is.na(HD$rain)] <- 0
HD$blocks_count[is.na(HD$blocks_count)] <- 0
HD$floods_count[is.na(HD$floods_count)] <- 0

# date of travel

HD$DATA[HD$DATA < as.Date("2007-01-01")] <- NA
HD$DATA1 <- as.Date(ymd(HD$DATA) - days(1))

# year FE

HD$year <- year(HD$DATA1)

# month FE

HD$month <- month(HD$DATA1)

# count trips per individual

# TIPOVG = 1 (public)
# TIPOVG = 2 (car)
# TIPOVG = 3 (walk)
# TIPOVG = 4 (bike)

HD$trip <- ifelse(HD$TIPOVG %in% c(1,2,3,4), 1, 0)
HD$trips_individual <- ave(HD$trip, HD$ID_PESS, FUN = sum)
HD$travels <- ifelse(HD$trips_individual > 0, 1, 0)

# public transport trips

HD$public <- ifelse(HD$TIPOVG == 1, 1, 0)
HD$public[is.na(HD$public)] <- 0

# car trips

HD$car <- ifelse(HD$TIPOVG == 2, 1, 0)
HD$car[is.na(HD$car)] <- 0

# generate rain bins

HD$rain.bins <- ifelse(HD$rain == 0, "0",
                          ifelse(HD$rain <= 2.5, "1",
                                 ifelse(HD$rain <= 7.6, "2",
                                        ifelse(HD$rain > 7.6, "3", 0))))

HD$rain.dummy <- as.numeric(ifelse(HD$rain.bins > 0, 1, 0))
HD$rain.dummy[which(is.na(HD$rain.dummy))] <- 0

HD$rain.bins1 <- as.numeric(ifelse(HD$rain.bins == "1", 1, 0))
HD$rain.bins1[which(is.na(HD$rain.bins1))] <- 0

HD$rain.bins2 <- as.numeric(ifelse(HD$rain.bins == "2", 1, 0))
HD$rain.bins2[which(is.na(HD$rain.bins2))] <- 0

HD$rain.bins3 <- as.numeric(ifelse(HD$rain.bins == "3", 1, 0))
HD$rain.bins3[which(is.na(HD$rain.bins3))] <- 0

# create dummies for morning and evening peak hours 

HD$morning.peak <- ifelse(HD$H_SAIDA >= 7 & HD$H_SAIDA <= 10, 1, 0)
HD$morning.peak[which(is.na(HD$morning.peak))] <- 0

HD$evening.peak <- ifelse(HD$H_SAIDA >= 17 & HD$H_SAIDA <= 20, 1, 0)
HD$evening.peak[which(is.na(HD$evening.peak))] <- 0

# Gender (Female)

HD$female <- ifelse(HD$SEXO == 2, 1, 0)

# Number of vehicles per household

HD$vehicles <- HD$QT_AUTO + HD$QT_MOTO

# Household Income Per Capita

HD$HH.IpC <- (HD$RENDA_FA/HD$NO_MORAF)/2.04


# subset to individuals (ID = Individual Data)

ID <- HD[!duplicated(HD$ID_PESS),]

# create dummy to indicate if it rained in the date of the interview

ID$rain[is.na(ID$rain)] <- 0
ID$rain_day_tot <- ave(ID$rain, ID$DATA1, FUN=sum)
ID$rain_day_D <- ifelse(ID$rain_day_tot > 0, 1, 0)

# create dummy for rain bins on date of interview

ID$rain1_day_tot <- ave(ID$rain.bins1, ID$DATA1, FUN = sum)
ID$rain1_day_D <- ifelse(ID$rain1_day_tot > 0, 1, 0)

ID$rain2_day_tot <- ave(ID$rain.bins2, ID$DATA1, FUN = sum)
ID$rain2_day_D <- ifelse(ID$rain2_day_tot > 0, 1, 0)

ID$rain3_day_tot <- ave(ID$rain.bins3, ID$DATA1, FUN = sum)
ID$rain3_day_D <- ifelse(ID$rain3_day_tot > 0, 1, 0)

# individual variables

ID$works <- ifelse(ID$CD_ATIVI %in% c(1,2), 1, 0)
ID$student <- ifelse(ID$CD_ATIVI %in% c(8), 1, 0)

# Create discrete choice Data Frame ------------------------------------------------------------

# Dependent Variable ____________
# Choice = mode used for the trip
#    car = Private vehicle modes
#    pub = Public modes
#    walk = Walking



# Alternative Specific __________
# Time = Travel Time (Average Travel Time by mode based on Google Crawled trips)
#    Time.car = travel time by private modes
#    Time.pub = travel time by public modes
#    Time.walk = travel time by walking

# Cost = Travel Monetary Cost
#    Cost.car = Cost by private modes
#               Estimated as using the following Parameters:
                # fuelEfficiency <- 10.4                        # http://www.inmetro.gov.br/consumidor/pbe/veiculos_leves.pdf 
                # PriceGas <- 2.651                             # Gasoline price per liter - http://www.anp.gov.br/preco/prc/Resumo_Mensal_Index.asp
                # CarKmCost <- PriceGas/fuelEfficiency          # cost per kilometer for cars
                # ParkingCost <- 17.22                          # Baseline Parking Cost (Only for downtown trips)
#    Cost.pub = Fare of public modes (Extracted from google crawler)
#    Cost.walk = cost of walking (Zero)



# Other covariates _____________
# rain = milimeters of rain at the moment of the trip departure time
# HH.IpC = Household Income per Capita (RENDA_FA/NO_MORAF = Householc Income/ NUmber of Family Members)
# IDADE = Individual age
# female = Dummy for women
# vehicles = number of vehicles owned by the household


# subset to discrete choice variables (DCV)

DCV <- c("travels",
         "car",
         "blocks_count",
         "floods_count",
         "rain_day_D",
         "rain1_day_D",
         "rain2_day_D",
         "rain3_day_D",
         "morning.peak",
         "evening.peak",
         "year",
         "month",
         "SUB")

ID_DCV <- ID[DCV]

# exclude variables with NAS (Problematic for creating Long Format, 2177 observations excluded)

for( i in 1:length(DCV) ){
  ID_DCV <- ID_DCV[!is.na(ID_DCV[ ,DCV[i] ] ), ]
}

# create dataset in a long format (LD = Long Discrete Choice Data)

LD <- mlogit.data(ID_DCV, shape = "wide", choice = "travels")

# Discrete Choice Estimation (with car trip dummy interaction) -------------------------------------------------------

LD$year <- as.factor(LD$year)
LD$month <- as.factor(LD$month)
LD$SUB <- as.factor(LD$SUB)

# Rain Day + Number of Blocks / Floods 

f1 <- mFormula(travels ~ 0 | rain_day_D +  car:blocks_count + car:floods_count + year + month + SUB)
m1 <- mlogit(f1, LD)
summary(m1)

f2 <- mFormula(travels ~ 0 | rain_day_D +  car:blocks_count + car:floods_count 
                              + year + SUB + morning.peak + evening.peak)
m2 <- mlogit(f2, LD)


# Rain Bins Day + Number of Blocks / Floods 

f3 <- mFormula(travels ~ 0 | rain1_day_D + rain2_day_D + rain3_day_D + car:blocks_count 
                             + car:floods_count + year + month + SUB)
m3 <- mlogit(f3, LD)

f4 <- mFormula(travels ~ 0 | rain1_day_D + rain2_day_D + rain3_day_D + car:blocks_count 
                             + car:floods_count + year + SUB + morning.peak + evening.peak)
m4 <- mlogit(f4, LD)


# Output tables -------------------------------------------------------------------

stargazer(m1, m2, m3, m4,
          type = "latex",
          dep.var.labels = "Prob. of Taking a Trip",
          add.lines = list(c("Year FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "Yes", "No", "Yes", "No"),
                           c("District FE", "Yes", "Yes", "Yes", "Yes")),
          df = FALSE,
          out = paste0(out.path, "combined.tex"))
