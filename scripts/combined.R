
rm(list = ls())

setwd("//141.142.208.117/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

trips.path <- "intermediate/floods/floods-model.rds"

trips <- readRDS(trips.path)

# combining blocks and floods

# reduced form

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(tr.time ~ mean
                    + rain.bins1 + rain.bins2 + rain.bins3| ID_ORDEM | 0 | ID_ORDEM, data = trips)


# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(tr.time ~ mean
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)


# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(tr.time ~ mean 
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)


# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(tr.time ~ mean 
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


stargazer(m1, m2, m3, m4,
          type = "latex",
          df = FALSE,
          dep.var.labels = c("Trip Duration"),
          covariate.labels = c("Floods Duration",
                               "Light Rain",
                               "Moderate Rain",
                               "Heavy Rain"),
          notes = "Standard errors clustered at trip level.",
          add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                           c("Month FE", "N", "Y", "Y", "Y"),
                           c("Day of Week FE", "N", "N", "Y", "Y"),
                           c("Hour FE", "N", "N", "N", "N")))
                           
                           
m5 <- felm(tr.time ~ mean:early.peak +
                     mean:late.peak + 
                     mean:not.peak +
                     rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


stargazer(m5,
          type = "latex",
          df = FALSE,
          dep.var.labels = c("Trip Duration"),
          notes = "Standard errors clustered at trip level.",
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")))
                           
# IV  - first stage ----------------------------------------------------------------------------

trips$combined <- ifelse(trips$blocks == 1 | trips$floods == 1, 1, 0)
trips$combined[is.na(trips$combined)] <- 0

iv <- felm(mean ~ combined:acc.rain + rain.bins1 + rain.bins2 + rain.bins3 
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.combined <- fitted(iv)

firststage <- as.data.frame(summary(iv)$coefficients)

stargazer(iv,
          type = "latex",
          df = FALSE,
          dep.var.labels = c("Floods Duration"),
          add.lines = list(c("Trip FE", "N"),
                           c("Month FE",  "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV First Stage")
          
#   IV second stage ----------------------------------------------------------------------------

# trip FE

iv.1 <- felm(tr.time ~ fitted.combined + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM | 0 | ID_ORDEM, data = trips)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

# trip + month FE

iv.2 <- felm(tr.time ~ fitted.combined + rain.bins2 + rain.bins3
                       | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# trip + month + day of week FE

iv.3 <- felm(tr.time ~ fitted.combined + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)

iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ fitted.combined + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

#   output -------------------------------------------------------------------------------------

# save coefficients

secondstage <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)

stargazer(iv.1, iv.2, iv.3, iv.4, 
           type = "latex",
           df = FALSE,
           title = "IV Second Stage",
           covariate.labels = c("pr.Floods Duration",
                                 "Light Rain",
                                 "Moderate Rain",
                                 "Heavy Rain"),
           add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                            c("Month FE", "N", "Y", "Y", "Y"),
                            c("Day of Week FE", "N", "N", "Y", "Y"),
                            c("Hour FE", "N", "N", "N", "Y")),
           notes = "Standard errors are clustered at the trip level.",
          dep.var.labels = "Trip Duration (minutes)")
          
# second stage with peak hour indicators -------------------------------------------------------

iv <- felm(tr.time ~ fitted.combined:early.peak +
                     fitted.combined:late.peak +
                     fitted.combined:not.peak +
                     rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

peak.coef <- as.data.frame(summary(iv)$coefficients)

stargazer(iv,
          type = "latex",
          dep.var.labels = c("Trip Duration"),
          df = FALSE,
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV Second Stage with Peak Hour Indicators")
          
# second stage with traffic direction indicators -------------------------------------------------------

iv <- felm(tr.time ~ fitted.combined:with.traffic +
                       fitted.combined:against.traffic +
                       fitted.combined:normal.traffic 
                       | ID_ORDEM + month + wd + hour.f, data = trips)
                       
dir.coef <- as.data.frame(summary(iv)$coefficients)

# LaTeX output
stargazer(iv,
          type = "latex",
          dep.var.labels = c("Trip Duration"),
          df = FALSE,
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV Second Stage with Traffic Directions")

# ----------------------------------------------------------------------------------------------

# Social Cost (Main Results)

HH.path <- "analysis/HH.rds"
HH <- readRDS(HH.path)

# IV First Stage Coefficient: Average Flood Duration 

floods.rain <- firststage$Estimate[[4]]

# IV Second Stage Coefficient: Effect of Floods on Travel Time

secondstage <- secondstage[secondstage$model == "iv.4",]
pr.floods <- secondstage$Estimate[[1]]

# Number of days in year 2016 where there are flood events

flood.days <- 135

# Average Travel Time Added Per Trip Per Year

Floods <- floods.rain * pr.floods * flood.days

# Estimate Social Cost

HH$CS <- 0.5 * HH$hourly.income.pwaa * (Floods / 60) 

CSD <- sum(HH$CS * HH$FE_PESS, na.rm = TRUE)
CSD
(CSD / SP.GDP) * 100

# ----------------------------------------------------------------------------------------------

# Social Cost with Peak Hours

# IV First Stage Coefficient: Average Flood Duration 

floods.rain <- firststage$Estimate[[4]]

# IV Second Stage Coefficient: Effect of Floods on Travel Time

early <- peak.coef$Estimate[[4]] 
late <- peak.coef$Estimate[[5]] 
off <- peak.coef$Estimate[[6]]

# Number of days in year 2016 where there are flood events

flood.days <- 135

# Average Travel Time Added Per Trip Per Year

floods.early <- early * floods.rain * flood.days
floods.late <- late * floods.rain * flood.days
floods.off <- off * floods.rain * flood.days

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

# Estimate Social Cost

HH$CS <- as.numeric(ifelse(HH$MP == 1, 0.5 * HH$hourly.income.pwaa * (floods.early / 60),
                            ifelse(HH$EP == 1, 0.5 * HH$hourly.income.pwaa * (floods.late / 60),
                                   ifelse(HH$OP == 1, 0.5 * HH$hourly.income.pwaa * (floods.off / 60), NA))))

CSD <- sum(HH$CS * HH$FE_PESS, na.rm = TRUE)
CSD

# ----------------------------------------------------------------------------------------------

# Social Cost with Traffic Direction 

# IV First Stage Coefficient: Average Flood Duration 

floods.rain <- firststage$Estimate[[4]]

# IV Second Stage Coefficient: Effect of Floods on Travel Time

with <- dir.coef$Estimate[[1]]
against <- dir.coef$Estimate[[2]]
normal <- dir.coef$Estimate[[3]]

# Number of days in year 2016 where there are flood events

flood.days <- 135

# Average Travel Time Added Per Trip Per Year

floods.with <- with * floods.rain * flood.days
floods.against <- against * floods.rain * flood.days
floods.normal <- normal * floods.rain * flood.days

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

# create indicators for traffic direction -------------------------------------------------------

HH$with.traffic <- as.numeric(ifelse((HH$traffic_d == 1 & HH$MP == 1) |
                                     (HH$traffic_o == 1 & HH$EP == 1), 1, 0))
summary(HH$with.traffic)

HH$against.traffic <- as.numeric(ifelse(((HH$traffic_o == 1 & HH$MP == 1) |
                                        (HH$traffic_d == 1 & HH$EP == 1)) 
                                        & HH$OP == 0, 1, 0))
summary(HH$against.traffic)

# Estimate Social Cost

HH$CS <- as.numeric(ifelse(HH$with.traffic == 1, 0.5 * HH$hourly.income.pwaa * (floods.with / 60),
                            ifelse(HH$against.traffic == 1, 0.5 * HH$hourly.income.pwaa * (floods.against / 60),
                                   ifelse(HH$OP == 1, 0.5 * HH$hourly.income.pwaa * (floods.normal / 60), NA))))

CSD <- sum(HH$CS * HH$FE_PESS, na.rm = TRUE)
CSD

