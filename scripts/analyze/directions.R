#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run delay on spillovers, trips and spillovers                                          |
#   |  Run travel time on delay from blocks, floods and spillovers                            |
#   |  with traffic direction heterogeneity                                                   |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------


rm(list = ls())
gc()

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr","lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"
HH.path <- "analysis/HH.rds"

# output 

coef.path <- "intermediate/floods/iv2-coef(traffic).rds"

# read files -----------------------------------------------------------------------------------

HH <- readRDS(HH.path)
trips <- readRDS(trips.path)

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

# merge indicator with trips dataset -----------------------------------------------------------

HH <- HH[,c("ID_ORDEM", "traffic_d", "traffic_o")]
trips <- merge(trips, HH, by = "ID_ORDEM", all.x = TRUE)

# create indicator for going with flow of traffic during peak times ----------------------------

# 'morning.traffic' defined as going into downtown during morning peak hours

trips$morning.traffic <- as.numeric(ifelse(trips$early.peak == 1 & 
                                           trips$traffic_d == 1, 1, 0))
summary(trips$morning.traffic)

# 'evening.traffic' defined as starting trip from downtown during evening peak hours

trips$evening.traffic <- as.numeric(ifelse(trips$late.peak == 1 &
                                           trips$traffic_o == 1, 1, 0))
summary(trips$evening.traffic)

# 'with.traffic' defined as either traveling to downtown in the morning or traveling away from downtown in the evening

trips$with.traffic <- as.numeric(ifelse(trips$morning.traffic == 1 |
                                        trips$evening.traffic == 1 , 1, 0))
summary(trips$with.traffic)

# 'against.traffic' defined as traveling away from downtown in the morning or traveling to downtown in the evening

trips$against.traffic <- as.numeric(ifelse(trips$with.traffic == 0 &
                                                   (trips$early.peak == 1 |
                                                    trips$late.peak == 1  ), 1, 0))
summary(trips$against.traffic)

# 'normal.traffic' defined as all other trips not going to downtown and during off-peak hours of the day

trips$normal.traffic <- as.numeric(ifelse(trips$with.traffic == 0 & 
                                          trips$against.traffic == 0, 1, 0))
summary(trips$normal.traffic)

saveRDS(trips, trips.path)

# second stage ---------------------------------------------------------------------------------

iv <- felm(tr.time ~ blocks:fitted.blocks:with.traffic + floods:fitted.floods:with.traffic +
                     blocks:fitted.blocks:against.traffic + floods:fitted.floods:against.traffic +
                     blocks:fitted.blocks:normal.traffic + floods:fitted.floods:normal.traffic +
                     rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd + hour.f, data = trips)

# save coefficients

iv.coef <- as.data.frame(summary(iv)$coefficients)
saveRDS(iv.coef, coef.path)

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
          title = "IV Second Stage with Traffic Directions",
          out = paste0(out.path, "iv(directions).tex"))

