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

#   prelims ------------------------------------------------------------------------------------
rm(list = ls())
gc()

setwd("~/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr","lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/date-merge.rds"
HH.path <- "analysis/HH.rds"

# output 

traffic.trips.path <- "intermediate/floods/date-merge (traffic).rds"
coef.path <- "intermediate/floods/iv1-coef (traffic).rds"
coef2.path <- "intermediate/floods/iv2-coef (traffic).rds"

trip.fe.path <- "analysis/tripfe (traffic).rds"
month.fe.path <- "analysis/monthfe (traffic).rds"
wd.fe.path <- "analysis/wdfe (traffic).rds"
hour.fe.path <- "analysis/hourfe (traffic).rds"

# read files -----------------------------------------------------------------------------------

HH <- readRDS(HH.path)
summary(HH$ZONA_D)

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

traffic.trips <- trips
traffic.trips <- merge(traffic.trips, HH, by = "ID_ORDEM", all.x = TRUE)

rm(trips, HH)
gc()

saveRDS(traffic.trips, traffic.trips.path)

# create indicator for going with flow of traffic during peak times ----------------------------

# 'morning.traffic' defined as going into downtown during morning peak hours

traffic.trips$morning.traffic <- as.numeric(ifelse(traffic.trips$early.peak == 1 & 
                                                   traffic.trips$traffic_d == 1, 1, 0))
summary(traffic.trips$morning.traffic)

# 'evening.traffic' defined as starting trip from downtown during evening peak hours

traffic.trips$evening.traffic <- as.numeric(ifelse(traffic.trips$late.peak == 1 &
                                                   traffic.trips$traffic_o == 1, 1, 0))
summary(traffic.trips$evening.traffic)

# 'with.traffic' defined as either traveling to downtown in the morning or traveling away from downtown in the evening

traffic.trips$with.traffic <- as.numeric(ifelse(traffic.trips$morning.traffic == 1 |
                                                traffic.trips$evening.traffic == 1 , 1, 0))
summary(traffic.trips$with.traffic)

# 'against.traffic' defined as traveling away from downtown in the morning or traveling to downtown in the evening

traffic.trips$against.traffic <- as.numeric(ifelse(traffic.trips$with.traffic == 0 &
                                                   (traffic.trips$early.peak == 1 |
                                                    traffic.trips$late.peak == 1  ), 1, 0))
summary(traffic.trips$against.traffic)

# 'normal.traffic' defined as all other trips not going to downtown and during off-peak hours of the day

traffic.trips$normal.traffic <- as.numeric(ifelse(traffic.trips$with.traffic == 0 & 
                                                  traffic.trips$against.traffic == 0, 1, 0))
summary(traffic.trips$normal.traffic)

saveRDS(traffic.trips, traffic.trips.path)

# first stage (blocks) -------------------------------------------------------------------------

iv.1 <- felm(delay ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = traffic.trips)

traffic.trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

rm(iv.1)
gc()

# first stage (floods) -------------------------------------------------------------------------

iv.2 <- felm(delay ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f, data = traffic.trips)

traffic.trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

rm(iv.2)
gc()

# first stage (spillovers) -------------------------------------------------------------------------

iv.3 <- felm(delay ~ spillovers:rain.bins1 + spillovers:rain.bins2 + spillovers:rain.bins3 | month + wd + hour.f, data = traffic.trips)

traffic.trips$fitted.spill <- as.numeric(fitted(iv.3))
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

rm(iv.3)
gc()

coef <- rbind(iv1.coef, iv2.coef, iv3.coef)
saveRDS(coef, coef.path)

# second stage ---------------------------------------------------------------------------------

iv.1 <- felm(tr.time ~ fitted.blocks:with.traffic + fitted.floods:with.traffic + fitted.spill:with.traffic +
                       fitted.blocks:against.traffic + fitted.floods:against.traffic + fitted.spill:against.traffic +
                       fitted.blocks:normal.traffic + fitted.floods:normal.traffic + fitted.spill:normal.traffic | ID_ORDEM, data = traffic.trips)
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

iv.2 <- felm(tr.time ~ fitted.blocks:with.traffic + fitted.floods:with.traffic + fitted.spill:with.traffic +
               fitted.blocks:against.traffic + fitted.floods:against.traffic + fitted.spill:against.traffic +
               fitted.blocks:normal.traffic + fitted.floods:normal.traffic + fitted.spill:normal.traffic | ID_ORDEM + month, data = traffic.trips)
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

iv.3 <- felm(tr.time ~ fitted.blocks:with.traffic + fitted.floods:with.traffic + fitted.spill:with.traffic +
               fitted.blocks:against.traffic + fitted.floods:against.traffic + fitted.spill:against.traffic +
               fitted.blocks:normal.traffic + fitted.floods:normal.traffic + fitted.spill:normal.traffic | ID_ORDEM + month + wd, data = traffic.trips)
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

iv.4 <- felm(tr.time ~ fitted.blocks:with.traffic + fitted.floods:with.traffic + fitted.spill:with.traffic +
               fitted.blocks:against.traffic + fitted.floods:against.traffic + fitted.spill:against.traffic +
               fitted.blocks:normal.traffic + fitted.floods:normal.traffic + fitted.spill:normal.traffic | ID_ORDEM + month + wd + hour.f, data = traffic.trips)
iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

# LaTeX output
stargazer(iv.1, iv.2, iv.3, iv.4, align = TRUE)

# save coefficients

coef2 <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef2, coef2.path)

rm(iv.1, iv.2, iv.3)
gc()

# ID_ORDEM FE ----------------------------------------------------------------------------------

# get trip FEs
iv4.fe <- getfe(iv.4)
iv4.trip <- iv4.fe[which(iv4.fe$fe == "ID_ORDEM"),]
iv4.trip <- iv4.trip[, c("effect", "idx")]
names(iv4.trip)[which(names(iv4.trip) == "effect")] <- "trip.fe"

saveRDS(iv4.trip, trip.fe.path)

rm(iv.4)
gc()
# month fixed effects --------------------------------------------------------------------------

# get month FEs
iv4.month <- iv4.fe[which(iv4.fe$fe == "month"),]
iv4.month <- iv4.month[, c("effect", "idx")]
names(iv4.month)[which(names(iv4.month) == "effect")] <- "month.fe"

saveRDS(iv4.month, month.fe.path)

# day of week fixed effects --------------------------------------------------------------------

# get day of week FEs
iv4.wd <- iv4.fe[which(iv4.fe$fe == "wd"),]
iv4.wd <- iv4.wd[, c("effect", "idx")]
names(iv4.wd)[which(names(iv4.wd) == "effect")] <- "wd.fe"

saveRDS(iv4.wd, wd.fe.path)

# time of day fixed effects ----------------------------------------------

iv4.hour <- iv4.fe[which(iv4.fe$fe == "hour.f"),]
iv4.hour <- iv4.hour[, c("effect", "idx")]
names(iv4.hour)[which(names(iv4.hour) == "effect")] <- "hour.fe"

saveRDS(iv4.hour, hour.fe.path)

