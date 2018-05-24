#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run delay on spillovers, trips and spillovers with peak hour effects                   |
#   |  Run travel time on delay from blocks, floods and spillovers                            |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Notes: 

# Archived on 05/24/2018 to remove spillovers and cluster standard errors at trip level

rm(list=ls())
setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/date-merge.rds"

#   output

coef.path <- "intermediate/floods/iv1-coef (peak).rds"
coef2.path <- "intermediate/floods/iv2-coef (peak).rds"

blocks.out <- "intermediate/floods/prblocks (peak).rds"
floods.out <- "intermediate/floods/prfloods (peak).rds"
spillovers.out <- "intermediate/floods/prspillovers (peak).rds"

peak.trips.path <- "intermediate/floods/date-merge (peak).rds"

# for subset of flood events where SENTIDO == AMBOS --------------------------------------------

blocks1.out <- "intermediate/floods/prblocks1.rds"
floods1.out <- "intermediate/floods/prfloods1.rds"
spillovers1.out <- "intermediate/floods/prspillovers1.rds"

# read files -----------------------------------------------------------------------------------

trips <- readRDS(trips.path)

names(trips)
summary(trips$delay)

# creating indicator for peak hours ------------------------------------------------------------

trips$early.peak <- as.numeric(ifelse(trips$hour >= 7 & 
                                        trips$hour < 11, 1, 0))
summary(trips$early.peak)


trips$late.peak <- as.numeric(ifelse(trips$hour >= 17 &
                                       trips$hour < 21, 1, 0))
summary(trips$late.peak)

trips$off.peak <- as.numeric(ifelse(trips$early.peak == 0 & trips$late.peak == 0, 1, 0))
summary(trips$off.peak)


# first stage (blocks) -------------------------------------------------------------------------

iv.1 <- felm(delay ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

rm(iv.1)
gc()

# first stage (floods) -------------------------------------------------------------------------

iv.2 <- felm(delay ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f, data = trips)

trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

rm(iv.2)
gc()

# first stage (spillovers) -------------------------------------------------------------------------

iv.3 <- felm(delay ~ spillovers:rain.bins1 + spillovers:rain.bins2 + spillovers:rain.bins3 | month + wd + hour.f, data = trips)

trips$fitted.spill <- as.numeric(fitted(iv.3))
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

rm(iv.3)
gc()

coef <- rbind(iv1.coef, iv2.coef, iv3.coef)
saveRDS(coef, coef.path)

# second stage ---------------------------------------------------------------------------------

iv.1 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM , data = trips)
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

iv.2 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM + month, data = trips)
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

iv.3 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM + month + wd , data = trips)
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

iv.4 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM + month + wd + hour.f , data = trips)
iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

# save coefficients

coef2 <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef2, coef2.path)

# LaTeX output
stargazer(iv.1, iv.2, iv.3, iv.4, align = TRUE)

rm(iv.1, iv.2, iv.3)
gc()

# generating predicted values for welfare analysis ---------------------------------------------

# ID_ORDEM FE ----------------------------------------------------------------------------------
trip.fe.path <- "analysis/tripfe (peak).rds"

# get trip FEs
iv4.fe <- getfe(iv.4)
iv4.trip <- iv4.fe[which(iv4.fe$fe == "ID_ORDEM"),]
iv4.trip <- iv4.trip[, c("effect", "idx")]
names(iv4.trip)[which(names(iv4.trip) == "effect")] <- "trip.fe"

saveRDS(iv4.trip, trip.fe.path)

rm(iv.4)
gc()
# month fixed effects --------------------------------------------------------------------------
month.fe.path <- "analysis/monthfe (peak).rds"

# get month FEs
iv4.month <- iv4.fe[which(iv4.fe$fe == "month"),]
iv4.month <- iv4.month[, c("effect", "idx")]
names(iv4.month)[which(names(iv4.month) == "effect")] <- "month.fe"

saveRDS(iv4.month, month.fe.path)

# day of week fixed effects --------------------------------------------------------------------
wd.fe.path <- "analysis/wdfe (peak).rds"

# get day of week FEs
iv4.wd <- iv4.fe[which(iv4.fe$fe == "wd"),]
iv4.wd <- iv4.wd[, c("effect", "idx")]
names(iv4.wd)[which(names(iv4.wd) == "effect")] <- "wd.fe"

saveRDS(iv4.wd, wd.fe.path)

# time of day fixed effects ----------------------------------------------
hour.fe.path <- "analysis/hourfe (peak).rds"

iv4.hour <- iv4.fe[which(iv4.fe$fe == "hour.f"),]
iv4.hour <- iv4.hour[, c("effect", "idx")]
names(iv4.hour)[which(names(iv4.hour) == "effect")] <- "hour.fe"

saveRDS(iv4.hour, hour.fe.path)

# create dataset to be used for peak hours analysis --------------------------------------------

names(trips)

peak.trips.path <- "intermediate/floods/date-merge (peak).rds"

trips[,c("trip.fe", "month.fe", "hour.fe", "wd.fe")] <- NULL
peak.trips <- trips
saveRDS(peak.trips, peak.trips.path)

rm(trips)
gc()




