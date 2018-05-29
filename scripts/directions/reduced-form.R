#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run reduced form model for trips where both directions of traffic are blocks           |
#   |  (coded as SENTIDOS = AMBOS in floods data)                                             |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# archived on 05/22/2018

rm(list = ls())
gc()

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"
floods.path <- "intermediate/floods/floods.rds"

#   output

out.path <- "views/floods/directions/"

#   read files

trips <- readRDS(trips.path)
floods <- readRDS(floods.path)

# merge SENTIDO

trips <- merge(trips, floods[,c("FID", "SENTIDO")], by = "FID", all.x = TRUE)
rm(floods)

summary(trips$SENTIDO)

# creating indicator variables for flood events where both directions of traffic are blocked
trips$blocks1 <- as.numeric(ifelse(trips$blocks == 1 & 
                                   trips$SENTIDO == "AMBOS", 1, 0))

trips$floods1 <- as.numeric(ifelse(trips$floods == 1 & 
                                   trips$SENTIDO == "AMBOS", 1, 0))

trips$spillovers1 <- as.numeric(ifelse(trips$spillovers == 1 & 
                                       trips$SENTIDO == "AMBOS", 1, 0))

saveRDS(trips, trips.path)

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM | 0 | ID_ORDEM, data = trips)

# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM + month| 0 | ID_ORDEM, data = trips)

# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)


# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM + month + wd + hour.f, data = trips)

# output ---------------------------------------------------------------------------------------


# reduced form model with peak hour interactions -----------------------------------------------
# creating indicator variables for peak travel times
# Sao Paulo peak hours: 7 - 10 am and 5 - 8 pm as determined by driving restrictions

m5 <- felm(ln_tr.time ~ blocks1:early.peak + floods1:early.peak + spillovers1:early.peak +
             blocks1:late.peak + floods1:late.peak + spillovers1:late.peak +
             blocks1:not.peak + floods1:not.peak + spillovers1:not.peak | ID_ORDEM + month + wd + hour.f, data = trips)

