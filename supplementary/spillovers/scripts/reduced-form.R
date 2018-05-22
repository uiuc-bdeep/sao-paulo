#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run reduced form model  (large dataset)                                                |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# archived on 05/22/2018

#   prelims ------------------------------------------------------------------------------------
rm(list = ls())
gc()

setwd("//141.142.208.117/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/date-merge.rds"
floods.path <- "intermediate/floods/floods.rds"

#   output

coef.path <- "intermediate/floods/coef.rds"

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

summary(trips$blocks)
summary(trips$floods)
summary(trips$spillovers)

summary(trips$blocks1)
summary(trips$floods1)
summary(trips$spillovers1)

# reduced form models ---------------------------------------------------------------------------------

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM, data = trips)
m1.coef <- as.data.frame(summary(m1)$coefficients)
m1.coef$model <- "m1"
rm(m1)

# run model for subset of flood events where SENTIDO == AMBOS
m1 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM, data = trips)

# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month, data = trips)
m2.coef <- as.data.frame(summary(m2)$coefficients)
m2.coef$model <- "m2"
rm(m2)

# run model for subset of flood events where SENTIDO == AMBOS
m2 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM + month, data = trips)

# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd, data = trips)
m3.coef <- as.data.frame(summary(m3)$coefficients)
m3.coef$model <- "m3"
rm(m3)

# run model for subset of flood events where SENTIDO == AMBOS
m3 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM + month + wd, data = trips)


# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd + hour.f, data = trips)
m4.coef <- as.data.frame(summary(m4)$coefficients)
m4.coef$model <- "m4"
rm(m4)

# run model for subset of flood events where SENTIDO == AMBOS
m4 <- felm(ln_tr.time ~ blocks1 + floods1 + spillovers1 | ID_ORDEM + month + wd + hour.f, data = trips)

# output ---------------------------------------------------------------------------------------
stargazer(m1, m2, m3, m4, align = TRUE)
rm(m1, m2, m3, m4)
gc()

# interact flood variables with time of day factors --------------------------------------------

m5 <- felm(ln_tr.time ~ blocks:hour.f + floods:hour.f + spillovers:hour.f| ID_ORDEM + month + wd + hour.f, data = trips)
m5.coef <- as.data.frame(summary(m5)$coefficients)
m5.coef$model <- "m5"
rm(m5)

# peak hour ------------------------------------------------------------------------------------

# creating indicator variables for peak travel times
# Sao Paulo peak hours: 7 - 10 am and 5 - 8 pm as determined by driving restrictions
trips$hour <- as.numeric(as.character(trips$hour.f))

trips$early.peak <- as.numeric(ifelse(trips$hour >= 7 & 
                                      trips$hour < 11, 1, 0))
summary(trips$early.peak)


trips$late.peak <- as.numeric(ifelse(trips$hour >= 17 &
                                     trips$hour < 21, 1, 0))
summary(trips$late.peak)

trips$off.peak <- as.numeric(ifelse(trips$early.peak == 0 & trips$late.peak == 0, 1, 0))
summary(trips$off.peak)

saveRDS(trips, trips.path)

# reduced form model with peak hour interactions -----------------------------------------------
m5 <- felm(ln_tr.time ~ blocks:early.peak + floods:early.peak + spillovers:early.peak +
                        blocks:late.peak + floods:late.peak + spillovers:late.peak +
                        blocks:not.peak + floods:not.peak + spillovers:not.peak | ID_ORDEM + month + wd + hour.f, data = trips)

# run model for subset of flood events where SENTIDO == AMBOS
m5 <- felm(ln_tr.time ~ blocks1:early.peak + floods1:early.peak + spillovers1:early.peak +
             blocks1:late.peak + floods1:late.peak + spillovers1:late.peak +
             blocks1:not.peak + floods1:not.peak + spillovers1:not.peak | ID_ORDEM + month + wd + hour.f, data = trips)

# output ---------------------------------------------------------------------------------------
stargazer(m5, align = TRUE)

rm(m5)
gc()

# output ---------------------------------------------------------------------------------------

coef <- rbind(m1.coef, m2.coef, m3.coef, m4.coef, m5.coef)
saveRDS(coef, coef.path)

rm(list=ls())
gc()


