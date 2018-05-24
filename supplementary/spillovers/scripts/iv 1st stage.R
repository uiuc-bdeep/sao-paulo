#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  IV first stage: Flood Duration against Rain Bins                                       |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#   |                                                                                         |
#   |  Edited on 02/22/2018 to include accumulated rainfall                                   |
#   |  Edited on 03/01/2018 to use dataset with one observation per crawled trip              |
#     ----------------------------------------------------------------------------------------

# Notes:

# Archived on 05/24/2018
# Removed spillovers and clustered standard errors at the trip level

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

#   output

coef.path <- "intermediate/floods/iv1-coef.rds"
blocks.out <- "intermediate/floods/prblocks.rds"
floods.out <- "intermediate/floods/prfloods.rds"
spillovers.out <- "intermediate/floods/prspillovers.rds"

out <- "documents/working paper/floods/tables/"

# for subset of flood events where SENTIDO == AMBOS

blocks1.out <- "intermediate/floods/prblocks1.rds"
floods1.out <- "intermediate/floods/prfloods1.rds"
spillovers1.out <- "intermediate/floods/prspillovers1.rds"


#   read files

trips <- readRDS(trips.path)

#   convert flood duration NA's to 0s

trips$duration1[which(is.na(trips$duration1))] <- "0"
trips$duration1 <- as.numeric(as.character(trips$duration1))

trips$fduration1[which(is.na(trips$fduration1))] <- "0"
trips$fduration1 <- as.numeric(as.character(trips$fduration1))


# IV  - first stage ----------------------------------------------------------------------------

# generate duration of delay

trips$delay <- difftime(trips$flood.fim, trips$flood.time, tz = "", units = "mins")
trips$delay[which(is.na(trips$delay))] <- "0"
trips$delay <- as.numeric(trips$delay)
summary(trips$delay)

saveRDS(trips, trips.path)


# first stage (blocks) -------------------------------------------------------------------------

# contemporaneous rain

iv.1 <- felm(delay ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

iv.1 <- felm(duration1 ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)


# accumulated rain

iv.1 <- felm(delay ~ blocks:mav.bins1 + blocks:mav.bins2 + blocks:mav.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

# accumulated + contemporaneous rain

trips$mav5[which(is.na(trips$mav5))] <- "0"
trips$mav5 <- as.numeric(as.character(trips$mav5))

iv.1 <- felm(duration.mean ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 + blocks:mav5 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"


# run model for subset of flood events where SENTIDO == AMBOS
iv.1 <- felm(delay ~ blocks1:rain.bins1 + blocks1:rain.bins2 + blocks1:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"


# generating predicted values ------------------------------------------------------------------

  # getting fixed effects
iv1.fe <- getfe(iv.1)

  # month fixed effects
iv1.month <- iv1.fe[which(iv1.fe$fe == "month"),]
iv1.month <- iv1.month[, c("effect", "idx")]
names(iv1.month)[which(names(iv1.month) == "effect")] <- "month.fe"

  # day of week fixed effects
iv1.wd <- iv1.fe[which(iv1.fe$fe == "wd"),]
iv1.wd <- iv1.wd[, c("effect", "idx")]
names(iv1.wd)[which(names(iv1.wd) == "effect")] <- "wd.fe"

  # time of day fixed effects
iv1.hour <- iv1.fe[which(iv1.fe$fe == "hour.f"),]
iv1.hour <- iv1.hour[, c("effect", "idx")]
names(iv1.hour)[which(names(iv1.hour) == "effect")] <- "hour.fe"

  # assign each trip with fixed effect values
trips.preds <- trips[, c("TID", "ID_ORDEM", "rain.date", "month", "wd", "hour.f",
                         "blocks", "rain.bins1", "rain.bins2", "rain.bins3", "mav5")]

# run model for subset of flood events where SENTIDO == AMBOS
trips.preds <- trips[, c("TID", "ID_ORDEM", "date", "hour_minute", "month", "wd", "hour.f",
                         "blocks1", "rain.bins1", "rain.bins2", "rain.bins3")]

# convert to factor for merge
trips.preds$month <- as.factor(as.character(trips.preds$month))
trips.preds$wd <- as.factor(as.character(trips.preds$wd))
trips.preds$hour <- as.factor(as.character(trips.preds$hour))

trips.preds <- merge(trips.preds, iv1.month, by.x = "month", by.y = "idx", all.x = TRUE)
trips.preds <- merge(trips.preds, iv1.wd, by.x = "wd", by.y = "idx", all.x = TRUE)
trips.preds <- merge(trips.preds, iv1.hour, by.x = "hour.f", by.y = "idx", all.x = TRUE)


  # generate predicted value of flood duration for spillovers

b1 <- iv1.coef$Estimate[[1]]
trips.preds$X1 <- trips.preds$blocks * trips.preds$rain.bins1

b2 <- iv1.coef$Estimate[[2]]
trips.preds$X2 <- trips.preds$blocks * trips.preds$rain.bins2

b3 <- iv1.coef$Estimate[[3]]
trips.preds$X3 <- trips.preds$blocks * trips.preds$rain.bins3

b4 <- iv1.coef$Estimate[[4]]
trips.preds$X4 <- trips.preds$blocks * trips.preds$mav5

trips.preds$pr.blocks <- b1 * trips.preds$X1 + 
                         b2 * trips.preds$X2 + 
                         b3 * trips.preds$X3 + 
                         b4 * trips.preds$X4 +
                         trips.preds$month.fe + 
                         trips.preds$wd.fe + 
                         trips.preds$hour.fe
summary(trips.preds$pr.blocks)

# merge with main dataset
trips.preds <- trips.preds[,c("TID","pr.blocks")]
trips <- merge(trips, trips.preds, by = "TID", all.x = TRUE)

# for subset of flood events where SENTIDO == AMBOS
trips.preds <- trips[,c("TID","fitted.blocks")]
saveRDS(trips.preds, blocks1.out)

rm(iv.1)
gc()

# first stage (floods) -------------------------------------------------------------------------

# contemporaneous rain

iv.2 <- felm(delay ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f, data = trips)

trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

iv.2 <- felm(fduration1 ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f, data = trips)


# accumulated rain

iv.2 <- felm(delay ~ floods:mav.bins1 + floods:mav.bins2 + floods:mav.bins3 | month + wd + hour.f, data = trips)

trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# accumulated + contemporaneous rain

iv.2 <- felm(fduration.mean ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 + floods:mav5 | month + wd + hour.f, data = trips)

trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# excluding 0s

trips0 <- trips[which(trips$rain > 0),]

iv.2 <- felm(fduration.mean ~ floods:rain.bins2 + floods:rain.bins3 + floods:mav5 | month + wd + hour.f, data = trips0)

# run model for subset of flood events where SENTIDO == AMBOS
iv.2 <- felm(delay ~ floods1:rain.bins1 + floods1:rain.bins2 + floods1:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# generating predicted values -----------------------

# getting fixed effects
iv2.fe <- getfe(iv.2)

# month fixed effects
iv2.month <- iv2.fe[which(iv2.fe$fe == "month"),]
iv2.month <- iv2.month[, c("effect", "idx")]
names(iv2.month)[which(names(iv2.month) == "effect")] <- "month.fe"

# day of week fixed effects
iv2.wd <- iv2.fe[which(iv2.fe$fe == "wd"),]
iv2.wd <- iv2.wd[, c("effect", "idx")]
names(iv2.wd)[which(names(iv2.wd) == "effect")] <- "wd.fe"

# time of day fixed effects
iv2.hour <- iv2.fe[which(iv2.fe$fe == "hour.f"),]
iv2.hour <- iv2.hour[, c("effect", "idx")]
names(iv2.hour)[which(names(iv2.hour) == "effect")] <- "hour.fe"

# assign each trip with fixed effect values

rm(trips.preds)

trips.preds <- trips[, c("TID", "ID_ORDEM", "rain.date", "month", "wd", "hour.f",
                         "floods", "rain.bins1", "rain.bins2", "rain.bins3", "mav5")]

# convert to factor for merge
trips.preds$month <- as.factor(as.character(trips.preds$month))
trips.preds$wd <- as.factor(as.character(trips.preds$wd))
trips.preds$hour <- as.factor(as.character(trips.preds$hour))

trips.preds <- merge(trips.preds, iv2.month, by.x = "month", by.y = "idx", all.x = TRUE)
trips.preds <- merge(trips.preds, iv2.wd, by.x = "wd", by.y = "idx", all.x = TRUE)
trips.preds <- merge(trips.preds, iv2.hour, by.x = "hour.f", by.y = "idx", all.x = TRUE)

# generate predicted value of flood duration for floods

b1 <- iv2.coef$Estimate[[1]]
trips.preds$X1 <- trips.preds$floods * trips.preds$rain.bins1

b2 <- iv2.coef$Estimate[[2]]
trips.preds$X2 <- trips.preds$floods * trips.preds$rain.bins2

b3 <- iv2.coef$Estimate[[3]]
trips.preds$X3 <- trips.preds$floods * trips.preds$rain.bins3

b4 <- iv2.coef$Estimate[[4]]
trips.preds$X4 <- trips.preds$floods * trips.preds$mav5

trips.preds$pr.floods <- b1 * trips.preds$X1 + 
                         b2 * trips.preds$X2 + 
                         b3 * trips.preds$X3 + 
                         b4 * trips.preds$X4 +
                         trips.preds$month.fe + 
                         trips.preds$wd.fe + 
                         trips.preds$hour.fe

summary(trips.preds$pr.floods)

# merge with main dataset
trips.preds <- trips.preds[,c("TID", "pr.floods")]
trips <- merge(trips, trips.preds, by = "TID", all.x = TRUE)

# for subset of flood events where SENTIDO == AMBOS
trips.preds <- trips[,c("TID", "fitted.floods")]
saveRDS(trips.preds, floods1.out)

rm(iv.2)
gc()

# first stage (spillovers) -------------------------------------------------------------------------

iv.3 <- felm(delay ~ spillovers:rain.bins1 + spillovers:rain.bins2 + spillovers:rain.bins3 | month + wd + hour.f, data = trips)

trips$fitted.spill <- as.numeric(fitted(iv.3))
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

# run model for subset of flood events where SENTIDO == AMBOS
iv.3 <- felm(delay ~ spillovers1:rain.bins1 + spillovers1:rain.bins2 + spillovers1:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.spill <- as.numeric(fitted(iv.3))
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

# generating predicted values -----------------------

# getting fixed effects
iv3.fe <- getfe(iv.3)

# month fixed effects
iv3.month <- iv3.fe[which(iv3.fe$fe == "month"),]
iv3.month <- iv3.month[, c("effect", "idx")]
names(iv3.month)[which(names(iv3.month) == "effect")] <- "month.fe"

# day of week fixed effects
iv3.wd <- iv3.fe[which(iv3.fe$fe == "wd"),]
iv3.wd <- iv3.wd[, c("effect", "idx")]
names(iv3.wd)[which(names(iv3.wd) == "effect")] <- "wd.fe"

# time of day fixed effects
iv3.hour <- iv3.fe[which(iv3.fe$fe == "hour.f"),]
iv3.hour <- iv3.hour[, c("effect", "idx")]
names(iv3.hour)[which(names(iv3.hour) == "effect")] <- "hour.fe"

# assign each trip with fixed effect values
rm(trips.preds)

trips.preds <- trips[, c("TID", "ID_ORDEM", "date", "hour_minute", "month", "wd", "hour.f",
                         "spillovers", "rain.bins1", "rain.bins2", "rain.bins3")]
trips.preds <- merge(trips.preds, iv3.month, by.x = "month", by.y = "idx", all.x = TRUE)
trips.preds <- merge(trips.preds, iv3.wd, by.x = "wd", by.y = "idx", all.x = TRUE)
trips.preds <- merge(trips.preds, iv3.hour, by.x = "hour.f", by.y = "idx", all.x = TRUE)

# generate predicted value of flood duration for spillovers

b1 <- iv3.coef$Estimate[[1]]
trips.preds$X1 <- trips.preds$spillovers * trips.preds$rain.bins1

b2 <- iv3.coef$Estimate[[2]]
trips.preds$X2 <- trips.preds$spillovers * trips.preds$rain.bins2

b3 <- iv3.coef$Estimate[[3]]
trips.preds$X3 <- trips.preds$spillovers * trips.preds$rain.bins3

trips.preds$pr.spillovers <- b1*trips.preds$X1 + b2*trips.preds$X2 + b3*trips.preds$X3 + trips.preds$month.fe + trips.preds$wd.fe + trips.preds$hour.fe
summary(trips.preds$pr.spillovers)

trips.preds <- trips.preds[, c("TID","pr.spillovers")]
saveRDS(trips.preds, spillovers.out)

# for subset of flood events where SENTIDO == AMBOS
trips.preds <- trips[, c("TID","fitted.spill")]
saveRDS(trips.preds, spillovers1.out)


rm(iv.3)
gc()

# generate table -------------------------------------------------------------------------------

# accumulated rain
stargazer(iv.1, iv.2, 
          align = TRUE,
          type = "latex",
          title = "IV 1st Stage with Accumulated Rainfall",
          out = paste0(out, "iv-firststage(acc-rain).tex"))

stargazer(iv.1, iv.2, 
          align = TRUE,
          type = "latex",
          title = "IV 1st Stage")

# output ---------------------------------------------------------------------------------------

iv.coef <- rbind(iv1.coef, iv2.coef, iv3.coef)
saveRDS(iv.coef, coef.path)

saveRDS(trips, trips.path)

# for subset of flood events where SENTIDO == AMBOS
coef.path <- "intermediate/floods/iv1-coef1.rds"

iv.coef <- rbind(iv1.coef, iv2.coef, iv3.coef)
saveRDS(iv.coef, coef.path)

trips.path <- "intermediate/floods/date-merge1.rds"

saveRDS(trips, trips.path)
