
#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run travel time on delay from blocks, floods and spillovers                            |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#   |                                                                                         |
#   |  Edited on 02/22/2018 to include accumulated rainfall                                   |
#     ----------------------------------------------------------------------------------------

# Notes:

# Archived on 05/24/2018 to remove spillovers and cluster standard errors at the trip level


rm(list = ls())

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

# for subset of flood events where SENTIDO == AMBOS
trips.path <- "intermediate/floods/date-merge1.rds"

blocks1.out <- "intermediate/floods/prblocks1.rds"
floods1.out <- "intermediate/floods/prfloods1.rds"
spillovers1.out <- "intermediate/floods/prspillovers1.rds"

#   output

out <- "views/floods/"
coef2.path <- "intermediate/floods/iv2-coef.rds"
iv.out <- "intermediate/floods/second-stage.rds"

# for subset of flood events where SENTIDO == AMBOS
iv.out1 <- "intermediate/floods/second-stage1.rds"
coef2_1.path <- "intermediate/floods/iv2-coef1.rds"

#   read files

trips <- readRDS(trips.path)
names(trips)

#   create dataset for use in IV 2nd stage -----------------------------------------------------

trips.subset <- trips[, c("TID", "ID_ORDEM", "month", "wd", "hour.f", "ln_tr.time")]
trips.subset <- as.data.table(trips.subset)
trips.subset$MID <- seq.int(nrow(trips.subset))
saveRDS(trips.subset, iv.out)
rm(trips)

# for subset of flood events where SENTIDO == AMBOS

trips.subset <- trips[, c("TID", "ID_ORDEM", "month", "wd", "hour.f", "tr.time")]
trips.subset <- as.data.table(trips.subset)
trips.subset$MID <- seq.int(nrow(trips.subset))
saveRDS(trips.subset, iv.out1)
rm(trips)

# merge predicted values for blocks ------------------------------------------------------------

blocks <- readRDS(blocks.out)
blocks$MID <- seq.int(nrow(blocks))
blocks <- as.data.table(blocks)
saveRDS(blocks, blocks.out)

trips.subset <- merge(trips.subset, blocks[,c("MID", "manual.blocked", "fitted.blocked")], by = "MID", all.x = TRUE)
names(trips.subset)
saveRDS(trips.subset, iv.out)

rm(blocks)
gc()

# for subset of flood events where SENTIDO == AMBOS

blocks1 <- readRDS(blocks1.out)
blocks1 <- as.data.table(blocks1)
blocks1$MID <- seq.int(nrow(blocks1))
saveRDS(blocks1, blocks1.out)

trips.subset <- merge(trips.subset, blocks1[,c("MID", "fitted.blocks")], by = "MID", all.x = TRUE)
names(trips.subset)
saveRDS(trips.subset, iv.out1)

rm(blocks1)
gc()

# merge predicted values for floods ------------------------------------------------------------

floods <- readRDS(floods.out)
floods$MID <- seq.int(nrow(floods))
floods <- as.data.table(floods)
saveRDS(floods, floods.out)

trips.subset <- merge(trips.subset, floods[,c("MID", "manual.floods", "fitted.floods")], by = "MID", all.x = TRUE)
saveRDS(trips.subset, iv.out)

rm(floods)
gc()

# for subset of flood events where SENTIDO == AMBOS
floods1 <- readRDS(floods1.out)
floods1$MID <- seq.int(nrow(floods1))
floods1 <- as.data.table(floods1)
saveRDS(floods1, floods1.out)

trips.subset <- merge(trips.subset, floods1[,c("MID", "fitted.floods")], by = "MID", all.x = TRUE)
saveRDS(trips.subset, iv.out1)

rm(floods1)
gc()

# merge predicted values for spillovers --------------------------------------------------------

spillovers <- readRDS(spillovers.out)
spillovers$MID <- seq.int(nrow(spillovers))
spillovers <- as.data.table(spillovers)
saveRDS(spillovers, spillovers.out)

trips.subset <- merge(trips.subset, spillovers[,c("MID", "manual.spill", "fitted.spill")], by = "MID", all.x = TRUE)
saveRDS(trips.subset, iv.out)

rm(spillovers)
gc()

# for subset of flood events where SENTIDO == AMBOS
spillovers1 <- readRDS(spillovers1.out)
spillovers1$MID <- seq.int(nrow(spillovers1))
spillovers1 <- as.data.table(spillovers1)
saveRDS(spillovers1, spillovers1.out)

trips.subset <- merge(trips.subset, spillovers1[,c("MID", "fitted.spill")], by = "MID", all.x = TRUE)
saveRDS(trips.subset, iv.out1)

rm(spillovers1)
gc()

rm(trips.subset)
gc()

#   IV second stage ----------------------------------------------------------------------------

trips <- readRDS(iv.out)

# run model for subset of flood events where SENTIDO == AMBOS
trips <- readRDS(iv.out1)

iv.1 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM , data = trips)
iv.1 <- felm(tr.time ~ pr.blocks + pr.floods | ID_ORDEM , data = trips)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

iv.2 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM + month, data = trips)
iv.2 <- felm(tr.time ~ pr.blocks + pr.floods | ID_ORDEM + month, data = trips)


iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

iv.3 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM + month + wd , data = trips)
iv.3 <- felm(tr.time ~ pr.blocks + pr.floods | ID_ORDEM + month + wd , data = trips)

iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

iv.4 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM + month + wd + hour.f , data = trips)
iv.4 <- felm(tr.time ~ pr.blocks + pr.floods | ID_ORDEM + month + wd + hour.f , data = trips)


iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

# LaTeX output
stargazer(iv.1, iv.2, iv.3, iv.4, 
          align = TRUE,
          type = "latex",
          title = "IV Second Stage",
          covariate.labels = c("Blocks",
                               "Floods"),
          add.lines = list(c("Trip FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "No", "Yes", "Yes", "Yes"),
                           c("Day of Week FE", "No", "No", "Yes", "Yes"),
                           c("Hour FE", "No", "No", "No", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "iv-secondstage.tex"))

# save coefficients

coef2 <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef2, coef2.path)

# for the subset of flood events where SENTIDO = AMBOS

coef2.1 <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef2.1, coef2_1.path)

# getting predicted values manually ------------------------------------------------------------

trips.subset <- readRDS(iv.out)

# run model for subset of flood events where SENTIDO == AMBOS
trips.subset <- readRDS(iv.out1)

# model ----------------------------------------------------------------------------------------
iv.5 <- felm(tr.time ~ fitted.blocks + fitted.floods + fitted.spill | ID_ORDEM + month + wd + hour.f , data = trips.subset)
iv5.coef <- as.data.frame(summary(iv.5)$coefficients)

# ID_ORDEM FE ----------------------------------------------------------------------------------
trip.fe.path <- "analysis/tripfe.rds"

# for subset of flood events where SENTIDO == AMBOS
trip.fe.path <- "analysis/tripfe1.rds"

# get trip FEs
iv5.fe <- getfe(iv.5)
iv5.trip <- iv5.fe[which(iv5.fe$fe == "ID_ORDEM"),]
iv5.trip <- iv5.trip[, c("effect", "idx")]
names(iv5.trip)[which(names(iv5.trip) == "effect")] <- "trip.fe"

saveRDS(iv5.trip, trip.fe.path)

# month fixed effects --------------------------------------------------------------------------
month.fe.path <- "analysis/monthfe.rds"

# for subset of flood events where SENTIDO == AMBOS
month.fe.path <- "analysis/monthfe1.rds"

# get month FEs
iv5.month <- iv5.fe[which(iv5.fe$fe == "month"),]
iv5.month <- iv5.month[, c("effect", "idx")]
names(iv5.month)[which(names(iv5.month) == "effect")] <- "month.fe"

saveRDS(iv5.month, month.fe.path)

# day of week fixed effects --------------------------------------------------------------------
wd.fe.path <- "analysis/wdfe.rds"

# for subset of flood events where SENTIDO == AMBOS
wd.fe.path <- "analysis/wdfe1.rds"

# get day of week FEs
iv5.wd <- iv5.fe[which(iv5.fe$fe == "wd"),]
iv5.wd <- iv5.wd[, c("effect", "idx")]
names(iv5.wd)[which(names(iv5.wd) == "effect")] <- "wd.fe"

saveRDS(iv5.wd, wd.fe.path)

# time of day fixed effects ----------------------------------------------
hour.fe.path <- "analysis/hourfe.rds"

# for subset of flood events where SENTIDO == AMBOS
hour.fe.path <- "analysis/hourfe1.rds"

iv5.hour <- iv5.fe[which(iv5.fe$fe == "hour.f"),]
iv5.hour <- iv5.hour[, c("effect", "idx")]
names(iv5.hour)[which(names(iv5.hour) == "effect")] <- "hour.fe"

saveRDS(iv5.hour, hour.fe.path)

# merge fixed effects coefficients to trips dataset --------------------------------------------

trips.subset <- merge(trips.subset, iv5.trip, by.x = "ID_ORDEM", by.y = "idx", all.x = TRUE)
trips.subset <- merge(trips.subset, iv5.month, by.x = "month", by.y = "idx", all.x = TRUE)
trips.subset <- merge(trips.subset, iv5.wd, by.x = "wd", by.y = "idx", all.x = TRUE)
trips.subset <- merge(trips.subset, iv5.hour, by.x = "hour.f", by.y = "idx", all.x = TRUE)

b1 <- iv5.coef$Estimate[[1]]

b2 <- iv5.coef$Estimate[[2]]

b3 <- iv5.coef$Estimate[[3]]

trips.subset$preds <- b1*trips.subset$fitted.blocks + b2*trips.subset$fitted.floods + b3*trips.subset$fitted.spill + trips.subset$trip.fe + trips.subset$month.fe + trips.subset$wd.fe + trips.subset$hour.fe
summary(trips.subset$preds)

saveRDS(trips.subset, iv.out)

# for subset of flood events where SENTIDO == AMBOS
saveRDS(trips.subset, iv.out1)

#   output -------------------------------------------------------------------------------------

iv.coef <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(iv.coef, coef.path)

# for subset of flood events where SENTIDO == AMBOS
coef2.path <- "intermediate/floods/iv2-coef.rds"

iv.coef <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(iv.coef, coef2.path)


saveRDS(trips, trips.path)

rm(list = ls())
gc()
