# ---------------------------------------------------------------------------------------- #
#                                                                                          #
# Regress flood duration on blocks and floods                                              #
#                                                                                          #
# Created by: Amanda Ang                                                                   #
#             Big Data Environmental Economics and Policy Group                            #
#                                                                                          #
# ---------------------------------------------------------------------------------------- #

# clear workspace

rm(list = ls())
gc()

# set working directory

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

#   output

coef.path <- "intermediate/floods/iv1-coef.rds"
out.path <- "views/floods/"

#   read files

trips <- readRDS(trips.path)

# generate 9 hour accumulated rain

trips$lag4 <- as.numeric(as.character(trips$lag4))
trips$lag5 <- as.numeric(as.character(trips$lag5))
trips$lag6 <- as.numeric(as.character(trips$lag6))
trips$lag7 <- as.numeric(as.character(trips$lag7))
trips$lag8 <- as.numeric(as.character(trips$lag8))
trips$lag9 <- as.numeric(as.character(trips$lag9))
trips$lag10 <- as.numeric(as.character(trips$lag10))
trips$lag11 <- as.numeric(as.character(trips$lag11))
trips$lag12 <- as.numeric(as.character(trips$lag12))

trips$lag4[is.na(trips$lag4)] <- 0
trips$lag5[is.na(trips$lag5)] <- 0
trips$lag6[is.na(trips$lag6)] <- 0
trips$lag7[is.na(trips$lag7)] <- 0
trips$lag8[is.na(trips$lag8)] <- 0
trips$lag9[is.na(trips$lag9)] <- 0
trips$lag10[is.na(trips$lag10)] <- 0
trips$lag11[is.na(trips$lag11)] <- 0
trips$lag12[is.na(trips$lag12)] <- 0

trips$acc.rain <- trips$lag4 + trips$lag5 + trips$lag6 + trips$lag7 + trips$lag8 + trips$lag9 + trips$lag10 + trips$lag11 + trips$lag12


# IV  - first stage ----------------------------------------------------------------------------

# blocks

iv.1 <- felm(duration.mean ~ blocks:acc.rain + rain.bins1 + rain.bins2 + rain.bins3
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.blocks <- fitted(iv.1)
trips$res.blocks <- trips$duration.mean - residuals(iv.1)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

# floods

iv.2 <- felm(fduration.mean ~ floods:acc.rain + rain.bins1 + rain.bins2 + rain.bins3 
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.floods <- fitted(iv.2)
trips$res.floods <- trips$fduration.mean - residuals(iv.2)

iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# output ----------------------------------------------------------------------------------------

coef <- rbind(iv1.coef, iv2.coef)
saveRDS(coef, coef.path)

stargazer(iv.1, iv.2, 
          type = "latex",
          df = FALSE,
          dep.var.labels = c("Blocks Duration",
                             "Floods Duration"),
          covariate.labels = c("Light Rain", 
                               "Moderate Rain",
                               "Heavy Rain",
                               "Blocks $\times$ Acc. Rain",
                               "Floods $\times$ Acc. Rain"),
          add.lines = list(c("Trip FE", "N", "N"),
                           c("Month FE", "Y", "Y"),
                           c("Day of Week FE", "Y","Y"),
                           c("Hour FE", "Y", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV First Stage",
          out = paste0(out.path, "iv-1ststage.tex"))
