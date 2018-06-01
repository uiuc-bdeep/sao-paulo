# -------------------------------------------------------------------------------------- #
#                                                                                        #
# Regress trip travel time on predicted flood durations                                  #
#                                                                                        #
#                                                                                        #
# Created by: Amanda Ang                                                                 #
#             Big Data Environmental Economics and Policy Group                          #
#                                                                                        #
# -------------------------------------------------------------------------------------- #



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

coef.path <- "intermediate/floods/iv2-coef.rds"
out <- "views/floods/"

#   read files

trips <- readRDS(trips.path)
names(trips)

#   IV second stage ----------------------------------------------------------------------------

# trip FE

iv.1 <- felm(tr.time ~ fitted.blocks + fitted.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM | 0 | ID_ORDEM, data = trips)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

# trip + month FE

iv.2 <- felm(tr.time ~ fitted.blocks + fitted.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# trip + month + day of week FE

iv.3 <- felm(tr.time ~ fitted.blocks + fitted.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)

iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ fitted.blocks + fitted.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

#   output -------------------------------------------------------------------------------------

# save coefficients

coef <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef, coef.path)

# LaTeX table
stargazer(iv.1, iv.2, iv.3, iv.4, 
           type = "latex",
           df = FALSE,
           title = "IV Second Stage",
           covariate.labels = c("pr.Blocks Duration",
                                "pr.Floods Duration",
                                 "Light Rain",
                                 "Moderate Rain",
                                 "Heavy Rain"),
           add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                            c("Month FE", "N", "Y", "Y", "Y"),
                            c("Day of Week FE", "N", "N", "Y", "Y"),
                            c("Hour FE", "N", "N", "N", "Y")),
           notes = "Standard errors are clustered at the trip level.",
          dep.var.labels = "Trip Duration (minutes)",
          out = paste0(out.path, "iv-secondstage.tex"))
