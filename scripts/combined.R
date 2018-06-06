
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





