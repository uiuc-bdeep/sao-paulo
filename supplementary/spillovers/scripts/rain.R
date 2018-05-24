#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run effect of rain bins on travel time                                                 |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#   |  Edited on 2/22/2018 to add in accumulated rain                                         |
#   |  Edited on 2/26/2018 to run models using dataset with unique rows for crawled trips     |
#   |  Edited on 3/9/2018 to run model with accumulated and contemporaneous rain together     |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Notes:

# Archived on 05/24/2018
# Removed spillovers and ran models by clustering standard errors at trip level

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

#   output

# path for saving regression output

out <- "documents/working paper/floods/tables/"

#   read files

trips <- readRDS(trips.path)

rain <- readRDS(rain.path)

#   create variable for date and hour that trip started

trips$year <- year(trips$date)
trips$day <- day(trips$date)

trips$hour <- as.numeric(as.character(trips$hour))
trips$month <- as.numeric(as.character(trips$month))


trips$rain.date <- ISOdatetime(year = trips$year,
                                month = trips$month,
                                day = trips$day,
                                hour = trips$hour,
                                min = 0,
                                sec = 0,
                                tz = "")

trips$rain.date <- format(as.POSIXct(strptime(trips$rain.date,"%Y-%m-%d %H:%M",tz=""), format = "%Y-%m-%d %H:%M"))


#   merge accumulated rain with trips dataset

trips <- merge(trips, rain[,c("rain.date","mav")], by = "rain.date", all.x = TRUE)

# save datasets

saveRDS(trips, trips.path)

#   generate rain bins -------------------------------------------------------------------------

#   contemporaneous rain

summary(trips$rain)

trips$rain.bins <- as.numeric(ifelse(trips$rain == 0, "0",
                          ifelse(trips$rain <= 2.5, "1",
                                 ifelse(trips$rain <= 7.6, "2",
                                        ifelse(trips$rain > 7.6, "3", NA)))))
summary(trips$rain.bins)

trips$rain.dummy <- as.numeric(ifelse(trips$rain > 0, 1, 0))
summary(trips$rain.dummy)


trips$rain.bins1 <- as.numeric(ifelse(trips$rain.bins == "1", 1, 0))
trips$rain.bins2 <- as.numeric(ifelse(trips$rain.bins == "2", 1, 0))
trips$rain.bins3 <- as.numeric(ifelse(trips$rain.bins == "3", 1, 0))

summary(trips$rain.bins1)
summary(trips$rain.bins2)
summary(trips$rain.bins3)

saveRDS(trips, trips.path)

# accumulated rain

trips$mav <- as.numeric(as.character(trips$mav))

trips$mav.bins <- ifelse(trips$mav == 0, "0",
                                     ifelse(trips$mav <= 2.5, "1",
                                            ifelse(trips$mav <= 7.6, "2",
                                                   ifelse(trips$mav > 7.6, "3", NA))))

trips$mav.bins <- as.factor(as.character(trips$mav.bins))
summary(trips$mav.bins)

trips$mav.bins1 <- as.numeric(ifelse(trips$mav.bins == "1", 1, 0))
trips$mav.bins2 <- as.numeric(ifelse(trips$mav.bins == "2", 1, 0))
trips$mav.bins3 <- as.numeric(ifelse(trips$mav.bins == "3", 1, 0))

summary(trips$mav.bins1)
summary(trips$mav.bins2)
summary(trips$mav.bins3)

saveRDS(trips, trips.path)


#   effect of rain on travel time --------------------------------------------------------------

# trip FE --------------------------------------------------------------------------------------

# contemporaneous rain

m1 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM, data = trips)
m1.coef <- as.data.frame(summary(m1)$coefficients)
m1.coef$model <- "m1"
rm(m1)

# accumulated rain

m1 <- felm(ln_tr.time ~ mav.bins1 + mav.bins2 + mav.bins3 | ID_ORDEM, data = trips)
m1.coef <- as.data.frame(summary(m1)$coefficients)
m1.coef$model <- "m1"
rm(m1)

# accumulated rain + contemporaneous rain

m1 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3 + mav5 | ID_ORDEM, data = trips)
m1.coef <- as.data.frame(summary(m1)$coefficients)
m1.coef$model <- "m1"
rm(m1)



# trip + month FE ------------------------------------------------------------------------------

# contemporaneous rain

m2 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3  | ID_ORDEM + month, data = trips)
m2.coef <- as.data.frame(summary(m2)$coefficients)
m2.coef$model <- "m2"
rm(m2)

# accumulated rain

m2 <- felm(ln_tr.time ~ mav.bins1 + mav.bins2 + mav.bins3  | ID_ORDEM + month, data = trips)
m2.coef <- as.data.frame(summary(m2)$coefficients)
m2.coef$model <- "m2"
rm(m2)

# accumulated rain + contemporaneous rain

m2 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3 + mav5  | ID_ORDEM + month, data = trips)
m2.coef <- as.data.frame(summary(m2)$coefficients)
m2.coef$model <- "m2"
rm(m2)


# trip + month + day of week FE ----------------------------------------------------------------

# contemporaneous rain

m3 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3  | ID_ORDEM + month + wd, data = trips)
m3.coef <- as.data.frame(summary(m3)$coefficients)
m3.coef$model <- "m3"
rm(m3)

# accumulated rain

m3 <- felm(ln_tr.time ~ mav.bins1 + mav.bins2 + mav.bins3  | ID_ORDEM + month + wd, data = trips)
m3.coef <- as.data.frame(summary(m3)$coefficients)
m3.coef$model <- "m3"
rm(m3)

# accumulated rain + contemporaneous rain

m3 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3 + mav5  | ID_ORDEM + month + wd, data = trips)
m3.coef <- as.data.frame(summary(m3)$coefficients)
m3.coef$model <- "m3"
rm(m3)


# trip + month + day of week + time of day FE --------------------------------------------------

# contemporaneous rain

m4 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3  | ID_ORDEM + month + wd + hour.f, data = trips)
m4.coef <- as.data.frame(summary(m4)$coefficients)
m4.coef$model <- "m4"
rm(m4)

# accumulated rain

m4 <- felm(ln_tr.time ~ mav.bins1 + mav.bins2 + mav.bins3  | ID_ORDEM + month + wd + hour.f, data = trips)
m4.coef <- as.data.frame(summary(m4)$coefficients)
m4.coef$model <- "m4"
rm(m4)

# accumulated rain + contemporaneous rain

m4 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3 + mav5  | ID_ORDEM + month + wd + hour.f, data = trips)
m4.coef <- as.data.frame(summary(m4)$coefficients)
m4.coef$model <- "m4"
rm(m4)


# excluding 0s ---------------------------------------------------------------------------------

trips0 <- trips[which(trips$rain > 0),]

m1 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 + mav5 | ID_ORDEM, data = trips0)
m2 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 + mav5 | ID_ORDEM + month, data = trips0)
m3 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 + mav5 | ID_ORDEM + month + wd, data = trips0)
m4 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 + mav5 | ID_ORDEM + month + wd + hour.f, data = trips0)

# with lags ------------------------------------------------------------------------------------

# convert to numeric variable

trips$rain <- as.numeric(as.character(trips$rain))

trips$lag1 <- as.numeric(as.character(trips$lag1))
trips$lag1[which(is.na(trips$lag1))] <- "0"
trips$lag1 <- as.numeric(as.character(trips$lag1))

trips$lag2 <- as.numeric(as.character(trips$lag2))
trips$lag2[which(is.na(trips$lag2))] <- "0"
trips$lag2 <- as.numeric(as.character(trips$lag2))

trips$lag3 <- as.numeric(as.character(trips$lag3))
trips$lag3[which(is.na(trips$lag3))] <- "0"
trips$lag3 <- as.numeric(as.character(trips$lag3))

trips$lag4 <- as.numeric(as.character(trips$lag4))
trips$lag4[which(is.na(trips$lag4))] <- "0"
trips$lag4 <- as.numeric(as.character(trips$lag4))

trips$lag5 <- as.numeric(as.character(trips$lag5))
trips$lag5[which(is.na(trips$lag5))] <- "0"
trips$lag5 <- as.numeric(as.character(trips$lag5))

trips$lag6 <- as.numeric(as.character(trips$lag6))
trips$lag7 <- as.numeric(as.character(trips$lag7))
trips$lag8 <- as.numeric(as.character(trips$lag8))
trips$lag9 <- as.numeric(as.character(trips$lag9))
trips$lag10 <- as.numeric(as.character(trips$lag10))
trips$lag11 <- as.numeric(as.character(trips$lag11))
trips$lag12 <- as.numeric(as.character(trips$lag12))


m4 <- felm(ln_tr.time ~ rain + lag1 + lag2 + lag3 + lag4 + lag5 
                        + lag6 + lag7 + lag8 + lag9 + lag10 + lag11 + lag12 | ID_ORDEM + month + wd + hour.f, data = trips)

# generate table

stargazer(m4,
          align = TRUE,
          title = "Effect of Rain on Travel Time",
          type = "latex",
          covariate.labels = c("$Rain_t$",
                               "$Rain_{t-1}$",
                               "$Rain_{t-2}$",
                               "$Rain_{t-3}$",
                               "$Rain_{t-4}$",
                               "$Rain_{t-5}$",
                               "$Rain_{t-6}$",
                               "$Rain_{t-7}$",
                               "$Rain_{t-8}$",
                               "$Rain_{t-9}$",
                               "$Rain_{t-10}$",
                               "$Rain_{t-11}$",
                               "$Rain_{t-12}$"),
          add.lines = list(c("Trip FE", "Yes"),
                           c("Month FE", "Yes"),
                           c("Day of Week FE","Yes"),
                           c("Hour FE", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "rain-lags.tex"))

# Up to 5 lags ---------------------------------------------------------------------------------

m4 <- felm(ln_tr.time ~ rain + lag1 + lag2 + lag3 + lag4 + lag5 | ID_ORDEM + month + wd + hour.f, data = trips)

stargazer(m4,
          align = TRUE,
          title = "Effect of Rain on Travel Time",
          type = "latex",
          covariate.labels = c("$Rain_t$",
                               "$Rain_{t-1}$",
                               "$Rain_{t-2}$",
                               "$Rain_{t-3}$",
                               "$Rain_{t-4}$",
                               "$Rain_{t-5}$"),
          add.lines = list(c("Trip FE", "Yes"),
                           c("Month FE", "Yes"),
                           c("Day of Week FE","Yes"),
                           c("Hour FE", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "rain-lags5.tex"))


# generate table -------------------------------------------------------------------------------

# contemporaneous rain
stargazer(m1, m2, m3, m4, 
          align = TRUE, 
          type = "latex",
          title = "Effect of Accumulated Rain on Travel Time",
          covariate.labels = c("Low Rain",
                               "Medium Rain",
                               "Heavy Rain"),
          add.lines = list(c("Trip FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "No", "Yes", "Yes", "Yes"),
                           c("Day of Week FE", "No", "No", "Yes", "Yes"),
                           c("Hour FE", "No", "No", "No", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "rain.tex"))

# accumulated rain
stargazer(m1, m2, m3, m4, 
          align = TRUE, 
          title = "Effect of Accumulated Rain on Travel Time",
          covariate.labels = c("Low Rain",
                               "Medium Rain",
                               "Heavy Rain"),
          add.lines = list(c("Trip FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "No", "Yes", "Yes", "Yes"),
                           c("Day of Week FE", "No", "No", "Yes", "Yes"),
                           c("Hour FE", "No", "No", "No", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "rain-accumulated.tex"))

# accumulated rain + contemporaneous rain

stargazer(m1, m2, m3, m4, 
          align = TRUE, 
          title = "Effect of Accumulated Rain on Travel Time",
          covariate.labels = c("Low Rain",
                               "Medium Rain",
                               "Heavy Rain",
                               "5 Hour Average"),
          add.lines = list(c("Trip FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "No", "Yes", "Yes", "Yes"),
                           c("Day of Week FE", "No", "No", "Yes", "Yes"),
                           c("Hour FE", "No", "No", "No", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "rain-both.tex"))


# excluding 0s

stargazer(m1, m2, m3, m4, 
          align = TRUE, 
          title = "Effect of Accumulated Rain on Travel Time",
          covariate.labels = c("Medium Rain",
                               "Heavy Rain",
                               "5-Hour Average"),
          dep.var.labels = "ln(Trip Duration)",
          add.lines = list(c("Trip FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "No", "Yes", "Yes", "Yes"),
                           c("Day of Week FE", "No", "No", "Yes", "Yes"),
                           c("Hour FE", "No", "No", "No", "Yes")),
          out = paste0(out, "rain0.tex"))

# interact with hour dummies -------------------------------------------------------------------

m5 <- felm(ln_tr.time ~ rain.bins1:hour.f + rain.bins2:hour.f + rain.bins3:hour.f  | ID_ORDEM + month + wd + hour.f, data = trips)
m5.coef <- as.data.frame(summary(m5)$coefficients)
m5.coef$model <- "m5"
rm(m5)

# ----------------------------------------------------------------------------------------------
# peak hour effect of rain

# contemporaneous rain

m5 <- felm(ln_tr.time ~ rain.bins1:early.peak + rain.bins2:early.peak + rain.bins3:early.peak +
                        rain.bins1:late.peak + rain.bins2:late.peak + rain.bins3:late.peak +
                        rain.bins1:not.peak + rain.bins2:not.peak + rain.bins3:not.peak | ID_ORDEM + month + wd, data = trips)


# accumulated rain

m5 <- felm(ln_tr.time ~ mav.bins1:early.peak + mav.bins2:early.peak + mav.bins3:early.peak +
             mav.bins1:late.peak + mav.bins2:late.peak + mav.bins3:late.peak +
             mav.bins1:not.peak + mav.bins2:not.peak + mav.bins3:not.peak | ID_ORDEM + month + wd + hour.f, data = trips)




# generate table -------------------------------------------------------------------------------

stargazer(m5,
          align = TRUE,
          title = "Effect of Rain on Travel Time During Peak Hours",
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "peak-hours-rain.tex"))

stargazer(m5,
          align = TRUE,
          title = "Effect of Accumulated Rain on Travel Time During Peak Hours",
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "peak-hours-acc-rain.tex"))

