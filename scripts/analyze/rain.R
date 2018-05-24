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

#   effect of rain on travel time --------------------------------------------------------------

# trip FE 
# contemporaneous rain bins
# cluster standard errors at trip level

m1 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM | 0 | ID_ORDEM, data = trips)

# trip + month FE 
# contemporaneous rain bins
# cluster standard errors at trip level

m2 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3  | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)


# trip + month + day of week FE 
# contemporaneous rain bins
# cluster standard errors at trip level

m3 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3  | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)

# trip + month + day of week + time of day FE 
# contemporaneous rain
# cluster standard errors at trip level

m4 <- felm(ln_tr.time ~ rain.bins1 + rain.bins2 + rain.bins3  | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


# excluding 0s ---------------------------------------------------------------------------------

trips0 <- trips[which(trips$rain > 0),]

m1 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 | ID_ORDEM | 0 | ID_ORDEM, data = trips0)
m2 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 | ID_ORDEM + month | 0 | ID_ORDEM, data = trips0)
m3 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips0)
m4 <- felm(ln_tr.time ~ rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips0)


# generate table -------------------------------------------------------------------------------

# contemporaneous rain
stargazer(m1, m2, m3, m4, 
          align = TRUE, 
          type = "latex",
          df = FALSE,
          title = "Effect of Accumulated Rain on Travel Time",
          covariate.labels = c("Light Rain",
                               "Moderate Rain",
                               "Heavy Rain"),
          add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                           c("Month FE", "N", "Y", "Y", "Y"),
                           c("Day of Week FE", "N", "N", "Y", "Y"),
                           c("Hour FE", "N", "N", "N", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "rain.tex"))


# excluding 0s

stargazer(m1, m2, m3, m4, 
          align = TRUE, 
          type = "latex",
          df = FALSE,
          title = "Effect of Rain on Travel Time (Excluding 0s)",
          covariate.labels = c("Medium Rain",
                               "Heavy Rain"),
          dep.var.labels = "ln(Trip Duration)",
          add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                           c("Month FE", "N", "Y", "Y", "Y"),
                           c("Day of Week FE", "N", "N", "Y", "Y"),
                           c("Hour FE", "No", "N", "N", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          out = paste0(out, "rain0.tex"))

# interact with hour dummies -------------------------------------------------------------------

m5 <- felm(ln_tr.time ~ rain.bins1:hour.f + rain.bins2:hour.f + rain.bins3:hour.f  | ID_ORDEM + month + wd + hour.f, data = trips)

# ----------------------------------------------------------------------------------------------
# peak hour effect of rain

# contemporaneous rain

m5 <- felm(ln_tr.time ~ rain.bins1:early.peak + rain.bins2:early.peak + rain.bins3:early.peak +
                        rain.bins1:late.peak + rain.bins2:late.peak + rain.bins3:late.peak +
                        rain.bins1:not.peak + rain.bins2:not.peak + rain.bins3:not.peak | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

