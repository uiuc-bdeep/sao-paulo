# ----------------------------------------------------------------------------------------------------- #
#                                                                                                       #
# Regress travel time on block and flood durations                                                      #
#                                                                                                       #
#                                                                                                       #
# Created by: Amanda Ang                                                                                #
#             Big Data Environmental Economics and Policy Group                                         #
# ----------------------------------------------------------------------------------------------------- #


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

out.path <- "views/floods/"

#   read files

trips <- readRDS(trips.path)

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(tr.time ~ duration.mean + fduration.mean 
                    + rain.bins1 + rain.bins2 + rain.bins3| ID_ORDEM | 0 | ID_ORDEM, data = trips)


# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(tr.time ~ duration.mean + fduration.mean
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)


# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(tr.time ~ duration.mean + fduration.mean 
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)


# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(tr.time ~ duration.mean + fduration.mean 
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


# reduced form model with peak hour interactions -----------------------------------------------

m5 <- felm(tr.time ~ duration.mean:early.peak + fduration.mean:early.peak + 
                     duration.mean:late.peak + fduration.mean:late.peak + 
                     duration.mean:not.peak + fduration.mean:not.peak 
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# output ----------------------------------------------------------------------------------------

stargazer(m1, m2, m3, m4,
          type = "latex",
          df = FALSE,
          dep.var.labels = c("Trip Duration"),
          covariate.labels = c("Blocks Duration",
                               "Floods Duration",
                               "Light Rain",
                               "Moderate Rain",
                               "Heavy Rain"),
          notes = "Standard errors clustered at trip level.",
          add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                           c("Month FE", "N", "Y", "Y", "Y"),
                           c("Day of Week FE", "N", "N", "Y", "Y"),
                           c("Hour FE", "N", "N", "N", "N")),
                   out = paste0(out.path, "reduced-form.tex"))


