# ----------------------------------------------------------------------------------------------------- #
#                                                                                                       #
# Regress travel time on block and flood durations with duration bins                                   #
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

# generate duration bins 

quantile(trips$tr.time, c(0.33, 0.67))

trips$short.trips <- ifelse(trips$tr.time > 0 & trips$tr.time <= 9.70, 1, 0)
trips$med.trips <- ifelse(trips$tr.time > 9.70 & trips$tr.time <= 22.31, 1, 0)
trips$long.trips <- ifelse(trips$tr.time > 22.31, 1, 0)

# reduced form model with duration bins -----------------------------------------------

m5 <- felm(tr.time ~ blocks:duration.mean:short.trips + floods:fduration.mean:short.trips + 
                     blocks:duration.mean:med.trips + floods:fduration.mean:med.trips + 
                     blocks:duration.mean:long.trips + floods:fduration.mean:long.trips 
                     + rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


stargazer(m5,
          type = "latex",
          df = FALSE,
          title = "Reduced Form Model with Trip Duration Bins",
          dep.var.labels = c("Trip Duration"),
          notes = "Standard errors clustered at trip level.",
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          out = paste0(out.path, "duration-bins.tex"))
