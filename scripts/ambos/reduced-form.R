# ---------------------------------------------------------------------------------------- #
#                                                                                          #
# Regress trip duration on blocks and floods                                               #
# (for subset of flood events where both directions of traffic are affected                #
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

out.path <- "views/floods/"

#   read files

trips <- readRDS(trips.path)

# subset to trips which encounter floods where both directions of traffic are affected
# the variable 'ambos' gives us the number of flood events per trip where both directions of traffic are affected

trips <- trips[trips$ambos > 0,]

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(ln_tr.time ~ blocks + floods + spillovers| ID_ORDEM | 0 | ID_ORDEM, data = trips)


# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)


# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)


# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# output ----------------------------------------------------------------------------------------

stargazer(m1, m2, m3, m4,
          type = "latex",
          df = FALSE,
          dep.var.label = "ln(Trip Duration)",
          add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                           c("Month FE", "N", "Y", "Y", "Y"),
                           c("Day of Week FE", "N", "N", "Y", "Y"),
                           c("Hour FE", "N", "N", "N", "Y")),
          notes = "Standard errors clustered at trip level.",
          out = paste0(out.path, "reduced-form(ambos).tex"))
