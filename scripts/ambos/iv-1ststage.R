
# ---------------------------------------------------------------------------------------- #
#                                                                                          #
# Regress flood duration on blocks and floods                                              #
# (for subset of flood events where both directions of traffic are affected                #
#                                                                                          #
# Created by: Amanda Ang                                                                   #
#             Big Data Environmental Economics and Policy Group                            #
#                                                                                          #
# ---------------------------------------------------------------------------------------- #

# Edited on 05/29/2018

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

# IV  - first stage ----------------------------------------------------------------------------

# blocks

iv.1 <- felm(duration.mean ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 + rain
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.blocks <- fitted(iv.1)

# floods

iv.2 <- felm(fduration.mean ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 + rain
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.floods <- fitted(iv.2)

# spillovers 

iv.3 <- felm(mean ~ spillovers:rain.bins1 + spillovers:rain.bins2 + spillovers:rain.bins3 + rain
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.spill <- fitted(iv.3)

# output ----------------------------------------------------------------------------------------

stargazer(iv.1, iv.2, iv.3,
          type = "latex",
          df = FALSE,
          title = "IV 1st Stage with Traffic Direction Effects",
          add.lines = list(c("Trip FE", "N", "N", "N"),
                           c("Month FE", "Y", "Y", "Y"),
                           c("Day of Week FE", "Y", "Y", "Y"),
                           c("Hour FE", "Y", "Y", "Y")),
          notes = "Standard errors are clustered at the trip level.", 
          out = paste0(out.path, "iv-1ststage(ambos).tex"))
