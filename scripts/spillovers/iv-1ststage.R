# ---------------------------------------------------------------------------------------- #
#                                                                                          #
# Regress flood duration on blocks and floods                                              #
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

coef.path <- "intermediate/floods/iv1-coef(spillovers).rds"
out.path <- "views/floods/spillovers/"


#   read files

trips <- readRDS(trips.path)


# IV  - first stage ----------------------------------------------------------------------------

# spillovers
# mean = average duration of blocks and floods coinciding with spillover trips

iv <- felm(mean ~ spillovers:acc.rain + rain.bins1 + rain.bins2 + rain.bins3 
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# generate predicted values for spillovers 
trips$fitted.spill <- fitted(iv)

# save coefficients

iv.coef <- as.data.frame(summary(iv)$coefficients)


# output ----------------------------------------------------------------------------------------

saveRDS(iv.coef, coef.path)

stargazer(iv,  
          type = "latex",
          df = FALSE,
          title = "IV First Stage with Spillovers",
          dep.var.labels = c("Spillover Duration"),
          add.lines = list(c("Trip FE", "N"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          out = paste0(out.path, "iv-1ststage.tex"))
