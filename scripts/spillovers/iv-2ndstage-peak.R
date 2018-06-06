#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run delay on spillovers, trips  with peak hour effects and spillovers                  |
#   |  Run travel time on delay from blocks, floods                                           |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Notes: 

# Edited on 05/29/2018

rm(list=ls())

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

#   output

coef2.path <- "intermediate/floods/iv2-coef-peak (spillovers).rds"
out.path <- "views/floods/spillovers/"

# read files -----------------------------------------------------------------------------------

trips <- readRDS(trips.path)

# second stage with peak hour indicators -------------------------------------------------------

# trip + month + day of week + time of day FE

iv <- felm(tr.time ~ fitted.spill:early.peak + fitted.spill:late.peak + fitted.spill:off.peak 
             | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

iv.coef <- as.data.frame(summary(iv)$coefficients)


# output ---------------------------------------------------------------------------------------

# save coefficients

saveRDS(iv.coef, coef2.path)

# generate LaTeX table 

stargazer(iv, 
          type = "latex",
          title = "IV Second Stage with Spillovers and Peak Hour Indicators", 
          dep.var.labels = c("Trip Duration"),
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          df = FALSE,
          out = paste0(out.path, "iv (peak).tex"))
