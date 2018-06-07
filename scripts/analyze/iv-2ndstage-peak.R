#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run delay on spillovers, trips  with peak hour effects                                 |
#   |  Run travel time on delay from blocks, floods                                           |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

rm(list=ls())

setwd("/home/bdeep/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

#   output

coef.path <- "intermediate/floods/iv2-coef (peak).rds"
out.path <- "views/floods/"

# read files -----------------------------------------------------------------------------------

trips <- readRDS(trips.path)

# second stage with peak hour indicators -------------------------------------------------------

# trip + month + day of week + time of day FE

iv <- felm(tr.time ~ blocks:fitted.blocks:early.peak + floods:fitted.floods:early.peak +
                     blocks:fitted.blocks:late.peak + floods:fitted.floods:late.peak +
                     blocks:fitted.blocks:off.peak + floods:fitted.floods:off.peak + 
                     rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

iv.coef <- as.data.frame(summary(iv)$coefficients)

# output ---------------------------------------------------------------------------------------

# save coefficients

saveRDS(iv.coef, coef.path)

# generate LaTeX table 

stargazer(iv,
          type = "latex",
          df = FALSE,
          title = "IV Second Stage with Peak Hour Indicators",
          dep.var.labels = c("Trip Duration"),
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          out = paste0(out.path, "iv (peak).tex"))


