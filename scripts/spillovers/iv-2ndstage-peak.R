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

coef2.path <- "intermediate/floods/iv2-coef (peak).rds"
out.path <- "views/floods/spillovers/"

# read files -----------------------------------------------------------------------------------

trips <- readRDS(trips.path)

# second stage with peak hour indicators -------------------------------------------------------

# trip FE 
# cluster standard errors at trip level

iv.1 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM | 0 | ID_ORDEM, data = trips)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

# trip + month FE
# cluster standard errors at trip level

iv.2 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:early.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak| ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# trip + month + day of week FE
# cluster standard errors at trip level

iv.3 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)

iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + fitted.spill:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + fitted.spill:late.peak +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

# output ---------------------------------------------------------------------------------------

# save coefficients

coef2 <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef2, coef2.path)

# generate LaTeX table 

stargazer(iv.1, iv.2, iv.3, iv.4, 
          align = TRUE,
          type = "latex",
          df = FALSE,
          out = paste0(out, "iv (peak).tex"))
