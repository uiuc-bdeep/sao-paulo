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

out.path <- "views/floods/spillovers/"

#   read files

trips <- readRDS(trips.path)


# IV  - first stage ----------------------------------------------------------------------------

# blocks
# duration.mean = average block duration per trip (trips may encounter more than one block)

iv.1 <- felm(duration.mean ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# generate predicted values for blocks 
trips$fitted.blocks <- fitted(iv.1)

# floods
# fduration.mean = average flood duration per trip (trips may encounter more than one flood) 

iv.2 <- felm(fduration.mean ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# generate predicted values for floods
trips$fitted.floods <- fitted(iv.2)

# spillovers
# mean = average duration of blocks and floods per trip 

iv.3 <- felm(mean ~ spillovers:rain.bins1 + spillovers:rain.bins2 + spillovers:rain.bins3 | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# generate predicted values for spillovers 
trips$fitted.spill <- fitted(iv.3)

# output ----------------------------------------------------------------------------------------

stargazer(iv.1, iv.2, iv.3,  
          align = TRUE,
          type = "latex",
          df = FALSE,
          notes = "Standard errors are clustered at the trip level.",
          out = paste0(out.path, "iv-1ststage.tex"))
