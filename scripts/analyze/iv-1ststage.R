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

out.path <- "views/floods/"

#   read files

trips <- readRDS(trips.path)

#   convert flood duration NA's to 0s

trips$duration1[which(is.na(trips$duration1))] <- 0
trips$duration1 <- as.numeric(as.character(trips$duration1))

trips$fduration1[which(is.na(trips$fduration1))] <- 0
trips$fduration1 <- as.numeric(as.character(trips$fduration1))


# IV  - first stage ----------------------------------------------------------------------------

# blocks

iv.1 <- felm(duration.mean ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 + rain
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# floods

iv.2 <- felm(fduration.mean ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 + rain
             | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# output ----------------------------------------------------------------------------------------

stargazer(iv.1, iv.2, 
          align = TRUE,
          type = "latex",
          title = "IV 1st Stage",
          out = paste0(out.path, "iv-1ststage.tex"))
