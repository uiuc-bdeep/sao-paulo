#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Run reduced form model  (large dataset)                                                |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# archived on 05/22/2018

#   prelims ------------------------------------------------------------------------------------
rm(list = ls())
gc()

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

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM | 0 | ID_ORDEM, data = trips)

# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)

# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd + hour.f| 0 | ID_ORDEM, data = trips)

# output ---------------------------------------------------------------------------------------

stargazer(m1, m2, m3, m4, align = TRUE)

# peak hour ------------------------------------------------------------------------------------

# creating indicator variables for peak travel times
# Sao Paulo peak hours: 7 - 10 am and 5 - 8 pm as determined by driving restrictions

# reduced form model with peak hour interactions -----------------------------------------------

m5 <- felm(ln_tr.time ~ blocks:early.peak + floods:early.peak + spillovers:early.peak +
                        blocks:late.peak + floods:late.peak + spillovers:late.peak +
                        blocks:not.peak + floods:not.peak + spillovers:not.peak | ID_ORDEM + month + wd + hour.f| 0 | ID_ORDEM, 
                        data = trips)

# output ---------------------------------------------------------------------------------------
stargazer(m5, align = TRUE)



