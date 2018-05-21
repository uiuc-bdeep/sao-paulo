
# clear workspace 

rm(list = ls())
gc()

# set working directory

setwd("~/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/date-merge.rds"
floods.path <- "intermediate/floods/floods.rds"

#   output

coef.path <- "intermediate/floods/coef.rds"

#   read files

trips <- readRDS(trips.path)
floods <- readRDS(floods.path)

# trip FE --------------------------------------------------------------------------------------

m1 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM, data = trips)
m1.coef <- as.data.frame(summary(m1)$coefficients)
m1.coef$model <- "m1"
rm(m1)

# trip + month FE ------------------------------------------------------------------------------

m2 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month, data = trips)
m2.coef <- as.data.frame(summary(m2)$coefficients)
m2.coef$model <- "m2"
rm(m2)

# trip + month + day of week FE ----------------------------------------------------------------

m3 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd, data = trips)
m3.coef <- as.data.frame(summary(m3)$coefficients)
m3.coef$model <- "m3"
rm(m3)

# trip + month + day of week + time of day FE --------------------------------------------------

m4 <- felm(ln_tr.time ~ blocks + floods + spillovers | ID_ORDEM + month + wd + hour.f, data = trips)
m4.coef <- as.data.frame(summary(m4)$coefficients)
m4.coef$model <- "m4"
rm(m4)

