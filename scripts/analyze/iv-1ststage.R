# clear workspace

rm(list = ls())
gc()

# set working directory

setwd("//141.142.208.117/share/projects/Congestion/")
source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

# trips.path <- "intermediate/floods/date-merge.rds"
trips.path <- "intermediate/floods/floods-model.rds"

#   output

out <- "documents/working paper/floods/tables/"

#   read files

trips <- readRDS(trips.path)

#   convert flood duration NA's to 0s

trips$duration1[which(is.na(trips$duration1))] <- 0
trips$duration1 <- as.numeric(as.character(trips$duration1))

trips$fduration1[which(is.na(trips$fduration1))] <- 0
trips$fduration1 <- as.numeric(as.character(trips$fduration1))


# IV  - first stage ----------------------------------------------------------------------------

# blocks

iv.1 <- felm(duration1 ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | ID_ORDEM + month + wd + hour.f, data = trips)

trips$fitted.blocks <- as.numeric(fitted(iv.1))
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

rm(iv.1)

# floods

iv.2 <- felm(fduration1 ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f, data = trips)

trips$fitted.floods <- as.numeric(fitted(iv.2))
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# generate table -------------------------------------------------------------------------------

stargazer(iv.1, iv.2, 
          align = TRUE,
          type = "latex",
          title = "IV 1st Stage",
          out = paste0(out, "iv-1ststage.tex"))

# save output ---------------------------------------------------------------------------------------

saveRDS(trips, trips.path)
