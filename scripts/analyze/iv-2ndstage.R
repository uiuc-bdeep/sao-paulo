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

trips.path <- "intermediate/floods/floods-model.rds"

#   output

out <- "views/floods/"

#   read files

trips <- readRDS(trips.path)
names(trips)

#   IV second stage ----------------------------------------------------------------------------

# trip FE

iv.1 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM , data = trips)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

# trip + month FE

iv.2 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM + month, data = trips)

iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

# trip + month + day of week FE

iv.3 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM + month + wd, data = trips)

iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ fitted.blocks + fitted.floods | ID_ORDEM + month + wd + hour.f , data = trips)

iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"


# getting predicted values manually ------------------------------------------------------------



#   output -------------------------------------------------------------------------------------

stargazer(iv.1, iv.2, iv.3, iv.4, 
          align = TRUE,
          type = "latex",
          title = "IV Second Stage",
          covariate.labels = c("Blocks",
                               "Floods"),
          add.lines = list(c("Trip FE", "Yes", "Yes", "Yes", "Yes"),
                           c("Month FE", "No", "Yes", "Yes", "Yes"),
                           c("Day of Week FE", "No", "No", "Yes", "Yes"),
                           c("Hour FE", "No", "No", "No", "Yes")),
          dep.var.labels = "ln(Trip Duration)",
          out = paste0(out, "iv-secondstage.tex"))
