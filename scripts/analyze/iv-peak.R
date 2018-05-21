
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

trips.path <- "intermediate/floods/date-merge.rds"

#   output

coef.path <- "intermediate/floods/iv2-coef (peak).rds"
out <- "view/floods/"

# read files -----------------------------------------------------------------------------------

trips <- readRDS(trips.path)

# creating indicators for peak hours ------------------------------------------------------------

# based on hours defined for rodizo in the city of Sao Paulo

trips$early.peak <- as.numeric(ifelse(trips$hour >= 7 & 
                                        trips$hour < 11, 1, 0))
summary(trips$early.peak)


trips$late.peak <- as.numeric(ifelse(trips$hour >= 17 &
                                       trips$hour < 21, 1, 0))
summary(trips$late.peak)

trips$off.peak <- as.numeric(ifelse(trips$early.peak == 0 & trips$late.peak == 0, 1, 0))
summary(trips$off.peak)

# second stage ---------------------------------------------------------------------------------

# trip FE

iv.1 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + 
                       fitted.blocks:off.peak + fitted.floods:off.peak | ID_ORDEM , data = trips)
iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

rm(iv.1)

# trip + month FE

iv.2 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak  +
                       fitted.blocks:late.peak + fitted.floods:late.peak  +
                       fitted.blocks:off.peak + fitted.floods:off.peak + fitted.spill:off.peak | ID_ORDEM + month, data = trips)
iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

rm(iv.2)

# trip + month + day of week FE

iv.3 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak +
                       fitted.blocks:late.peak + fitted.floods:late.peak + 
                       fitted.blocks:off.peak + fitted.floods:off.peak | ID_ORDEM + month + wd , data = trips)
iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

rm(iv.3)

# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ fitted.blocks:early.peak + fitted.floods:early.peak + 
                       fitted.blocks:late.peak + fitted.floods:late.peak + 
                       fitted.blocks:off.peak + fitted.floods:off.peak  | ID_ORDEM + month + wd + hour.f , data = trips)
iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

rm(iv.4)

# output -----------------------------------------------------------------------------------------

# save coefficients

coef <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef, coef.path)

# LaTeX output
stargazer(iv.1, iv.2, iv.3, iv.4, 
          style = "latex",
          align = TRUE,
          out = paste0(out, "iv-peak.tex"))






