trips <- readRDS("intermediate/floods/floods-model.rds") 

                                      
trips$lag5 <- as.numeric(as.character(trips$lag5))
trips$lag5[is.na(trips$lag5)] <- 0

trips$lag.rain.bins <- ifelse(trips$lag5 == 0, "0",
                        ifelse(trips$lag5 <= 2.5, "1",
                               ifelse(trips$lag5 <= 7.6, "2",
                                      ifelse(trips$lag5 > 7.6, "3", NA))))

trips$lag.bins1 <- as.numeric(ifelse(trips$lag.rain.bins == "1", 1, 0))
trips$lag.bins2 <- as.numeric(ifelse(trips$lag.rain.bins == "2", 1, 0))
trips$lag.bins3 <- as.numeric(ifelse(trips$lag.rain.bins == "3", 1, 0))

blocks <- felm(duration.mean ~ blocks:lag.bins1 + blocks:lag.bins2 + blocks:lag.bins3 
                               + rain.bins1 + rain.bins2 + rain.bins3 
                               | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.blocks <- fitted(blocks)
                               
floods <- felm(fduration.mean ~ floods:lag.bins1 + floods:lag.bins2 + floods:lag.bins3 
                                + rain.bins1 + rain.bins2 + rain.bins3 
                                | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.floods <- fitted(floods)

spillovers <- felm(mean ~ spillovers:lag.bins1 + spillovers:lag.bins2 + spillovers:lag.bins3 
                          + rain.bins1 + rain.bins2 + rain.bins3 
                          | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

trips$fitted.spill <- fitted(spillovers)


stargazer(blocks, floods, spillovers,
          df = FALSE,
          type = "latex",
          title = "IV 1st Stage")

iv.1 <- felm(tr.time ~ fitted.blocks + fitted.floods 
                     + rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM | 0 | ID_ORDEM, data = trips)

iv1.coef <- as.data.frame(summary(iv.1)$coefficients)
iv1.coef$model <- "iv.1"

iv.2 <- felm(tr.time ~ fitted.blocks + fitted.floods 
                     + rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

iv2.coef <- as.data.frame(summary(iv.2)$coefficients)
iv2.coef$model <- "iv.2"

iv.3 <- felm(tr.time ~ fitted.blocks + fitted.floods 
                     + rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd  | 0 | ID_ORDEM, data = trips)

iv3.coef <- as.data.frame(summary(iv.3)$coefficients)
iv3.coef$model <- "iv.3"

iv.4 <- felm(tr.time ~ fitted.blocks + fitted.floods 
                     + rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

iv4.coef <- as.data.frame(summary(iv.4)$coefficients)
iv4.coef$model <- "iv.4"

coef <- rbind(iv1.coef, iv2.coef, iv3.coef, iv4.coef)
saveRDS(coef, coef.path)

# LaTeX table
stargazer(iv.1, iv.2, iv.3, iv.4, 
           type = "latex",
           df = FALSE,
           title = "IV Second Stage",
           covariate.labels = c("pr.Blocks",
                                "pr.Floods",
                                "Light Rain",
                                "Moderate Rain",
                                "Heavy Rain"),
           add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                            c("Month FE", "N", "Y", "Y", "Y"),
                            c("Day of Week FE", "N", "N", "Y", "Y"),
                            c("Hour FE", "N", "N", "N", "Y")),
           notes = "Standard errors are clustered at the trip level.",
          dep.var.labels = "Trip Duration (minutes)",
          out = paste0(out.path, "iv-secondstage(lags).tex"))
