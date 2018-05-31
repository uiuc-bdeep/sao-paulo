trips <- readRDS("intermediate/floods/floods-model.rds") 

trips$lag.rain.bins <- ifelse(trips$lag5 == 0, "0",
                            ifelse(trips$lag5 <= 2.5, "1",
                               ifelse(trips$lag5 <= 7.6, "2",
                                      ifelse(trips$lag5 > 7.6, "3", NA))))
                                      
trips$lag5 <- as.numeric(as.character(trips$lag5))
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
                               
floods <- felm(fduration.mean ~ floods:lag.bins1 + floods:lag.bins2 + floods:lag.bins3 
                                + rain.bins1 + rain.bins2 + rain.bins3 
                                | month + wd + hour.f | 0 | ID_ORDEM, data = trips)

spillovers <- felm(mean ~ spillovers:lag.bins1 + spillovers:lag.bins2 + spillovers:lag.bins3 
                          + rain.bins1 + rain.bins2 + rain.bins3 
                          | month + wd + hour.f | 0 | ID_ORDEM, data = trips)
                          


stargazer(blocks, floods, spillovers,
          df = FALSE,
          type = "latex",
          title = "IV 1st Stage")
