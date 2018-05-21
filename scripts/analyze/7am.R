#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  Creates the data for the table distance and duration.csv                               |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Renato Schwambach Vieira                                                               |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

# Prelims -------------------------------------------------------------------------------------
  
  rm(list=ls())
  
  setwd("//141.142.208.117/share/projects/transit")
  source("analysis/environment.R")

  # Inputs files:
  hhdata_path <- "intermediate/sp12 master/master.rds"
    
  # Outputs files:
  # csv table
		out.path <- "analysis/descriptives/survey 2012/counterfactual travel time/7am medians/7am.csv"


# read data -----------------------------------------------------------------------------------

  HH <- readRDS(hhdata_path)
  
  # subset to trips
  HH <- HH[(HH$TIPOVG %in% c(1,2)),]
  
  HH$trip <- 1
  
  # keep only 7 am trips
  HH$dep.min <- ((HH$H_SAIDA)*60) + (HH$MIN_SAIDA)
  
  HH <- HH[HH$dep.min >=420 & HH$dep.min < 440,]
  
  HH$Time.pub <- HH$Time.pub*60
  HH$Time.car <- HH$Time.car*60
  HH$Time.walk <- HH$Time.walk*60
  
  
  # variables
  m <- matrix(nrow = 21, ncol = 7, 0)
  
  
  vars <- c("ext.crawler_ave.Time.car5_00", "", "ext.crawler_ave.Time.car7_00", "", 
            "Time.pub", "", "Time.walk")
  vars.c <- c("Cost.car", "",  "Cost.car", "", 
              "Cost.pub", "", "Cost.walk")
  
  for(i in seq(1,7,2)){
   
   m[i,1] <- median(HH[,vars[i]], na.rm = T)/60
   m[i,3] <- median(HH[,vars.c[i]], na.rm = T)
  
   m[i,5] <- quantile(HH[,vars[i]]/
                       HH[,vars[1]], na.rm = T, 0.1)
   m[i,6] <- quantile(HH[,vars[i]]/
                       HH[,vars[1]], na.rm = T, 0.5)
   m[i,7] <- quantile(HH[,vars[i]]/
                       HH[,vars[1]], na.rm = T, 0.9)
  }
  
   m[9,3] <- median(HH[,vars.c[1]], na.rm = T)
   
    vars <- c("ext.crawler_ave.Time.car5_00",
              "ext.crawler_ave.Time.car5_20",
              "ext.crawler_ave.Time.car5_40",
              "ext.crawler_ave.Time.car6_00",
              "ext.crawler_ave.Time.car6_20",
              "ext.crawler_ave.Time.car6_40",
              "ext.crawler_ave.Time.car7_00",
              "ext.crawler_ave.Time.car7_20",
              "ext.crawler_ave.Time.car7_40",
              "ext.crawler_ave.Time.car8_00",
              "ext.crawler_ave.Time.car8_20",
              "ext.crawler_ave.Time.car8_40")
   
 for(i in 10:21){
   
   m[i,1] <- median(HH[,vars[i-9]], na.rm = T)/60
   
   m[i,5] <- quantile(HH[,vars[i-9]]/
                       HH[,vars[1]], na.rm = T, 0.1)
   m[i,6] <- quantile(HH[,vars[i-9]]/
                       HH[,vars[1]], na.rm = T, 0.5)
   m[i,7] <- quantile(HH[,vars[i-9]]/
                       HH[,vars[1]], na.rm = T, 0.9)
  }
   
  write.csv(m, out.path, row.names = F)
  
  
 
