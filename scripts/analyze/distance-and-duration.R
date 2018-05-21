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
		out.path <- "analysis/descriptives/survey 2012/distance and duration/distance and duration.csv"
    
  
# read and manipulate data from original household survey ------------------------------------

		OD <- readRDS(hhdata_path)
		
		TD <- OD[OD$TIPOVG %in% c(1,2,3,4),]
		
		TD$Time.bus <- TD$ext.crawler_ave.Time.pub
		TD$Time.walk <- TD$ext.crawler_ave.Time.walk
		TD$Time.car <- TD$ext.crawler_ave.Time.car
		
		out <- matrix(nrow = 22, ncol = 7, "")
	
		# Header -----------------------------------------------------------------------	
  out[1,3] <- "Distance (km)"
		out[1,4] <- "Distance (km)"
		out[1,6] <- "Duration (minutes)"
		out[1,7] <- "Duration (minutes)"
		
		
		out[2,3] <- "Mean"
		out[2,4] <- "Total"
		out[2,6] <- "Mean"
		out[2,7] <- "Total"
		
		
		out[3,4] <- "Millions"
		out[3,7] <- "Millions"
		
 	# Mode -----------------------------------------------------------------------	
		out[4,1] <- "Mode"
		
		TTD <- TD[TD$TIPOVG == 1,]
		out[5,2] <- "public"
		out[5,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[5,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[5,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[5,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		TTD <- TD[TD$TIPOVG == 2,]
		out[6,2] <- "private"
		out[6,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[6,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[6,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[6,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		TTD <- TD[TD$TIPOVG == 3,]
		out[7,2] <- "walking"
		out[7,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[7,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[7,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[7,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		# Purpose -----------------------------------------------------------------------	
		out[9,1] <- "Mode"
		
		TTD <- TD[TD$MOTIVO_D %in% c(1,2,3),]
		out[10,2] <- "work"
		out[10,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[10,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[10,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[10,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$MOTIVO_D %in% c(4),]
		out[11,2] <- "school"
		out[11,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[11,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[11,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[11,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$MOTIVO_D %in% c(5,6,7,9,10),]
		out[12,2] <- "other"
		out[12,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[12,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[12,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[12,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$MOTIVO_D %in% c(8),]
		out[13,2] <- "home"
		out[13,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[13,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[13,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[13,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		# Income Percentile ------------------------------------------------------------------	
		out[15,1] <- "Income percentile"
		
		TTD <- TD[TD$HH.IpC <= quantile(TD$HH.IpC, 0.2),]
		out[16,2] <- "20th ($ 0 - 231)"
		out[16,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[16,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[16,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[16,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$HH.IpC <= quantile(TD$HH.IpC, 0.4) & 
		          TD$HH.IpC > quantile(TD$HH.IpC, 0.2),]
		out[17,2] <- "40th ($ 231 - 349)"
		out[17,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[17,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[17,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[17,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$HH.IpC <= quantile(TD$HH.IpC, 0.6) & 
		          TD$HH.IpC > quantile(TD$HH.IpC, 0.4),]
		out[18,2] <- "60th ($ 349 - 530)"
		out[18,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[18,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[18,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[18,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$HH.IpC <= quantile(TD$HH.IpC, 0.8) & 
		          TD$HH.IpC > quantile(TD$HH.IpC, 0.6),]
		out[19,2] <- "80th ($ 530 - 903)"
		out[19,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[19,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[19,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[19,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		
		TTD <- TD[TD$HH.IpC > quantile(TD$HH.IpC, 0.8),]
		out[20,2] <- "100th ($ 903 - 13,019)"
		out[20,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[20,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[20,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[20,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
		# Total ------------------------------------------------------------------	
		
		TTD <- TD
		out[22,1] <- "Total"
		out[22,3] <- weighted.mean(x = TTD$DISTANCIA, w = TTD$FE_VIA, na.rm = T)/1000
		out[22,4] <- sum(x = TTD$DISTANCIA*TTD$FE_VIA, na.rm = T)/(10^9)
		out[22,6] <- weighted.mean(x = TTD$Time.bus, w = TTD$FE_VIA, na.rm = T)/60
		out[22,7] <- sum(x = TTD$Time.bus*TTD$FE_VIA, na.rm = T)/((10^6)*60)
		
	
		#write --------------------------------------------------------------------
		
		write.csv(out, out.path, row.names = F)
		
		
