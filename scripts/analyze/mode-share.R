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
		out.path <- "analysis/descriptives/survey 2012/mode share/tables/mode share.csv"
    
  
# read and manipulate data from original household survey ------------------------------------

		OD <- readRDS(hhdata_path)
		TD <- OD[OD$TIPOVG %in% c(1,2,3,4),]
		
		out <- matrix(nrow = 20, ncol = 3, "")
		
		# mode
		mode.l <- c(1,2,3)
		line.l <- c(1,9,15)
		
		for(i in 1:length(mode.l)){
		 m <- mode.l[i]
		 l <- line.l[i]
		 out[l,1] <- nrow(TD[TD$TIPOVG == m,])
		 out[l,2] <- sum(TD$FE_VIA[TD$TIPOVG == m])
		 out[l,3] <- sum(TD$FE_VIA[TD$TIPOVG == m])/sum(TD$FE_VIA) 
		}   
	
		
		# submode
		mode.l <- list(c(1,2,3), c(9,10,11), 4,
		               5, 12, 13, 6, 7, 14, 8, 16,
		               15, 17)
		line.l <- c(2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 16, 17, 18)
		
		for(i in 1:length(line.l)){
		 
		 m <- mode.l[[i]]
		 l <- line.l[i]
		 out[l,1] <- nrow(TD[TD$MODOPRIN %in% m,])
		 out[l,2] <- sum(TD$FE_VIA[TD$MODOPRIN %in% m])
		 out[l,3] <- sum(TD$FE_VIA[TD$MODOPRIN %in% m])/sum(TD$FE_VIA) 
		}
		
		# total
		 out[20,1] <- nrow(TD)
		 out[20,2] <- sum(TD$FE_VIA)
		 out[20,3] <- 1
		 
		
		# write -------------------------------------------------------------------------------------
		
		write.csv(out, out.path, row.names = F)
		
		
