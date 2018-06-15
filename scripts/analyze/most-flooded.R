
# Looking at the effect sizes on the trips which occurred on the most flooded roads
# Created by: Amanda Ang

setwd("/home/bdeep/share/projects/Congestion")
source("intermediate/floods/environment.R")

packages <- c("lfe",
              "dplyr", 
              "data.table")
lapply(packages, pkgTest)

floods.path <- "intermediate/floods/floods.rds"
trips.path <- "intermediate/floods/date-merge.rds"

floods <- readRDS(floods.path)
floods <- as.data.table(floods)

trips <- readRDS(trips.path)
trips <- as.data.table(trips)

# subset to relevant variables

floods <- floods[,c("FID", "LOCAL")]

# merge the flood street names with trips 

trips <- merge(trips, floods, by = "FID", all.x = TRUE)
saveRDS(trips, trips.path)

# subset to the floods on streets with the most number of floods

floods <- group_by(floods, LOCAL)
floods1 <- summarise(floods, count = n())
floods1$fraction <- floods1$count / 740



