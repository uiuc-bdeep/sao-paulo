# created on 05/31/2018

# clear workspace 

rm(list = ls())

# function for loading packages

# required packages

# inputs 

floods.path <- "intermediate/floods/floods.rds"
survey.path <- "intermediate/floods/survey-sub-2012.rds"
trips.path <- "intermediate/floods/floods-model.rds"

# read data 

floods <- readRDS(floods.path)
survey <- readRDS(survey.path)
trips <- readRDS(trips.path)

# merge districts from 2012 survey with crawled trips data

survey <- as.data.table(survey)
survey <- survey[,c("ID_ORDEM", "SUB")]

trips <- as.data.table(trips)
trips <- merge(trips, survey, by = "ID_ORDEM", all.x = TRUE)

# generate number of floods per district

floods <- group_by(floods, SUB, SITUACAO)
floods1 <- summarise(floods, count = n())



