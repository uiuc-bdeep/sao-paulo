# created on 05/31/2018

# clear workspace 

rm(list = ls())

# function for loading packages

# required packages

# inputs 

floods.path <- "intermediate/floods/floods.rds"
trips.path <- "intermediate/floods/floods-model.rds"

# read data 

floods <- readRDS(floods.path)


