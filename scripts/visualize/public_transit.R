# ------------------------------------------------------------------------------------ #
#                                                                                      #
#   Merge public transit times with crawled trips                                      #
#   Plot difference between public and private transit times                           #
#                                                                                      #
#   Created by: Amanda Ang                                                             #
#               Big Data Environmental Economics and Policy Group                      #
#   Date: 04/11/2018                                                                   #
# ------------------------------------------------------------------------------------ #

# Edited on 05/25/2018 to include clustering standard errors and removing residuals from plot
# Converted density plot to histogram plot

# Edited on 05/27/2018 to use crawled trips for all modes of transportation instead of only car trips


# Clear workspace

rm(list = ls())

# Set working directory

setwd("/home/bdeep/share/projects/Congestion/")

# Function for loading packages

pkgTest <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}


# Load required packages

packages <- c("data.table",
              "dplyr",
              "stringr",
              "lfe",
              "lubridate",
              "ggplot2",
              "stringr",
              "stargazer")

lapply(packages, pkgTest)

# Inputs

# Crawled public transit times
transit.path <- "intermediate/floods/google trips.csv"

# Crawled private transit trips
trips.path <- "intermediate/floods/floods-model.rds"

# Coefficients from IV Second Stage
coef.path <- "intermediate/floods/iv2-coef.rds"

# Output

out.path <- "views/floods/"

# ----------------------------------------------------------------------------------

# Read files

transit <- read.csv(transit.path, header = TRUE)
transit <- as.data.table(transit)

trips <- readRDS(trips.path)
trips <- as.data.table(trips)

coef <- readRDS(coef.path)
coef <- coef[which(coef$model == "iv.4"),]

# generate predicted travel time using second stage coefficients

trips$pr.time <- trips$tr.time * (1 + coef[[1]] + coef[[2]])

# Subset to relevant variables

transit <- transit[,c("ID_ORDEM",
                      "query_timestamp",
                      "google.transit_time")]

trips <- trips[,c("TID",
                  "ID_ORDEM",
                  "wd",
                  "hour",
                  "tr.time",
                  "pr.time",
                  "blocks",
                  "floods")]

# Format date and time variables

transit$query_timestamp <- format(as.POSIXct(strptime(transit$query_timestamp,"%Y-%m-%d %H:%M",tz="")), format = "%Y-%m-%d %H:%M")

transit[,c("date", "time")] <- str_split_fixed(transit$query_timestamp, " ", 2)

transit$date <- format(as.POSIXct(strptime(transit$date,"%Y-%m-%d",tz="")), format = "%Y-%m-%d")
transit$date <- as.Date(transit$date)

transit$wd <- weekdays(transit$date)
transit$hour <- hour(transit$query_timestamp)

# Merge transit time

trips$ID_ORDEM <- as.character(trips$ID_ORDEM)
transit$ID_ORDEM <- as.character(transit$ID_ORDEM)

trips <- merge(trips, transit,
               by = c("ID_ORDEM", "wd", "hour"),
               all.x = TRUE)

# Exclude NAs

trips <- trips[!is.na(trips$google.transit_time),]

# Compare public and private transit times

trips$pub_tr.time <- trips$google.transit_time / 60
trips$diff.time <- trips$pub_tr.time - trips$tr.time

# create LaTeX table for summary statistics -----------------------------------------------------------------

trips$b.diff.time <- ifelse(trips$blocks == 1, trips$diff.time, NA)
trips$nob.diff.time <- ifelse(trips$blocks == 0, trips$diff.time, NA)

trips$f.diff.time <- ifelse(trips$floods == 1, trips$diff.time, NA)
trips$nof.diff.time <- ifelse(trips$floods == 0, trips$diff.time, NA)

summary <- trips[,c("b.diff.time",
                    "nob.diff.time",
                    "f.diff.time",
                    "nof.diff.time",
                    "diff.time",
                    "residuals")]

names(summary)[which(names(summary) == "b.diff.time")] <- "Blocks"
names(summary)[which(names(summary) == "nob.diff.time")] <- "No Blocks"

names(summary)[which(names(summary) == "f.diff.time")] <- "Floods"
names(summary)[which(names(summary) == "nof.diff.time")] <- "No Floods"

names(summary)[which(names(summary) == "diff.time")] <- "Whole Sample"

stargazer(summary,
          type = "latex",
          summary = TRUE,
          out = paste0(out.path, "tr_time.tex"))

