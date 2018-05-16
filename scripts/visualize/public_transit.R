# ------------------------------------------------------------------------------------ #
#                                                                                      #
#   Merge public transit times with crawled trips                                      #
#   Plot difference between public and private transit times                           #
#                                                                                      #
#   Created by: Amanda Ang                                                             #
#               Big Data Environmental Economics and Policy Group                      #
#   Date: 04/11/2018                                                                   #
# ------------------------------------------------------------------------------------ #

# Clear workspace

rm(list = ls())

# Set working directory

setwd("//141.142.208.117/share/projects/Congestion/")

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

# Output

out.path <- "views/floods/"

# ----------------------------------------------------------------------------------

# Read files

transit <- read.csv(transit.path, header = TRUE)
transit <- as.data.table(transit)

trips <- readRDS(trips.path)
trips <- as.data.table(trips)

# Predicted values 

iv.1 <- felm(duration.mean ~ blocks:rain.bins1 + blocks:rain.bins2 + blocks:rain.bins3 | month + wd + hour.f, data = trips)

iv.2 <- felm(fduration.mean ~ floods:rain.bins1 + floods:rain.bins2 + floods:rain.bins3 | month + wd + hour.f, data = trips)

trips$pr.blocks <- fitted(iv.1)
trips$pr.floods <- fitted(iv.2)

iv.3 <- felm(tr.time ~ pr.blocks + pr.floods | ID_ORDEM + month + wd + hour.f , data = trips)

coef <- as.data.frame(summary(iv.3)$coefficients)

trips$pr.time <- trips$tr.time * (1 + coef[[1]] + coef[[2]])

# Subset to relevant variables

transit <- transit[,c("ID_ORDEM",
                      "query_timestamp",
                      "google.transit_time")]

trips <- trips[,c("TID",
                  "ID_ORDEM",
                  "rain.date",
                  "wd",
                  "hour",
                  "tr.time",
                  "pr.time",
                  "blocks",
                  "floods")]

# Format date and time variables

transit$query_timestamp <- format(as.POSIXct(strptime(transit$query_timestamp,"%Y-%m-%d %H:%M",tz="")), format = "%Y-%m-%d %H:%M")

trips[,c("date", "time")] <- str_split_fixed(trips$rain.date, " ", 2)
transit[,c("date", "time")] <- str_split_fixed(transit$query_timestamp, " ", 2)

trips$date <- format(as.POSIXct(strptime(trips$date,"%Y-%m-%d",tz="")), format = "%Y-%m-%d")
trips$date <- as.Date(trips$date)

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

trips$residuals <- trips$pr.time - trips$tr.time

# Use 'density' function instead of geom_density to generate density of diff.time manually
# This ensures that the plotted density adheres to maximum and minimum values of the data 
# geom_density smoothes the densities such that the plot shows values below 0 when in fact there are none

blocks <- trips[which(trips$blocks == 1),]

ggplot(blocks) +
  ggtitle("Blocks",
          subtitle = "Difference in Public and Private Transit Times") +
  ylab("Density") +
  xlab("Difference in Transit Time \n (Minutes)") +
  geom_density(aes(diff.time, ..scaled..,
                   fill = "Google"), color = NA) + 
  geom_density(aes(residuals, ..scaled..,
                   fill = "Our Model"), color = NA) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave(paste0(out.path, "diff-blocks.png"),
       width = 8, height = 5, dpi = 300)

floods <- trips[which(trips$floods == 1),]

ggplot(floods) +
  ggtitle("Floods",
          subtitle = "Difference in Public and Private Transit Times") +
  ylab("Density") +
  xlab("Difference in Transit Time \n (Minutes)") +
  geom_density(aes(diff.time, ..scaled..,
                   fill = "Google"), color = NA) +
  geom_density(aes(residuals, ..scaled..,
                   fill = "Our Model"), color = NA) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave(paste0(out.path, "diff-floods.png"),
       width = 8, height = 5, dpi = 300)

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
names(summary)[which(names(summary) == "residuals")] <- "Estimated Added Travel Time From Floods"

stargazer(summary,
          type = "latex",
          summary = TRUE,
          out = paste0(out.path, "tr_time.tex"))

# Plot added travel time due to blocks / floods -------------------------------------------------


ggplot(trips) + 
  ggtitle("Whole Sample",
          subtitle = "Difference between public and private transit times") +
  geom_density(aes(diff.time, ..scaled..,
                   fill = "Google"), color = NA) +
  geom_density(aes(residuals, ..scaled..,
                   fill = "Our Model"), color = NA) +
  xlab("Minutes") +
  ylab("Density") +
  coord_cartesian(xlim = c(0, 100)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave(paste0(out.path, "whole-sample.png"), width = 8, height = 5, dpi = 300)

