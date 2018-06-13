# Run the main model with manually generated predictions
# N.B. the fitted() command in felm gives us predicted values without the FEs

source("intermediate/floods/environment.R")

#   loading packages

packages <- c("dplyr", "data.table", "lfe", "stargazer")
lapply(packages, pkgTest)

#   input

trips.path <- "intermediate/floods/floods-model.rds"

#   output

out.path <- "views/floods/"

#   read files

trips <- readRDS(trips.path)

#   IV second stage ----------------------------------------------------------------------------

# trip FE

iv.1 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM | 0 | ID_ORDEM, data = trips)

# trip + month FE

iv.2 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

# trip + month + day of week FE

iv.3 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)


# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


#   output -------------------------------------------------------------------------------------

# LaTeX table
stargazer(iv.1, iv.2, iv.3, iv.4, 
           type = "latex",
           digits = 6,
           df = FALSE,
           title = "IV Second Stage",
           add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                            c("Month FE", "N", "Y", "Y", "Y"),
                            c("Day of Week FE", "N", "N", "Y", "Y"),
                            c("Hour FE", "N", "N", "N", "Y")),
           notes = "Standard errors are clustered at the trip level.",
          dep.var.labels = "Trip Duration (minutes)")
          
#   IV second stage with spillovers ----------------------------------------------------------------------------

# trip FE

iv.1 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + spillovers:res.spill 
                       + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM | 0 | ID_ORDEM, data = trips)

# trip + month FE

iv.2 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + spillovers:res.spill
                       + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month | 0 | ID_ORDEM, data = trips)

# trip + month + day of week FE

iv.3 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + spillovers:res.spill
                       + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd | 0 | ID_ORDEM, data = trips)


# trip + month + day of week + time of day FE

iv.4 <- felm(tr.time ~ blocks:res.blocks + floods:res.floods + spillovers:res.spill
                       + rain.bins1 + rain.bins2 + rain.bins3
                       | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)


#   output -------------------------------------------------------------------------------------

# LaTeX table
stargazer(iv.1, iv.2, iv.3, iv.4, 
           type = "latex",
           digits = 6,
           df = FALSE,
           title = "IV Second Stage with Spillovers",
           add.lines = list(c("Trip FE", "Y", "Y", "Y", "Y"),
                            c("Month FE", "N", "Y", "Y", "Y"),
                            c("Day of Week FE", "N", "N", "Y", "Y"),
                            c("Hour FE", "N", "N", "N", "Y")),
           notes = "Standard errors are clustered at the trip level.",
          dep.var.labels = "Trip Duration (minutes)")
  
          
# second stage with peak hour indicators -------------------------------------------------------

# trip + month + day of week + time of day FE

iv <- felm(tr.time ~ blocks:res.blocks:early.peak + floods:res.floods:early.peak +
                     blocks:res.blocks:late.peak + floods:res.floods:late.peak +
                     blocks:res.blocks:off.peak + floods:res.floods:off.peak + 
                     rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# output ---------------------------------------------------------------------------------------


# generate LaTeX table 

stargazer(iv,
          type = "latex",
          digits = 6,
          df = FALSE,
          title = "IV Second Stage with Peak Hour Indicators",
          dep.var.labels = c("Trip Duration"),
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.")
          
# second stage with peak hour indicators and spillovers  -------------------------------------------------------

# trip + month + day of week + time of day FE

iv <- felm(tr.time ~ blocks:res.blocks:early.peak + floods:res.floods:early.peak + spillovers:res.spill:early.peak +
                     blocks:res.blocks:late.peak + floods:res.floods:late.peak + spillovers:res.spill:late.peak +
                     blocks:res.blocks:off.peak + floods:res.floods:off.peak + spillovers:res.spill:off.peak +
                     rain.bins1 + rain.bins2 + rain.bins3 | ID_ORDEM + month + wd + hour.f | 0 | ID_ORDEM, data = trips)

# output ---------------------------------------------------------------------------------------


# generate LaTeX table 

stargazer(iv,
          type = "latex",
          digits = 6,
          df = FALSE,
          title = "IV Second Stage with Peak Hour Indicators",
          dep.var.labels = c("Trip Duration"),
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.")
          
# second stage with traffic directions ---------------------------------------------------------------------------------

iv <- felm(tr.time ~ blocks:res.blocks:morning.traffic + floods:res.floods:morning.traffic +
                     blocks:res.blocks:morning.against + floods:res.floods:morning.against +
                     blocks:res.blocks:evening.traffic + floods:res.floods:evening.traffic +
                     blocks:res.blocks:evening.against + floods:res.floods:evening.against +
                     blocks:res.blocks:uncongested + floods:res.floods:uncongested +
                     rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd + hour.f, data = trips)


# LaTeX output
stargazer(iv,
          type = "latex",
          digits = 6, 
          dep.var.labels = c("Trip Duration"),
          df = FALSE,
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV Second Stage with Traffic Directions")
          
# second stage with traffic directions and spillovers ---------------------------------------------------------------------------------

iv <- felm(tr.time ~ blocks:res.blocks:morning.traffic + floods:res.floods:morning.traffic + spillovers:res.spill:morning.traffic +
                     blocks:res.blocks:morning.against + floods:res.floods:morning.against + spillovers:res.spill:morning.against +
                     blocks:res.blocks:evening.traffic + floods:res.floods:evening.traffic + spillovers:res.spill:evening.traffic +
                     blocks:res.blocks:evening.against + floods:res.floods:evening.against + spillovers:res.spill:evening.against +
                     blocks:res.blocks:uncongested + floods:res.floods:uncongested + spillovers:res.spill:uncongested +
                     rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd + hour.f, data = trips)


# LaTeX output
stargazer(iv,
          type = "latex",
          digits = 6, 
          dep.var.labels = c("Trip Duration"),
          df = FALSE,
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV Second Stage with Traffic Directions")

# second stage with trip duration bins ---------------------------------------------------------------------------------

iv <- felm(tr.time ~ blocks:res.blocks:short.trips + floods:res.floods:short.trips + 
                     blocks:res.blocks:med.trips + floods:res.floods:med.trips +    
                     blocks:res.blocks:long.trips + floods:res.floods:long.trips + 
                     rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd + hour.f, data = trips)


# LaTeX output
stargazer(iv,
          type = "latex",
          digits = 6, 
          dep.var.labels = c("Trip Duration"),
          df = FALSE,
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV Second Stage with Trip Duration Bins")

# second stage with trip duration bins and spillovers  ---------------------------------------------------------------------------------

iv <- felm(tr.time ~ blocks:res.blocks:short.trips + floods:res.floods:short.trips + spillovers:res.spill:short.trips +
                     blocks:res.blocks:med.trips + floods:res.floods:med.trips + spillovers:res.spill:med.trips +    
                     blocks:res.blocks:long.trips + floods:res.floods:long.trips + spillovers:res.spill:long.trips +
                     rain.bins1 + rain.bins2 + rain.bins3
                     | ID_ORDEM + month + wd + hour.f, data = trips)


# LaTeX output
stargazer(iv,
          type = "latex",
          digits = 6, 
          dep.var.labels = c("Trip Duration"),
          df = FALSE,
          add.lines = list(c("Trip FE", "Y"),
                           c("Month FE", "Y"),
                           c("Day of Week FE", "Y"),
                           c("Hour FE", "Y")),
          notes = "Standard errors are clustered at the trip level.",
          title = "IV Second Stage with Trip Duration Bins")

