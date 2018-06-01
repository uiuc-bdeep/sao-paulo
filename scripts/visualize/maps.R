#     ----------------------------------------------------------------------------------------
#   |                                                                                         |
#   |  plot maps of flood events                                                              |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Peter Christensen,  Amanda Ang                                                         |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#     ----------------------------------------------------------------------------------------

rm(list = ls())
setwd("~/share/projects/Congestion/")
source("intermediate/floods/environment.R")

packages <- c("readxl", 
              "data.table",
              "sp", 
              "rgdal", 
              "raster", 
              "rgeos", 
              "ggmap", 
              "mapproj", 
              "maps", 
              "ggplot2", 
              "mapdata")
lapply(packages, pkgTest)

# input

floods.path <- "intermediate/floods/floods.rds"
intersect.path <- "intermediate/floods/floods-intersect.rds"

routes.dsn <- "stores/floods/"
routes.path <- "routes"

sub.dsn <- "stores/floods/LAYER_SUBPREFEITURAS_2013"
sub.path <- "DEINFO_SUBPREFEITURAS_2013"

# output

out.path <- "views/floods/"

# read data 

floods <- readRDS(floods.path)

# ----------------------------------------------------------------------------------------------
names(floods)
floods$SITUACAO <- as.factor(floods$SITUACAO)
summary(floods$SITUACAO)

floods$long <- as.numeric(as.character(floods$long))
floods$lat <- as.numeric(as.character(floods$lat))

summary(floods$long)
summary(floods$lat)


# map 
SP <- c(long = -46.6, lat = -23.56)
SP.map <- get_map(location = SP,  maptype = "satellite", source = "google", zoom = 11)
SP_Test <- ggmap(SP.map)
SP_Test

SP_Test +
  geom_point(data = floods, aes(long, lat, color = SITUACAO), size = 0.3) +
  scale_color_discrete(breaks = c("intransitavel", "transitavel"), 
                       labels = c("Blocks", "Floods")) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank())

ggsave(paste0(out.path, "floods - map.png"), height = 4, width = 6, dpi = 300)

# ----------------------------------------------------------------------------------------------

# hot spot map

ggmap(SP.map, extent = "normal", maprange=FALSE) %+% floods + aes(x = long, y = lat) +
  geom_point(data = floods, aes(long, lat, color = SITUACAO), size = 1) +
  geom_density2d(size = 0.3, color = "white") +
  stat_density2d(aes(fill = ..level.., alpha = ..level..),
                 size = 0.07, bins = 8, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.2, 0.8), guide = FALSE) +
  coord_map(projection="mercator",
            xlim=c(-46.81, -46.39),
            ylim=c(-23.81, -23.39)) +
  theme(legend.position = "none", 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())


ggsave(paste0(out.path, "floods - hotspot map.png"), height = 4, width = 6, dpi = 300)

# ----------------------------------------------------------------------------------------------


# plot trips with those that intersect with floods and those that do not

# load dataset with survey trips intersected with flood events
intersect <- readRDS(intersect.path)

# subset to trips taken by car
intersect <- intersect[which(intersect$TIPOVG == 2),]

# subset variables
intersect <- intersect[,c("ID_ORDEM", "SITUACAO")]

# removing unused factor levels
intersect$SITUACAO <- droplevels.factor(intersect$SITUACAO)

# read routes shapefile
RD <- readOGR(dsn = routes.dsn, layer = routes.path)
RD <- spTransform(RD, CRS("+proj=longlat +ellps=WGS84"))

# merge flood type variable
RD@data <- merge(RD@data, intersect, by = "ID_ORDEM")

# subset to car trips
RD <- subset(RD, !is.na(RD@data$length))

# subset to only trips that intersect with blocks and floods
RD <- subset(RD, !is.na(RD@data$SITUACAO))

RD.points <- fortify(RD, region = "ID_ORDEM")
names(RD@data)[which(names(RD@data) == "ID_ORDEM")] <- "id"

RD.df <- join(RD.points, RD@data, by = "id")
RD.df <- subset(RD.df, !is.na(RD.df$SITUACAO))
names(RD.df)

SP <- c(long = -46.6, lat = -23.56)
SP.map <- get_map(location = SP,  maptype = "satellite", source = "google", zoom = 10)
SP_Test <- ggmap(SP.map)


SP_Test +
  geom_path(data = RD.df, aes(long, lat, group = group, color = SITUACAO), size = 0.4) +
  scale_color_discrete(breaks = c("intransitavel", "transitavel"), labels = c("Blocks", "Floods")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))

ggsave(paste0(out.path, "trips - map.png"), height = 4, width = 6, dpi = 300)

# ---------------------------------------------------------------------------------------------------

# plot district with the largest number of flood events


# load districts shapefile



