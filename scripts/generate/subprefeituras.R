#   ----------------------------------------------------------------------------------------- #
#   |                                                                                         |
#   |  Generate variables for matching based on subprefecture                                 |
#   |                                                                                         |
#   |  By:                                                                                    |
#   |  Amanda Ang                                                                             |
#   |  Big Data for Environmental Economics and Policy                                        |
#   |  University of Illinois at Urbana Chamapaign                                            |
#   |                                                                                         |
#   ----------------------------------------------------------------------------------------- #

# Set WD

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

packages <- c("sp", 
              "maptools", 
              "rgeos", 
              "rgdal", 
              "lubridate",
              "dplyr",
              "stringr",
              "ggplot2",
              "ggmap")

lapply(packages, pkgTest)


# Inputs

# 2007 Mobility Survey

survey07.path <- "data/OD2007_v3.csv"

# 2012 Mobility Survey
survey12.path <- "data/mobilidade_2012_google_info.csv"

# Subprefeituras shapefile
sub.path <- "data/LAYER_SUBPREFEITURAS_2013"

# 2007 Floods
floods07.path <- "data/2007-floods.rds"

# 2012 Floods
floods12.path <- "data/2012-floods.rds"

# Output

out.path <- "data/survey-sub.rds"

# ----------------------------------------------------------------------------------------------

# Read files

survey07 <- read.csv(survey07.path, header = TRUE)
survey12 <- read.csv(survey12.path, header = TRUE)

sub <- readOGR(sub.path, "DEINFO_SUBPREFEITURAS_2013")

floods12 <- readRDS(floods12.path)
floods07 <- readRDS(floods07.path)

# Assign CRS: SAD69 (cf. "deinfometadadossubprefeituras2013.csv")
proj4string(sub) <- CRS("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs ")

# Transform SAD69 to WGS84
sub <- spTransform(sub, CRS("+proj=longlat +ellps=WGS84"))

# Subset to required variables

survey07 <- survey07[,c("ID_ORDEM",
                    "DATA",
                    "CO_DOM_X",
                    "CO_DOM_Y")]

survey12 <- survey12[,c("ID_ORDEM",
                        "DATA",
                        "CO_DOM_X",
                        "CO_DOM_Y")]


# Remove NAs

survey07 <- survey07[which(!is.na(survey07$CO_DOM_X)),]
survey12 <- survey12[which(!is.na(survey12$CO_DOM_X)),]

# Generate coordinate vector for individual's place of residence

survey07$CO_DOM_X <- as.numeric(as.character(survey07$CO_DOM_X))
survey07$CO_DOM_Y <- as.numeric(as.character(survey07$CO_DOM_Y))
coords07 <- cbind(survey07$CO_DOM_X, survey07$CO_DOM_Y)

survey12$CO_DOM_X <- as.numeric(as.character(survey12$CO_DOM_X))
survey12$CO_DOM_Y <- as.numeric(as.character(survey12$CO_DOM_Y))
coords12 <- cbind(survey12$CO_DOM_X, survey12$CO_DOM_Y)

# Create SpatialPointsDataFrame

# Individual's Place of Residence

dom07.spdf <- SpatialPointsDataFrame(coords07, survey07,
                                   proj4string = CRS("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs "))
dom07.spdf <- spTransform(dom07.spdf, CRS("+proj=longlat +ellps=WGS84"))

dom12.spdf <- SpatialPointsDataFrame(coords12, survey12,
                                     proj4string = CRS("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs "))
dom12.spdf <- spTransform(dom12.spdf, CRS("+proj=longlat +ellps=WGS84"))


# Save as Shapefile

writeOGR(dom07.spdf, 
         sub.path, 
         layer = "dom_2007", 
         driver = "ESRI Shapefile", 
         overwrite_layer = TRUE )


writeOGR(dom12.spdf, 
         sub.path, 
         layer = "dom_2012", 
         driver = "ESRI Shapefile", 
         overwrite_layer = TRUE )

# Spatial Intersect

intersect07 <- over(dom07.spdf, sub)
survey07 <- cbind(survey07, intersect07)

survey07 <- survey07[,c("ID_ORDEM",
                    "DATA",
                    "NOME")]


intersect12 <- over(dom12.spdf, sub)
survey12 <- cbind(survey12, intersect12)

survey12 <- survey12[,c("ID_ORDEM",
                        "DATA",
                        "NOME")]

# ----------------------------------------------------------------------------------------------

# Match subprefeituras names to initials

survey07$SUB <- ifelse(survey07$NOME == "ARICANDUVA", "AF",
                     ifelse(survey07$NOME == "BUTANTA", "BT",
                            ifelse(survey07$NOME == "CAMPO LIMPO", "CL",
                                   ifelse(survey07$NOME == "CAPELA DO SOCORRO", "CS",
                                          ifelse(survey07$NOME == "CASA VERDE-CACHOEIRINHA", "CV",
                                                 ifelse(survey07$NOME == "CIDADE ADEMAR", "AD",
                                                        ifelse(survey07$NOME == "CIDADE TIRADENTES", "CT",
                                                               ifelse(survey07$NOME == "ERMELINO MATARAZZO", "EM",
                                                                      ifelse(survey07$NOME == "FREGUESIA-BRASILANDIA", "FO",
                                                                             ifelse(survey07$NOME == "GUAIANAZES", "GU",
                                                                                    ifelse(survey07$NOME == "IPIRANGA", "IP",
                                                                                           ifelse(survey07$NOME == "ITAIM PAULISTA", "IT",
                                                                                                  ifelse(survey07$NOME == "ITAQUERA", "IQ",
                                                                                                         ifelse(survey07$NOME == "JABAQUARA", "JA",
                                                                                                                ifelse(survey07$NOME == "JACANA-TREMEMBE", "JT",
                                                                                                                       ifelse(survey07$NOME == "LAPA", "LA",
                                                                                                                              ifelse(survey07$NOME == "M'BOI MIRIM", "MB",
                                                                                                                                     ifelse(survey07$NOME == "MOOCA", "MO",
                                                                                                                                            ifelse(survey07$NOME == "PARELHEIROS", "PA",
                                                                                                                                                   ifelse(survey07$NOME == "PENHA", "PE",
                                                                                                                                                          ifelse(survey07$NOME == "PERUS", "PR",
                                                                                                                                                                 ifelse(survey07$NOME == "PINHEIROS", "PI",
                                                                                                                                                                        ifelse(survey07$NOME == "PIRITUBA", "PJ",
                                                                                                                                                                               ifelse(survey07$NOME == "SANTANA-TUCURUVI", "ST",
                                                                                                                                                                                      ifelse(survey07$NOME == "SANTO AMARO", "SA",
                                                                                                                                                                                             ifelse(survey07$NOME == "SAO MATEUS", "SM",
                                                                                                                                                                                                    ifelse(survey07$NOME == "SAO MIGUEL", "MP",
                                                                                                                                                                                                           ifelse(survey07$NOME == "SAPOPEMBA", "SP",
                                                                                                                                                                                                                  ifelse(survey07$NOME == "SE", "SE",
                                                                                                                                                                                                                         ifelse(survey07$NOME == "VILA MARIA-VILA GUILHERME", "MG",
                                                                                                                                                                                                                                ifelse(survey07$NOME == "VILA MARIANA", "VM",
                                                                                                                                                                                                                                       ifelse(survey07$NOME == "VILA PRUDENTE", "VP", NA))))))))))))))))))))))))))))))))


survey12$SUB <- ifelse(survey12$NOME == "ARICANDUVA", "AF",
                     ifelse(survey12$NOME == "BUTANTA", "BT",
                            ifelse(survey12$NOME == "CAMPO LIMPO", "CL",
                                   ifelse(survey12$NOME == "CAPELA DO SOCORRO", "CS",
                                          ifelse(survey12$NOME == "CASA VERDE-CACHOEIRINHA", "CV",
                                                 ifelse(survey12$NOME == "CIDADE ADEMAR", "AD",
                                                        ifelse(survey12$NOME == "CIDADE TIRADENTES", "CT",
                                                               ifelse(survey12$NOME == "ERMELINO MATARAZZO", "EM",
                                                                      ifelse(survey12$NOME == "FREGUESIA-BRASILANDIA", "FO",
                                                                             ifelse(survey12$NOME == "GUAIANAZES", "GU",
                                                                                    ifelse(survey12$NOME == "IPIRANGA", "IP",
                                                                                           ifelse(survey12$NOME == "ITAIM PAULISTA", "IT",
                                                                                                  ifelse(survey12$NOME == "ITAQUERA", "IQ",
                                                                                                         ifelse(survey12$NOME == "JABAQUARA", "JA",
                                                                                                                ifelse(survey12$NOME == "JACANA-TREMEMBE", "JT",
                                                                                                                       ifelse(survey12$NOME == "LAPA", "LA",
                                                                                                                              ifelse(survey12$NOME == "M'BOI MIRIM", "MB",
                                                                                                                                     ifelse(survey12$NOME == "MOOCA", "MO",
                                                                                                                                            ifelse(survey12$NOME == "PARELHEIROS", "PA",
                                                                                                                                                   ifelse(survey12$NOME == "PENHA", "PE",
                                                                                                                                                          ifelse(survey12$NOME == "PERUS", "PR",
                                                                                                                                                                 ifelse(survey12$NOME == "PINHEIROS", "PI",
                                                                                                                                                                        ifelse(survey12$NOME == "PIRITUBA", "PJ",
                                                                                                                                                                               ifelse(survey12$NOME == "SANTANA-TUCURUVI", "ST",
                                                                                                                                                                                      ifelse(survey12$NOME == "SANTO AMARO", "SA",
                                                                                                                                                                                             ifelse(survey12$NOME == "SAO MATEUS", "SM",
                                                                                                                                                                                                    ifelse(survey12$NOME == "SAO MIGUEL", "MP",
                                                                                                                                                                                                           ifelse(survey12$NOME == "SAPOPEMBA", "SP",
                                                                                                                                                                                                                  ifelse(survey12$NOME == "SE", "SE",
                                                                                                                                                                                                                         ifelse(survey12$NOME == "VILA MARIA-VILA GUILHERME", "MG",
                                                                                                                                                                                                                                ifelse(survey12$NOME == "VILA MARIANA", "VM",
                                                                                                                                                                                                                                       ifelse(survey12$NOME == "VILA PRUDENTE", "VP", NA))))))))))))))))))))))))))))))))


# Format date variable 

survey12$DATA <- format(as.POSIXct(strptime(survey12$DATA,"%m/%d/%Y",tz="")), format = "%Y-%m-%d")
survey12$DATA <- as.Date(survey12$DATA)

# Create variable for the day before 

survey07$DATA1 <- as.Date(ymd(survey07$DATA) - days(1))
survey12$DATA1 <- as.Date(ymd(survey12$DATA) - days(1))

# Create dataset with number of floods per neighborhood per day --------------------------------

floods07 <- group_by(floods07, DATA, SITUACAO, SUB)
floods12 <- group_by(floods12, DATA, SITUACAO, SUB)

floods07_1 <- summarise(floods07, count = n())
floods12_1 <- summarise(floods12, count = n())

blocks_per_day_2007 <- floods07_1[which(floods07_1$SITUACAO == "intransitavel"),]
names(blocks_per_day_2007)[which(names(blocks_per_day_2007) == "count")] <- "blocks_count"
blocks_per_day_2007$SITUACAO <- NULL

blocks_per_day_2012 <- floods07_1[which(floods12_1$SITUACAO == "intransitavel"),]
names(blocks_per_day_2012)[which(names(blocks_per_day_2012) == "count")] <- "blocks_count"
blocks_per_day_2012$SITUACAO <- NULL


floods_per_day_2007 <- floods07_1[which(floods07_1$SITUACAO == "transitavel"),]
names(floods_per_day_2007)[which(names(floods_per_day_2007) == "count")] <- "floods_count"
floods_per_day_2007$SITUACAO <- NULL

floods_per_day_2012 <- floods12_1[which(floods12_1$SITUACAO == "transitavel"),]
names(floods_per_day_2012)[which(names(floods_per_day_2012) == "count")] <- "floods_count"
floods_per_day_2012$SITUACAO <- NULL


# Merge

survey07 <- merge(survey07, blocks_per_day_2007,
                by.x = c("DATA1", "SUB"), by.y = c("DATA", "SUB"),
                all.x = TRUE)

survey07 <- merge(survey07, floods_per_day_2007,
                by.x = c("DATA1", "SUB"), by.y = c("DATA", "SUB"),
                all.x = TRUE)

survey12 <- merge(survey12, blocks_per_day_2012,
                  by.x = c("DATA1", "SUB"), by.y = c("DATA", "SUB"),
                  all.x = TRUE)

survey12 <- merge(survey12, floods_per_day_2012,
                  by.x = c("DATA1", "SUB"), by.y = c("DATA", "SUB"),
                  all.x = TRUE)


# Save output ----------------------------------------------------------------------------------

saveRDS(survey07, "data/survey-sub-2007.rds")
saveRDS(survey12, "data/survey-sub-2012.rds")
