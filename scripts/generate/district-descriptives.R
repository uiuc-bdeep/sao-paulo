# created on 05/31/2018

# clear workspace 

rm(list = ls())

# function for loading packages

# required packages

# inputs 

floods.path <- "intermediate/floods/floods.rds"
survey.path <- "intermediate/floods/survey-sub-2012.rds"
trips.path <- "intermediate/floods/floods-model.rds"

sub.path <- "stores/floods/LAYER_SUBPREFEITURAS_2013/"

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

floods <- group_by(floods, SUB)
floods1 <- summarise(floods, count = n())

# names of subprefectures

sub <- readOGR(sub.path, "DEINFO_SUBPREFEITURAS_2013")

# Assign CRS: SAD69 (cf. "deinfometadadossubprefeituras2013.csv")
proj4string(sub) <- CRS("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs ")

# Transform SAD69 to WGS84
sub <- spTransform(sub, CRS("+proj=longlat +ellps=WGS84"))

# Extract data 
sub <- as.data.frame(sub@data)

# Subset to names of districts

sub <- as.data.frame(sub$NOME)
names(sub)[names(sub) == "sub$NOME"] <- "NOME"

# Add abbreviation

sub$SUB <- ifelse(sub$NOME == "ARICANDUVA", "AF",
                     ifelse(sub$NOME == "BUTANTA", "BT",
                            ifelse(sub$NOME == "CAMPO LIMPO", "CL",
                                   ifelse(sub$NOME == "CAPELA DO SOCORRO", "CS",
                                          ifelse(sub$NOME == "CASA VERDE-CACHOEIRINHA", "CV",
                                                 ifelse(sub$NOME == "CIDADE ADEMAR", "AD",
                                                        ifelse(sub$NOME == "CIDADE TIRADENTES", "CT",
                                                               ifelse(sub$NOME == "ERMELINO MATARAZZO", "EM",
                                                                      ifelse(sub$NOME == "FREGUESIA-BRASILANDIA", "FO",
                                                                             ifelse(sub$NOME == "GUAIANAZES", "GU",
                                                                                    ifelse(sub$NOME == "IPIRANGA", "IP",
                                                                                           ifelse(sub$NOME == "ITAIM PAULISTA", "IT",
                                                                                                  ifelse(sub$NOME == "ITAQUERA", "IQ",
                                                                                                         ifelse(sub$NOME == "JABAQUARA", "JA",
                                                                                                                ifelse(sub$NOME == "JACANA-TREMEMBE", "JT",
                                                                                                                       ifelse(sub$NOME == "LAPA", "LA",
                                                                                                                              ifelse(sub$NOME == "M'BOI MIRIM", "MB",
                                                                                                                                     ifelse(sub$NOME == "MOOCA", "MO",
                                                                                                                                            ifelse(sub$NOME == "PARELHEIROS", "PA",
                                                                                                                                                   ifelse(sub$NOME == "PENHA", "PE",
                                                                                                                                                          ifelse(sub$NOME == "PERUS", "PR",
                                                                                                                                                                 ifelse(sub$NOME == "PINHEIROS", "PI",
                                                                                                                                                                        ifelse(sub$NOME == "PIRITUBA", "PJ",
                                                                                                                                                                               ifelse(sub$NOME == "SANTANA-TUCURUVI", "ST",
                                                                                                                                                                                      ifelse(sub$NOME == "SANTO AMARO", "SA",
                                                                                                                                                                                             ifelse(sub$NOME == "SAO MATEUS", "SM",
                                                                                                                                                                                                    ifelse(sub$NOME == "SAO MIGUEL", "MP",
                                                                                                                                                                                                           ifelse(sub$NOME == "SAPOPEMBA", "SP",
                                                                                                                                                                                                                  ifelse(sub$NOME == "SE", "SE",
                                                                                                                                                                                                                         ifelse(sub$NOME == "VILA MARIA-VILA GUILHERME", "MG",
                                                                                                                                                                                                                                ifelse(sub$NOME == "VILA MARIANA", "VM",
                                                                                                                                                                                                                                       ifelse(sub$NOME == "VILA PRUDENTE", "VP", NA))))))))))))))))))))))))))))))))





# Merge floods

sub <- merge(sub, floods1, by = "SUB", all.x = TRUE)

# replace NAs with 0s

sub$count[is.na(sub$count)] <- 0



# Define function for generating percentiles

pct <- ecdf(sub$count)

# Generate percentiles

sub$pct <- pct(sub$count)

# Sort according to percentiles (descending order)

sub <- sub[order(-sub$pct),]

# Output

sub1 <- sub[,c("NOME", "pct")]
xtable(sub1)




