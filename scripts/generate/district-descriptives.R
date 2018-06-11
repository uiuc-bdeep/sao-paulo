# created on 05/31/2018

# clear workspace 

rm(list = ls())

# function for loading packages

# required packages

# inputs 

floods.path <- "intermediate/floods/floods.rds"
sub.path <- "stores/floods/LAYER_SUBPREFEITURAS_2013"

# read data 

floods <- readRDS(floods.path)

# generate number of floods per district

floods <- group_by(floods, SUB)
floods1 <- summarise(floods, count = n())

# names of subprefectures

sub <- readOGR(dsn = sub.path, layer = "DEINFO_SUBPREFEITURAS_2013")

# Transform SAD69 to WGS84
sub <- spTransform(sub, CRS("+proj=longlat +ellps=WGS84"))

# Extract data 
sub.df <- as.data.frame(sub@data)

# Subset to names of districts

sub.df <- as.data.frame(sub$NOME)
names(sub.df)[names(sub.df) == "sub$NOME"] <- "NOME"

# Add abbreviation

sub.df$SUB <- ifelse(sub.df$NOME == "ARICANDUVA", "AF",
                     ifelse(sub.df$NOME == "BUTANTA", "BT",
                            ifelse(sub.df$NOME == "CAMPO LIMPO", "CL",
                                   ifelse(sub.df$NOME == "CAPELA DO SOCORRO", "CS",
                                          ifelse(sub.df$NOME == "CASA VERDE-CACHOEIRINHA", "CV",
                                                 ifelse(sub.df$NOME == "CIDADE ADEMAR", "AD",
                                                        ifelse(sub.df$NOME == "CIDADE TIRADENTES", "CT",
                                                               ifelse(sub.df$NOME == "ERMELINO MATARAZZO", "EM",
                                                                      ifelse(sub.df$NOME == "FREGUESIA-BRASILANDIA", "FO",
                                                                             ifelse(sub.df$NOME == "GUAIANAZES", "GU",
                                                                                    ifelse(sub.df$NOME == "IPIRANGA", "IP",
                                                                                           ifelse(sub.df$NOME == "ITAIM PAULISTA", "IT",
                                                                                                  ifelse(sub.df$NOME == "ITAQUERA", "IQ",
                                                                                                         ifelse(sub.df$NOME == "JABAQUARA", "JA",
                                                                                                                ifelse(sub.df$NOME == "JACANA-TREMEMBE", "JT",
                                                                                                                       ifelse(sub.df$NOME == "LAPA", "LA",
                                                                                                                              ifelse(sub.df$NOME == "M'BOI MIRIM", "MB",
                                                                                                                                     ifelse(sub.df$NOME == "MOOCA", "MO",
                                                                                                                                            ifelse(sub.df$NOME == "PARELHEIROS", "PA",
                                                                                                                                                   ifelse(sub$NOME == "PENHA", "PE",
                                                                                                                                                          ifelse(sub.df$NOME == "PERUS", "PR",
                                                                                                                                                                 ifelse(sub.df$NOME == "PINHEIROS", "PI",
                                                                                                                                                                        ifelse(sub.df$NOME == "PIRITUBA", "PJ",
                                                                                                                                                                               ifelse(sub.df$NOME == "SANTANA-TUCURUVI", "ST",
                                                                                                                                                                                      ifelse(sub.df$NOME == "SANTO AMARO", "SA",
                                                                                                                                                                                             ifelse(sub.df$NOME == "SAO MATEUS", "SM",
                                                                                                                                                                                                    ifelse(sub.df$NOME == "SAO MIGUEL", "MP",
                                                                                                                                                                                                           ifelse(sub.df$NOME == "SAPOPEMBA", "SP",
                                                                                                                                                                                                                  ifelse(sub.df$NOME == "SE", "SE",
                                                                                                                                                                                                                         ifelse(sub.df$NOME == "VILA MARIA-VILA GUILHERME", "MG",
                                                                                                                                                                                                                                ifelse(sub.df$NOME == "VILA MARIANA", "VM",
                                                                                                                                                                                                                                       ifelse(sub.df$NOME == "VILA PRUDENTE", "VP", NA))))))))))))))))))))))))))))))))





# Merge floods

sub.df <- merge(sub.df, floods1, by = "SUB", all.x = TRUE)

# replace NAs with 0s

sub.df$count[is.na(sub.df$count)] <- 0

# Fraction of flood events per district

sub.df$fraction <- sub.df$count / 740

# Sort according to percentiles (descending order)

sub.df <- sub.df[order(-sub.df$fraction),]

# Output

sub1 <- sub.df[,c("NOME", "fraction")]
xtable(sub1)

# Plot 

plot.data <- ggplot2::fortify(sub, region = "NOME")

plot.data$NOME <- factor(plot.data$id)
plot.data$id <- NULL

plot.data <- merge(plot.data, sub1, by = "NOME", all.x = TRUE)

ggplot(plot.data, aes(x = long, y = lat, group = group, fill = fraction)) +
    ggtitle("Fraction of Flood Events Per District") +
    viridis::scale_fill_viridis(direction = -1, begin = 0.1, end = 0.5) +
    geom_polygon() +
    coord_equal() +
    theme_void() +
    theme(legend.position = "bottom",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank())

ggsave(paste0(out.path, "floods-frac.png"), height = 5, width = 8, dpi = 300)
       
       
