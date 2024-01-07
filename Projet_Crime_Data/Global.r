# global.R
data <- read.csv("DATA/Crime_Data_from_2020_to_Present.csv", header = TRUE)
titre_appli = " analyse crime 2023"

variables_data = names(data)

# Conversion de la date
data$DATE.OCC <- as.Date(data$DATE.OCC, format = "%m/%d/%Y")

# Extraction du noms des mois
data$month <- lubridate::month(data$DATE.OCC, label = TRUE) 

# Extraction du numéro du mois
data$month_num <- lubridate::month(data$DATE.OCC)

# Extraction de l'année
data$year <- lubridate::year(data$DATE.OCC)

# Conversion de l'année en facteur
data$year <- as.factor(data$year)

# Conversion de la date et de l'heure en objet lubridate
data$datetime <- lubridate::force_tz(lubridate::ymd_hm(paste(data$year, data$month, data$day, data$hour, data$minute, sep = "-")), tz = "UTC")

