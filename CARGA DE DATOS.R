#FOSFATO EN RIOS EUROPA XML
library(tidyverse)
library(XML)
library(xml2)
fosfato <- read_xml(x = "INPUT/DATA/fosfato_en_rios.xml")

fosfato_mxl <- xmlParse(fosfato)
fosfato_mxl
attributes(fosfato)

DF_fosfato <- xmlToDataFrame(doc = fosfato, stringsAsFactors = FALSE)
DF_fosfato
str(DF_fosfato)










#CANCER DE ESTÓMAGO EN EUROPA

library(readr)
cancer_estomago <- read_delim("INPUT/DATA/cancer_estomago.csv", 
                              delim = "\"", escape_backslash = TRUE, 
                              escape_double = FALSE, trim_ws = TRUE)
cancer_estomago$`2005` <- strsplit(as.character(datos$`2005`), ",")
cancer_estomago$`2006` <- strsplit(as.character(datos$`2006`), ",")
cancer_estomago$`2007` <- strsplit(as.character(datos$`2007`), ",")


datos_2005 <- do.call(rbind, cancer_estomago$`2005`)
datos_2006 <- do.call(rbind, cancer_estomago$`2006`)
datos_2007 <- do.call(rbind, cancer_estomago$`2007`)

# Convertir las matrices en data frames
datos_2005 <- as.data.frame(datos_2005)
datos_2006 <- as.data.frame(datos_2006)
datos_2007 <- as.data.frame(datos_2007)

# Asignar nombres a las nuevas columnas
colnames(datos_2005) <- paste("2005_", seq_along(datos_2005), sep = "")
colnames(datos_2006) <- paste("2006_", seq_along(datos_2006), sep = "")
colnames(datos_2007) <- paste("2007_", seq_along(datos_2007), sep = "")

# Unir los datos con las nuevas columnas separadas
cancer_estomago <- cbind(cancer_estomago, datos_2005, datos_2006, datos_2007)

# Ver los primeros datos con las nuevas columnas
head(cancer_estomago)







