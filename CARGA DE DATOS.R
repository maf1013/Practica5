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
library(dplyr)
library(tidyr)

# Carga el archivo usando comillas como delimitador
cancer_estomago <- read_delim("INPUT/DATA/cancer_estomago.csv", delim = "\"", 
                              escape_backslash = TRUE, escape_double = FALSE, trim_ws = TRUE)

# Limpia filas con todos los valores NA
cancer_estomago <- cancer_estomago %>% filter(rowSums(is.na(.)) != ncol(.))

# Divide la columna de años en varias columnas nuevas
cancer_estomago <- cancer_estomago %>%
  separate(col = ",2005,2006,2007,2008,2009,2010,2011,2012,2013,", 
           into = paste0("Year_", 2005:2013), sep = ",")
cancer_estomago <- cancer_estomago %>% select(-`...1`, -`...3`, -`...5`, -`,...6`, -`...7`, -`...9`,
                                              -`,...10`, -`...11`, -`...13`, -`,...14`, -`...15`, -`...17`, -`...19`)
# Visualiza el resultado
View(cancer_estomago)




