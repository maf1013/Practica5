
#CANCER DE ESTÓMAGO EN EUROPA

library(readr)
library(dplyr)
library(tidyr)

# Carga el archivo usando comillas como delimitador
cancer_estomago <- read_delim("INPUT/DATA/cancer_estomago.csv", delim = "\"", 
                              escape_backslash = TRUE, escape_double = FALSE, trim_ws = TRUE)

# Divide la columna de años en varias columnas nuevas
cancer_estomago <- cancer_estomago %>%
  separate(col = ",2005,2006,2007,2008,2009,2010,2011,2012,2013,", 
           into = paste0("Year_", 2005:2013), sep = ",")
cancer_estomago <- cancer_estomago %>% select(-`Indicator,`,-`...1`, -`...3`, -`...5`, -`,...6`, -`...7`, -`...9`,
                                              -`,...10`, -`...11`, -`...13`, -`,...14`,
                                              -`...15`, -`...17`, -`...19`)
cancer_estomago <- cancer_estomago %>% slice(-c(247, 248, 249))
# Visualiza el resultado
View(cancer_estomago)


#DATOS DE ORTOFOSFATO EN RÍOS DE EUROPA
library(readr)
rivers_orthophoshate_3 <- read_csv("C:/Users/USUARIO/Desktop/INFORMACION SEMINARIO FUENTES/rivers-orthophoshate-3.csv")
View(rivers_orthophoshate_3)



