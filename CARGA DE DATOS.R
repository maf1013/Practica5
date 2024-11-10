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
View(cancer_estomago)





