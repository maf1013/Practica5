
#CANCER DE ESTÓMAGO EN EUROPA

library(readr)
library(dplyr)
library(tidyr)

# Carga el archivo usando comillas como delimitador
cancer_estomago <- read_delim("INPUT/DATA/cancer_estomago.csv", delim = "\"", 
                              escape_backslash = TRUE, escape_double = FALSE, trim_ws = TRUE)

# Divide la columna de años en varias columnas nuevas
cancer_estomago <- cancer_estomago %>%
  separate(
    col = ",2005,2006,2007,2008,2009,2010,2011,2012,2013,", 
    into = paste0("Year_", 2005:2013), 
    sep = ","
  ) %>% 
  select(-`Indicator,`, -`...1`, -`...3`, -`...5`, -`,...6`, -`...7`, -`...9`,
         -`,...10`, -`...11`, -`...13`, -`,...14`, -`...15`, -`...17`, -`...19`, -`Year_2005`,
         -`Cancer`,  -`Age Group`) %>% 
  slice(-c(247, 248, 249)) %>%
  rename(País=Registry)%>%
  rename(Sexo=Sex)%>%
  rename(`Año 2006` = Year_2006) %>%
  rename(`Año 2007` = Year_2007) %>%
  rename(`Año 2008` = Year_2008) %>%
  rename(`Año 2009` = Year_2009) %>%
  rename(`Año 2010` = Year_2010) %>%
  rename(`Año 2011` = Year_2011) %>%
  rename(`Año 2012` = Year_2012)



View(cancer_estomago)


##################################################################################################

#DATOS FOSFATO EUROPA
library(readr)
library(stringr)

fosfato_rios<- read_csv("INPUT/DATA/eea_s_eu-sdg-06-50_p_2000-2021_v03_r00.csv")
fosfato_rios <- fosfato_rios %>%
  select(-c(1, 2, 3, 4, 5, 10)) %>%               # Elimina las columnas 1, 2, 3, 4, 5 y 10
  slice(-c(1:44)) %>%                             # Elimina las filas 1 a 44
  mutate(Registro = paste(geo, geo_label, sep = "  ")) %>%  # Crea una nueva columna "Registro"
  select(-geo, -geo_label) %>%                    # Elimina las columnas geo y geo_label
  relocate(Registro, .before = time) %>%          # Mueve la columna "Registro" antes de "time"
  pivot_wider(                                    # Convierte la tabla de formato largo a ancho
    names_from = "time", 
    values_from = "obs_value"
  ) %>%
  rename(País=Registro) %>%
  rename_with(~ str_replace(., "^\\d{4}$", "Año \\0"))
  


View(fosfato_rios)

###################################################################################################
#DATOS DE ORTOFOSFATO EN RÍOS DE EUROPA
library(readr)
library(dplyr)

# Cargar el archivo CSV y realizar todas las transformaciones en una sola tubería
rivers_orthophoshate_3 <- read_csv("C:/Users/USUARIO/Desktop/INFORMACION SEMINARIO FUENTES/rivers-orthophoshate-3.csv") %>%
  select(-`Period:text`) %>%                                
  rename(País = `Country:text`,                           
         Año = `Year:year`,                                 
         fosfato = colnames(.)[3]) %>%                      
  arrange(desc(fosfato))    # Ordenar por "fosfato" en orden descendente

# Visualizar los datos en el visor de RStudio
View(rivers_orthophoshate_3)




