
#CANCER DE ESTÓMAGO EN EUROPA

library(readr)
library(dplyr)
library(tidyverse)

# Carga el archivo usando comillas como delimitador
cancer_estomago <- read_delim("INPUT/DATA/cancer_estomago.csv", delim = "\"", 
                              escape_backslash = TRUE, escape_double = FALSE, trim_ws = TRUE)

# Divide la columna de años en varias columnas nuevas
cancer_estomago <- 
  cancer_estomago %>%
  separate(
    col = c(`,2005,2006,2007,2008,2009,2010,2011,2012,2013,`),
    into = paste0("Año_", 2005:2013), 
    sep = ","
  ) %>% 
  select(-`Indicator,`, -`...1`, -`...3`, -`...5`, -`,...6`, -`...7`, -`...9`,
         -`,...10`, -`...11`, -`...13`, -`,...14`, -`...15`, -`...17`, -`...19`, -`Año_2005`,
         -`Cancer`,  -`Age Group`) %>% 
  slice(-c(247, 248, 249)) %>%
  rename(Pais_Ciudad = Registry)%>%
  rename(Sexo=Sex)%>%
  pivot_longer(cols = c(`Año_2006`:`Año_2013`), names_to = "Año", values_to = "Casos") 
  

cancer_estomago <- cancer_estomago %>%
  mutate(
    Pais = case_when(
      str_starts(Pais_Ciudad, "AT") ~ "Austria",
      str_starts(Pais_Ciudad, "BE") ~ "Belgica",
      str_starts(Pais_Ciudad, "BG") ~ "Bulgaria",
      str_starts(Pais_Ciudad, "CH") ~ "Suiza",
      str_starts(Pais_Ciudad, "DE") ~ "Alemania",
      str_starts(Pais_Ciudad, "DK") ~ "Dinamarca",
      str_starts(Pais_Ciudad, "EE") ~ "Estonia",
      str_starts(Pais_Ciudad, "ES") ~ "España",
      str_starts(Pais_Ciudad, "FR") ~ "Francia",
      str_starts(Pais_Ciudad, "HR") ~ "Croacia",
      str_starts(Pais_Ciudad, "IE") ~ "Irlanda",
      str_starts(Pais_Ciudad, "IS") ~ "Islandia",
      str_starts(Pais_Ciudad, "IT") ~ "Italia",
      str_starts(Pais_Ciudad, "LT") ~ "Lituania",
      str_starts(Pais_Ciudad, "MT") ~ "Malta",
      str_starts(Pais_Ciudad, "NL") ~ "Holanda",
      str_starts(Pais_Ciudad, "NO") ~ "Noruega",
      str_starts(Pais_Ciudad, "PL") ~ "Polonia",
      str_starts(Pais_Ciudad, "PT") ~ "Portugal",
      str_starts(Pais_Ciudad, "RO") ~ "Rumania",
      str_starts(Pais_Ciudad, "RS") ~ "Serbia",
      str_starts(Pais_Ciudad, "SI") ~ "Eslovenia",
      str_starts(Pais_Ciudad, "SK") ~ "Eslovaquia",
      str_starts(Pais_Ciudad, "UA") ~ "Ucrania",
      str_starts(Pais_Ciudad, "UK") ~ "Reino Unido")) %>%
  select(-Pais_Ciudad) %>%
  relocate(Pais, Año, Sexo, Casos)

 
cancer_estomago <- cancer_estomago %>%
  mutate(Casos = as.numeric(Casos))%>%
  group_by(Pais, Año, Sexo)%>%
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") # se puede añadir: , .groups = "drop"  #Usamos summarise porque queremos cambair el numero de filas original


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
  arrange(desc(fosfato))   # Ordenar por "fosfato" en orden descendente
 
# Visualizar los datos en el visor de RStudio
View(rivers_orthophoshate_3)




