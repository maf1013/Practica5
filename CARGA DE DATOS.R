
#CANCER DE ESTÓMAGO EN EUROPA

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Se carga el archivo usando comillas como delimitador
cancer_estomago <- read_delim("INPUT/DATA/cancer_estomago.csv", delim = "\"", 
                              escape_backslash = TRUE, escape_double = FALSE, trim_ws = TRUE) %>%
  
  # Separar la columna con los años (2005-2013) en columnas individuales para cada año
  # La columna contiene los valores de varios años, por lo que usamos separate() para dividirla
  separate(
    col = c(`,2005,2006,2007,2008,2009,2010,2011,2012,2013,`), #La columna que contiene los años
    #into = paste0("Año_", 2005:2013), #Nombres de las nuevas columnas (Año_2005, Año_2006, ..., Año_2013)
    into = as.character(2005:2013),
    sep = "," #Delimitador para separar los valores
  ) %>%
  
  # Elimina columnas innecesarias
  select(-`Indicator,`, -`...1`, -`...3`, -`...5`, -`,...6`, -`...7`, -`...9`,
         -`,...10`, -`...11`, -`...13`, -`,...14`, -`...15`, -`...17`, -`...19`, -`2005`,
         -`Cancer`,  -`Age Group`) %>%
  
  # Elimina filas que están llenas de valores nulos
  slice(-c(247, 248, 249)) %>%

  # Se renombran las columnas
  rename(Pais_Ciudad = Registry) %>%
  rename(Sexo = Sex) %>%
  
  # Convertir columnas anchas en formato largo para poder trabajar mejor los datos próximamente
  pivot_longer(cols = c(`2006`:`2013`), names_to = "Año", values_to = "Casos") %>%
  
  mutate(Año = as.numeric(Año)) %>%
  
  # Se crea una nueva columna País basada en los prefijos de Pais_Ciudad. 
  mutate(
    País = case_when(
      str_starts(Pais_Ciudad, "AT") ~ "Austria",
      str_starts(Pais_Ciudad, "BE") ~ "Belgium",
      str_starts(Pais_Ciudad, "BG") ~ "Bulgaria",
      str_starts(Pais_Ciudad, "CH") ~ "Switzerland",
      str_starts(Pais_Ciudad, "DE") ~ "Germany",
      str_starts(Pais_Ciudad, "DK") ~ "Denmark",
      str_starts(Pais_Ciudad, "EE") ~ "Estonia",
      str_starts(Pais_Ciudad, "ES") ~ "Spain",
      str_starts(Pais_Ciudad, "FR") ~ "France",
      str_starts(Pais_Ciudad, "HR") ~ "Croatia",
      str_starts(Pais_Ciudad, "IE") ~ "Ireland",
      str_starts(Pais_Ciudad, "IS") ~ "Iceland",
      str_starts(Pais_Ciudad, "IT") ~ "Italy",
      str_starts(Pais_Ciudad, "LT") ~ "Lithuania",
      str_starts(Pais_Ciudad, "MT") ~ "Malta",
      str_starts(Pais_Ciudad, "NL") ~ "Holland",
      str_starts(Pais_Ciudad, "NO") ~ "Norway",
      str_starts(Pais_Ciudad, "PL") ~ "Poland",
      str_starts(Pais_Ciudad, "PT") ~ "Portugal",
      str_starts(Pais_Ciudad, "RO") ~ "Romania",
      str_starts(Pais_Ciudad, "RS") ~ "Serbia",
      str_starts(Pais_Ciudad, "SI") ~ "Slovenia",
      str_starts(Pais_Ciudad, "SK") ~ "Slovakia",
      str_starts(Pais_Ciudad, "UA") ~ "Ukraine",
      str_starts(Pais_Ciudad, "UK") ~ "United Kingdom"
    )
  ) %>%
  
  # Después de haber creado la nueva columna País, se elimina País_Ciudad porque ya no nos interesa
  select(-Pais_Ciudad) %>%
  relocate(País, Año, Sexo, Casos) %>%
  #Convertir los valores de la columna Casos a numéricos
  mutate(Casos = as.numeric(Casos)) %>%
  
  # Agrupar los datos por "País", "Año" y "Sexo", y luego calcular la suma de los casos por grupo
  group_by(País, Año, Sexo) %>%
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") 
  #Con .groups=drop se consigue que se devuelvan los resultados sin estar agrupados por las variables País, Año y Sexo
  #Esto es necesario para conseguir que los datos sean la suma de un país entero y no aparezcan 
  #agrupados por regiones.

View(cancer_estomago)


###################################################################################################
#DATOS DE FOSFATO EN RÍOS DE EUROPA
library(readr)
library(dplyr)
library(ggplot2)
Fosfato <- read_csv("INPUT/DATA/fosfato.csv") %>%
  select(-`Period:text`) %>%  #Se eliminina una columna que estaba llena de texto innecesario                           
  rename(País = `Country:text`,                           
         Año = `Year:year`,         #Se renombran las columnas               
         Cantidad_Fosfato = colnames(.)[3]) %>%   #El nombre de la tercera columna mg P/I:number se sustituye por Fosfato   
  arrange(País, Año)   # Ordena la tabla por orden alfabético de País
View(Fosfato)

###############################################################################################
#DATOS ECONOMÍA EUROPA
library(readr)
library(dplyr)
library(tidyverse)

economia <- read_csv("INPUT/DATA/economia.csv")%>%
  select(-c(1, 2, 3, 4, 5,9))%>%   #se eliminan columnas innecesarias 
  slice(-c(178:189),-c(118:141))%>% #Se eliminan las filas innecesarias
  rename(País=geo,
         Año=TIME_PERIOD,   #Se renombran las columnas 
         PIB=OBS_VALUE)

View(economia)






