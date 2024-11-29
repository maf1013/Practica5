
#CANCER DE ESTÓMAGO EN EUROPA

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

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
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") # se puede añadir: , .groups = "drop"  #Usamos summarise porque queremos cambiar el numero de filas original


View(cancer_estomago)


###### PREGUNTA 3: ¿El cáncer de estómago se ve influenciado por el sexo de los pacientes?
#Evaluación general de sexo frente a casos
cancer_estomago %>%
  group_by(Sexo) %>%
  summarise(Casos_promedio = mean(Casos, na.rm = TRUE))

#Evaluación del sexo frente al país
cancer_estomago %>%
  group_by(Sexo, Pais) %>%
  summarise(Casos_promedio = mean(Casos, na.rm = TRUE))


# Crear el gráfico con geom_bar
ggplot(cancer_estomago, aes(x = Año, y = Casos, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") + # Barras agrupadas
  labs(
    title = "Número de Casos de Cáncer de Estómago por Sexo y Año",
    x = "Año",
    y = "Número de Casos",
    fill = "Sexo"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas del eje X



###### 5. ¿Las tasas de cáncer de estómago han aumentado o disminuido con el tiempo?
tendencia_cancer <- cancer_estomago %>%
  group_by(Año) %>%
  summarise(Casos_promedio = mean(Casos, na.rm = TRUE))

ggplot(data = tendencia_cancer, aes(x = Año, y = Casos_promedio))+
  geom_point() +
  geom_smooth(method = "loess", formula = 'y ~ x' )

ggplot(data = cancer_estomago, aes(x = Año, y = Casos)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) + # Cambia el color y ajusta la transparencia
  geom_smooth(method = "loess", formula = y ~ x, color = "darkred", fill = "pink", alpha = 0.3) + # Personaliza la línea y el área sombreada
  labs(
    title = "Tendencia de casos promedio de cáncer",
    x = "Año",
    y = "Casos promedio",
    caption = "Fuente: Datos de tendencia_cancer"
  ) + 
  theme_minimal(base_size = 14)


#### CASO CON MÁS NUMERO DE CASOS DE CANCER
max_casos <- cancer_estomago %>%
  arrange(desc(Casos))%>%
  slice_head()
max_casos


#### CASO CON MENOS NUMERO DE CASOS DE CANCER
min_casos <- cancer_estomago %>%
  arrange(Casos)%>%
  slice_head()
min_casos
  
####################################################################################################

###################################################################################################
#DATOS DE FOSFATO EN RÍOS DE EUROPA
library(readr)
library(dplyr)
library(ggplot2)
Fosfato <- read_csv("INPUT/DATA/fosfato.csv") %>%
  select(-`Period:text`) %>%                                
  rename(País = `Country:text`,                           
         Año = `Year:year`,                                 
         Fosfato = colnames(.)[3]) %>%                      
  arrange(País)   # Ordenar País por orden alfabético
View(Fosfato)


#################################################################################################
#DATOS ECONOMÍA EUROPA
library(readr)
library(dplyr)
library(tidyverse)

economia <- read_csv("INPUT/DATA/economia.csv")

economia<-economia%>%
  select(-c(1, 2, 3, 4, 5,9))%>%   #se eliminan columnas innecesarias 
  slice(-c(178:189),-c(118:141))%>%
  rename(País=geo,
         Año=TIME_PERIOD,
         PIB=OBS_VALUE)
View(economia)






