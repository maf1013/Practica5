
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

