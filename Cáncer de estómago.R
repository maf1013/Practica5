
###### PREGUNTA 3: ¿El cáncer de estómago se ve influenciado por el sexo de los pacientes?
#Evaluación general de sexo frente a casos
cancer_estomago %>%
  group_by(Sexo) %>%
  summarise(Casos_promedio = mean(Casos, na.rm = TRUE))

#Evaluación del sexo frente al país
cancer_estomago %>%
  group_by(Sexo, País) %>%
  summarise(Casos_promedio = mean(Casos, na.rm = TRUE))


# Crear el gráfico con geom_bar
ggplot(cancer_estomago, aes(x= Año, y = Casos))+
  geom_bar(aes(fill = Sexo), position = "dodge", stat = "identity") +   
  #A stat le damos el valor identity, ya que queremos que use los valores de Casos, no que cuente las ocurrencias de esta variable.
  labs( title ="Número de casos de Cáncer de \n Estómago por Sexo y Año",
        x = "Año",
        y = "Número de casos",
        fill = "Sexo") +
  scale_x_continuous(breaks = seq(min(cancer_estomago$Año), max(cancer_estomago$Año))) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###### 5. ¿Las tasas de cáncer de estómago han aumentado o disminuido con el tiempo?
tendencia_cancer <- cancer_estomago %>%
  group_by(Año) %>%
  summarise(Casos_promedio = mean(Casos, na.rm = TRUE))
tendencia_cancer

ggplot(cancer_estomago, aes(x = factor(Año), y = Casos))+
  geom_bar(fill = "turquoise",stat = "identity") +
  labs( title = "Número de casos de Cáncer de Estómago por Año",
        x = "Año",
        y = "Casos")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### CASO CON MÁS NUMERO DE CASOS DE CANCER
max_casos <- cancer_estomago %>%
  arrange(desc(Casos))%>%
  slice_head()
max_casos


#### CASO CON MENOS NUMERO DE CASOS DE CANCER
min_casos <- cancer_estomago %>%
  filter(Casos > 0)%>%
  arrange(Casos)%>%
  slice_head()
min_casos

####################################################################################################
