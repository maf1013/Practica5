
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
#Pais con más cancer
maximo_fosfato<- Fosfato%>%
  group_by(Año) %>%  # Agrupar los datos por Año
  filter(Cantidad_Fosfato == max(Cantidad_Fosfato)) %>%
  arrange(Año)

View(maximo_fosfato)
#Y los países con menor cantidad de fosfato
minimo_fosfato<- Fosfato%>%
  group_by(Año) %>%  # Agrupar los datos por Año
  filter(Cantidad_Fosfato == min(Cantidad_Fosfato)) %>%
  arrange(Año)



#Se realiza un diagrama de barras con los paises que tienen más fosfato

ggplot(maximo_fosfato, aes(x = factor(Año), y = Cantidad_Fosfato)) +
  geom_bar(aes(fill = País),stat = "identity") + 
  labs( title = "Países con mayor concentración de fosfato en ríos",
        x = "País",
        y = "Concentración de Fosfato") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Se realiza otro diagrama de barras para tener en cuenta cuáles son los países con menos fosfato
ggplot(minimo_fosfato, aes(x = factor(Año), y = Cantidad_Fosfato, fill = País)) +
  geom_bar(stat = "identity", position = "dodge") +  # Usa 'dodge' para separar las barras
  labs(
    title = "Países con menor concentración de fosfato en ríos",
    x = "Año",  
    y = "Concentración de Fosfato"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # 90 grados para etiquetas en el eje X
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top"  # Ubicar la leyenda en la parte superior para que se vea con claridad
  )
