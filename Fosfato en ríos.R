
#OBJETIVO 1
#En primer lugar, vamos a estudiar la evolución de los fosfatos en ríos.
#Para ello se crea una nueva tabla por años que incluye el promedio de todos los países 

promedio_fosfato_por_año <- Fosfato %>%
  group_by(Año) %>%                    # Agrupa los datos por el año
  summarise(Promedio_Fosfato = mean(Cantidad_Fosfato, na.rm = TRUE)) # Calcula el promedio de fosfato para cada año
View(promedio_fosfato_por_año)


#En los resultados obtenidos podemos observar que desde 1992 a 2018 hay un descenso general en el
#nivel de fosfato en ríos. Se evoluciona de una concentración de 0,13 en 1992 a 0,08 en 2018. 
#Se alcanzó el menor nivel de fosfato en ríos en el año 2010.

#Generar gráfica para estudiar la evolución de los fosfatos en Europa 
grafico_fosfato<-ggplot(data=promedio_fosfato_por_año, aes(x=Año,y=Promedio_Fosfato))+
                 geom_point(na.rm=TRUE, colour="deeppink")+
                 geom_smooth(na.rm=TRUE)+
                 labs(title="Evolución de fosfato en ríos ",
                      x="Años",
                      y="Fosfato en ríos")+
                 theme_light()+
                 theme(axis.text.x = element_text(angle = 45, hjust = 1))
grafico_fosfato

#A continuación, se estudiará cuáles son los países con mayor cantidad de fosfato.
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

#PAÍS CON MÁS FOSFATO
pais_max <- Fosfato %>%
  arrange(desc(Cantidad_Fosfato))%>%
  slice_head()
pais_max
#PAÍS CON MENOS FOSFATO
pais_min <- Fosfato %>%
  arrange(Cantidad_Fosfato)%>%
  slice_head()
pais_min

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
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Países con menor concentración de fosfato en ríos",
    x = "Año",  
    y = "Concentración de Fosfato"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top" 
  )

#-----------------------------------------------------------------------------------

# Ahora, vamos a estudiar la posible relación de la economía con el nivel de fosfato en ríos
fosfato_economia <- Fosfato %>%
  left_join(economia, by = c("País", "Año")) %>% # Al hacer este join, se mantienen todos los años de datos de ECONOMÍA, pero en los datos de fosfato, no hay valores para algunos años
  filter(!is.na(PIB)) # Eliminamos las filas con NA en la columna PIB
View(fosfato_economia)

#Los datos que hemos conseguido de economía solo registran a partir del año 2012. Sabemos que 
#A partir de el año 2012 el país que menos fosfato registró fue Noruega. Vamos a observar qué ranking de 
#PIB ocupa noruega respecto del resto de países.

ranking_noruega<-fosfato_economia%>%
  group_by(Año) %>%
  mutate(ranking_pib = rank(-PIB, ties.method = "min")) %>%
  filter(País == "Norway") %>%
  arrange(Año) %>%
  select(Año, PIB, ranking_pib,Cantidad_Fosfato) 
View(ranking_noruega)

#El país que más fosfato registró a partir del año 2012 fue el Norte de Macedonia, vamos a estudiar
#qué ranking de PIB ocupa el Norte de Macedonia

ranking_macedonia<-fosfato_economia%>%
  group_by(Año) %>%
  # Crea el ranking para cada año
  mutate(ranking_pib = rank(-PIB, ties.method = "min")) %>%
  #se calcula el ranking para la columna PIB (-) en la que el mayor valor del PIB 
  #reciba el puesto número uno.Con ties.method="min" se asigna el mismo ranking a
  #dos valores empatados.
  filter(País == "North Macedonia") %>%
  arrange(Año) %>%
  select(Año, PIB, ranking_pib,Cantidad_Fosfato) 
View(ranking_macedonia)


#Grafico

datos_combinados <- bind_rows(
  ranking_noruega %>% mutate(Pais = "Noruega"),
  ranking_macedonia %>% mutate(Pais = "Macedonia")
)

datos_largo <- datos_combinados %>%
  pivot_longer(cols = c("Cantidad_Fosfato", "PIB"), 
               names_to = "Variable", 
               values_to = "Valor")

ggplot(datos_largo, aes(x = Año, y = Valor, color = Pais)) +
  geom_point(size=2) +
  geom_smooth(se = TRUE,alpha=0.15) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +
  labs(title = "Evolución fosfatos en ríos y \n PIB a lo largo del tiempo",
       x = "Año",
       y = "Valor") +
  scale_x_continuous(breaks = seq(min(datos_largo$Año), max(datos_largo$Año))) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

