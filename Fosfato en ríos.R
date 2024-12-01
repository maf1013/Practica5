
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
  labs( title = "País con mayor concentración de fosfato en ríos",
        x = "País",
        y = "Concentración de Fosfato") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------------------------------------------------------

#Ahora, vamos a estudiar la posible relación de la economía con el nivel de fosfato en ríos
fosfato_economia<-left_join(x=economia,y=Fosfato,by= c("País","Año")) #Al hacer este join, se mantienen 
#todos los años de datos de ECONOMIA, pero en los datos de fosfato, no hay valores para algunos 
#años, para eliminar los valores de NA:
fosfato_economia <- fosfato_economia[!is.na(fosfato_economia$Cantidad_Fosfato), ]
View(fosfato_economia)

#como a mayor PIB, mayor es la economia de ese pais vamos a estudiar si a menor PIB hay mayor 
#cantidad de fosfato en ríos.
menor_PIB<- fosfato_economia%>%
  group_by(Año)%>%
  filter(PIB == min(PIB)) %>%
  arrange(Año)
View(menor_PIB)

fosfato_economia%>%
  group_by(Año)%>%
  filter(PIB == min(PIB)) %>%
  arrange(Año)



mayor_PIB<-fosfato_economia%>%
  group_by(Año)%>%
  filter(PIB == max(PIB)) %>%
  arrange(Año)
View(mayor_PIB)





fosfato_economia_media <- fosfato_economia %>%
  group_by(Año) %>%
  summarise(Cantidad_Fosfato = mean(Cantidad_Fosfato, na.rm = TRUE),
            PIB = mean(PIB, na.rm = TRUE))


# Transformar los datos a formato largo para tener una columna común que distinga entre Fosfato y PIB
fosfato_economia_long <- fosfato_economia_media %>%
  pivot_longer(cols = c("Cantidad_Fosfato", "PIB"), 
               names_to = "Variable", 
               values_to = "Valor")


ggplot(fosfato_economia_long, aes(x = Año, y = Valor, color = Variable))+
  geom_point()+
  geom_smooth(se = TRUE)+
  facet_wrap(~ Variable, scales = "free_y", nrow = 2)+
  labs(title = "Evolución fosfatos en ríos y \n PIB a lo largo del tiempo",
       x = "Año",
       y = "Valor")+
  scale_x_continuous(breaks = seq(min(fosfato_economia_long$Año), max(fosfato_economia_long$Año)))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



