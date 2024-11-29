

#En primer lugar, vamos a estudiar la evolución de los fosfatos en ríos.
#Para ello se crea una nueva tabla por años que incluye el promedio de todos los países 

promedio_fosfato_por_año <- Fosfato %>%
  group_by(Año) %>%                    # Agrupa los datos por el año
  summarise(promedio_fosfato = mean(Fosfato, na.rm = TRUE)) # Calcula el promedio de fosfato para cada año
View(promedio_fosfato_por_año)


#En los resultados obtenidos podemos observar que desde 1992 a 2018 hay un descenso general en el
#nivel de fosfato en ríos. Se evoluciona de una concentración de 0,13 en 1992 a 0,08 en 2018. 
#Se alcanzó el menor nivel de fosfato en ríos en el año 2010.

#Generar gráfica para estudiar la evolución de los fosfatos en Europa 
grafico_fosfato<-ggplot(data=promedio_fosfato_por_año, aes(x=Año,y=promedio_fosfato))+
                 geom_point(na.rm=TRUE, colour="deeppink")+
                 geom_smooth(na.rm=TRUE)+
                 labs(x="Años",
                      y="Fosfato en ríos",
                      subtitle="Evolución de fosfato en ríos "
                   
                 )
grafico_fosfato

#A continuación, se estudiará cuáles son los países con mayor cantidad de fosfato.
maximo_fosfato<- Fosfato%>%
  group_by(Año) %>%  # Agrupar los datos por Año
  filter(Fosfato == max(Fosfato)) %>%
  arrange(Año)

View(maximo_fosfato)


#Se realiza un diagrama de barras con los paises que tienen más fosfato
grafico_paises <- ggplot(maximo_fosfato, aes(x = País, y = Fosfato, fill = País)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(
    x = "País",
    y = "Concentración de Fosfato",
    title = "País con mayor concentración de fosfato"
  ) +
  theme_minimal()
grafico_paises


#Ahora, vamos a estudiar la posible relación de la economía con el nivel de fosfato en ríos
fosfato_ec<-left_join(x=economia,y=Fosfato,by= c("País","Año")) #Al hacer este join, se mantienen 
#todos los años de datos de ECONOMIA, pero en los datos de fosfato, no hay valores para algunos 
#años, para eliminar los valores de NA:
fosfato_ec <- fosfato_ec[!is.na(fosfato_ec$Fosfato), ]
View(fosfato_ec)

#como a mayor PIB, mayor es la econmia de ese pais vamos a estudiar si a menor PIB hay mayor 
#cantidad de fosfato en ríos.
menor_PIB<- fosfato_ec%>%
  group_by(Año)%>%
  filter(PIB == min(PIB)) %>%
  arrange(Año)
View(menor_PIB)

mayor_PIB<-fosfato_ec%>%
  group_by(Año)%>%
  filter(PIB == max(PIB)) %>%
  arrange(Año)
View(mayor_PIB)
  




