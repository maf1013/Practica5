#DATOS DE ORTOFOSFATO EN RÍOS DE EUROPA
library(readr)
library(dplyr)
library(ggplot2)
# Cargar el archivo CSV y realizar todas las transformaciones en una sola tubería
Fosfato <- read_csv("C:/Users/USUARIO/Desktop/INFORMACION SEMINARIO FUENTES/rivers-orthophoshate-3.csv") %>%
  select(-`Period:text`) %>%                                
  rename(País = `Country:text`,                           
         Año = `Year:year`,                                 
         Fosfato = colnames(.)[3]) %>%                      
  arrange(País)   # Ordenar País por orden alfabético
View(Fosfato)

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