
cancer_estomago_suma <- cancer_estomago %>%
  group_by(País, Año) %>%  # Agrupar solo por País y Año
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop")

cancer_fosfatos <- left_join(x = cancer_estomago_suma, y = Fosfato, by = c("Año", "País"))

View(cancer_fosfatos)

cancer_fosfatos_media <- cancer_fosfatos %>%
  group_by(Año) %>%
  summarise(Casos = mean(Casos, na.rm = TRUE),
            Cantidad_Fosfato = mean(Cantidad_Fosfato, na.rm = TRUE))


cancer_fosfato_long <- cancer_fosfatos_media %>%
  pivot_longer(cols = c(Casos, Cantidad_Fosfato),
               names_to = "Variable",
               values_to = "Valor")

ggplot(cancer_fosfato_long, aes(x = Año, y = Valor, color = Variable))+
  geom_point()+
  geom_smooth(se = TRUE)+
  facet_wrap(~ Variable, scales = "free_y", nrow = 2)+
  labs(title = "Relación de Cáncer de estómago y \n Fosfatos en Ríos a lo largo del tiempo",
       x = "Año",
       y = "Valor")+
  scale_x_continuous(breaks = seq(min(cancer_fosfato_long$Año), max(cancer_fosfato_long$Año)))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Crear el gráfico con dos facetas
ggplot(cancer_fosfato_media_long, aes(x = Año, y = Valor, color = Variable)) +
  geom_line() +  # Puedes usar geom_point() si prefieres puntos en lugar de líneas
  geom_smooth(se = FALSE) +  # Línea de tendencia (opcional)
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +  # Facetas separadas para cada variable
  labs(
    title = "Comparación de Casos de Cáncer y Cantidad de Fosfato",
    x = "Año",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Eliminar la leyenda si no la necesitas
