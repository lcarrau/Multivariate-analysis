library(readr)
library(dplyr)
library(gmodels) # para tabla de contingencia
library(FactoMineR)
library(factoextra)
library(rcompanion) # para comparar las asociaciones

# Cargar los datos
base_pisa_brasil_peru_limpia <- read_csv("data/base_pisa_brasil_peru_limpia.csv")

base_cat <- base_pisa_brasil_peru_limpia %>% 
  select(
    pais, genero
  )

summary(base_cat)

# Tabla de frecuencias absolutas
tabla_freq <- base_cat %>%
  count(genero, pais) %>%
  tidyr::pivot_wider(names_from = pais, values_from = n, values_fill = 0)

tabla_freq

CrossTable(base_cat$genero, base_cat$pais,
           prop.r = TRUE,   # porcentaje por fila
           prop.c = FALSE,  # porcentaje por columna
           prop.t = FALSE,  # porcentaje sobre el total
           prop.chisq = FALSE,
           dnn = c("Género", "País"))

# Categorizar una variable continua con justificación teórica
# Por ejemplo, punt_finan (el puntaje de alfabetización financiera) se puede 
# discretizar en niveles (bajo/medio/alto) usando los puntos de corte de PISA, 
# y ahí sí tener una tercera variable categórica genuina para un ACM real:

base_3 <- base_pisa_brasil_peru_limpia %>%
  mutate(
    nivel_finan = cut(
      punt_finan,
      breaks = c(-Inf, 326, 400, 475, 550, 625, Inf),
      labels = c("Por debajo del Nivel 1", "Nivel 1", "Nivel 2",
                 "Nivel 3", "Nivel 4", "Nivel 5"),
      right = FALSE
    )
  )

table(base_3$nivel_finan, useNA = "ifany")

# ACM con las tres variables

vars_acm <- base_3 %>%
  select(genero, pais, nivel_finan) %>%
  mutate(across(everything(), as.factor))

res_mca <- MCA(vars_acm, graph = FALSE)
summary(res_mca)

# Nube de categorías (dónde se ubica cada nivel de cada variable en el plano)
fviz_mca_var(res_mca, repel = TRUE,
             col.var = "cos2",
             gradient.cols = c("#4C72B0", "#DD8452", "#D64550")) +
  labs(title = "ACM: Género, País y Nivel de Alfabetización Financiera")

# Varianza explicada por cada dimensión
fviz_screeplot(res_mca, addlabels = TRUE) +
  labs(title = "Varianza explicada por dimensión — ACM")

# Matriz de Burt
Z <- res_mca$call$Xtot
Z <- as.matrix(Z)
Burt <- t(Z) %*% Z
Burt

# Siguiendo el desarrollo del problema 3 
res_mca$eig

# Gráfico de los autovalores
eig <- res_mca$eig[,1] 
df <- data.frame( Dim = factor(1:length(eig)), Inercia = eig) 
ggplot(df, aes(x = Dim, y = Inercia)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(Inercia,1))), vjust = -0.5, size = 3) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.8)) +
  xlab("Dimensiones") + 
  ylab("Autovalores")

# Coordenadas
coord <- round(res_mca$var$coord[, 1:2], 3)

# Contribuciones absolutas
contrib_01 <- round(res_mca$var$contrib[, 1:2] / 100, 3)
abs_contri <- round(contrib_01[, 1:2],3)

# Contribuciones relativas
rel_contri <- round(res_mca$var$cos2[, 1:2], 3)
suma_rela <- rowSums(rel_contri)

# Tabla conjunta
print(tabla <- data.frame(
  Coord_Dim1 = coord[, 1],
  Coord_Dim2 = coord[, 2],
  Abs_Dim1 = abs_contri[, 1],
  Abs_Dim2 = abs_contri[, 2],
  Rel_Dim1 = rel_contri[, 1],
  Rel_Dim2 = rel_contri[, 2],
  Suma_rel = suma_rela))

# Gráfico de las variables en el plano factorial. 
p <- fviz_mca_var(res_mca, repel = TRUE, # evita que se superpongan las etiquetas 
                  col.var = "blue", 
                  label = "all", # muestra todas las etiquetas y el % de var. explicada de cada eje 
                  title = " Proyección de las variables sobre los dos primeros ejes principales del ACM ") 
p + theme(plot.title = element_text(hjust = 0.5))

# # Gráfico de las variables y pacientes en el plano factorial. 
# fviz_mca_biplot(res_mca, repel = TRUE, ggtheme = theme_minimal(), 
#                 label = "var", geom.ind = "point", col.ind = "forestgreen", alpha.ind = 0.5 ) + 
#   theme(plot.title = element_blank())

# Tests de independencia previos
tabla_gp <- table(base_3$genero, base_3$pais)
chisq.test(tabla_gp)

tabla_gn <- table(base_3$genero, base_3$nivel_finan)
chisq.test(tabla_gn)

tabla_pn <- table(base_3$pais, base_3$nivel_finan)
chisq.test(tabla_pn)

cramerV(tabla_gp)
cramerV(tabla_gn)
cramerV(tabla_pn)
