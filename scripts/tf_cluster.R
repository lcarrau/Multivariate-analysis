library(readr)
library(dplyr)
library(factoextra)
library(cluster)
library(purrr)
library(tidyr)
library(ggplot2)


# Cargar los datos
base_pisa_brasil_peru_limpia <- read_csv("data/base_pisa_brasil_peru_limpia.csv", show_col_types = FALSE)

# 1 Estandarización y escalado
# Variables numéricas continuas
vars_numericas <- c("i_soc_econ", "educ_padres", "ef_escuela", "ef_mult_asig",
                    "ef_familia", "ef_confianza", "ef_conf_tecno",
                    "ef_amigos", "punt_finan")

# Variables categóricas (se mantienen como factor, no se escalan)
vars_categoricas <- c("pais", "genero")

# Escalado 0-1 (min-max)
base_gower <- base_pisa_brasil_peru_limpia %>%
  mutate(across(all_of(vars_numericas),
                ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
                .names = "{.col}_esc"))

# Verificación: cada variable escalada debería ir de 0 a 1
base_gower %>%
  select(ends_with("_esc")) %>%
  summary()

# Convertir las categóricas a factor (necesario para el paso de Gower más adelante)
base_gower <- base_gower %>%
  mutate(across(all_of(vars_categoricas), as.factor))

str(base_gower)

# 2 Tendencia a la aglomeración
# Primero necesitamos la matriz de distancias de Gower ya calculada
dist_gower <- daisy(
  base_gower %>% select(ends_with("_esc"), all_of(vars_categoricas)),
  metric = "gower"
)

# Hopkins se calcula típicamente sobre los datos originales (no la distancia),
# pero también existe una versión que trabaja sobre la matriz de disimilitud
set.seed(123)
muestra_idx <- sample(1:nrow(base_gower), 500)  # muestra de 500 casos

dist_gower_muestra <- daisy(
  base_gower[muestra_idx, ] %>% select(ends_with("_esc"), all_of(vars_categoricas)),
  metric = "gower"
)

fviz_dist(dist_gower_muestra, gradient = list(low = "#4C72B0", mid = "white", high = "#D64550"))

# Cómo calcular Hopkins correctamente con distancia de Gower
set.seed(123)

datos_hopkins <- base_gower[muestra_idx, ] %>%
  select(ends_with("_esc"), all_of(vars_categoricas))

n <- nrow(datos_hopkins)
m <- 50  # cantidad de puntos a samplear para el test (subconjunto de la muestra)

# 1. Elegir m puntos reales al azar
idx_reales <- sample(1:n, m)

# 2. Distancia de cada punto real a su vecino más cercano (excluyéndose a sí mismo)
dist_matrix <- as.matrix(daisy(datos_hopkins, metric = "gower"))
diag(dist_matrix) <- NA

u_dist <- map_dbl(idx_reales, ~ min(dist_matrix[.x, ], na.rm = TRUE))

# 3. Generar m puntos "sintéticos" aleatorios dentro del rango de los datos
generar_punto_aleatorio <- function() {
  fila <- list()
  for (col in names(datos_hopkins)) {
    if (is.numeric(datos_hopkins[[col]])) {
      fila[[col]] <- runif(1, min(datos_hopkins[[col]]), max(datos_hopkins[[col]]))
    } else {
      fila[[col]] <- sample(levels(as.factor(datos_hopkins[[col]])), 1)
    }
  }
  as.data.frame(fila)
}

puntos_sinteticos <- map_dfr(1:m, ~ generar_punto_aleatorio())

# Asegurar mismos tipos de columna que el original
for (col in names(datos_hopkins)) {
  if (is.factor(datos_hopkins[[col]])) {
    puntos_sinteticos[[col]] <- factor(puntos_sinteticos[[col]], levels = levels(datos_hopkins[[col]]))
  }
}

# 4. Distancia de cada punto sintético al dato real más cercano
base_combinada <- bind_rows(puntos_sinteticos, datos_hopkins)
dist_combinada <- as.matrix(daisy(base_combinada, metric = "gower"))

w_dist <- map_dbl(1:m, ~ min(dist_combinada[.x, (m+1):(m+n)]))

# 5. Estadístico de Hopkins
H <- sum(w_dist) / (sum(w_dist) + sum(u_dist))
H

# Paso 3a: determinar k óptimo con silhouette
# Alternativa más liviana: selección de k sobre una muestra
set.seed(123)
muestra_grande_idx <- sample(1:nrow(base_gower), 1500)

dist_gower_muestra_grande <- daisy(
  base_gower[muestra_grande_idx, ] %>% select(ends_with("_esc"), all_of(vars_categoricas)),
  metric = "gower"
)

fviz_nbclust(
  as.matrix(dist_gower_muestra_grande),
  FUNcluster = pam,
  method = "silhouette",
  diss = dist_gower_muestra_grande,
  k.max = 8
)

# Paso 3b: aplicar PAM con el k elegido
dist_gower_completo <- daisy(
  base_gower %>% select(ends_with("_esc"), all_of(vars_categoricas)),
  metric = "gower"
)

set.seed(123)
pam_res <- pam(dist_gower_completo, k = 4, diss = TRUE)

# Ver tamaño de cada cluster
table(pam_res$clustering)

# Ver los medoides (los casos "representativos" de cada cluster)
base_gower[pam_res$id.med, ]

# Ancho de silueta promedio del resultado final
pam_res$silinfo$avg.width

# Visualización del resultado
# Reducir la matriz de distancias de Gower a 2 dimensiones
mds_coords <- cmdscale(dist_gower_completo, k = 2)
colnames(mds_coords) <- c("Dim1", "Dim2")

# Armar el gráfico pasando las coordenadas MDS + la asignación de cluster
fviz_cluster(
  list(data = mds_coords, cluster = pam_res$clustering),
  geom = "point",
  ellipse.type = "convex",
  alpha = 0.3,
  main = "PAM sobre distancia de Gower (proyección MDS) — Base PISA Brasil/Perú"
)

# Sobre el silhouette plot individual (buena práctica adicional)
fviz_silhouette(pam_res)

# Paso 4a: caracterización numérica (tabla de medias/medianas por cluster)

base_gower <- base_gower %>%
  mutate(cluster = factor(pam_res$clustering))

# Tabla resumen de variables numéricas por cluster
tabla_caracterizacion <- base_gower %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(c(i_soc_econ, educ_padres, ef_escuela, ef_mult_asig, ef_familia,
             ef_confianza, ef_conf_tecno, ef_amigos, punt_finan),
           ~ round(mean(.x, na.rm = TRUE), 2))
  )

# Paso 4b: caracterización categórica (composición de género y país por cluster)

# Distribución de país dentro de cada cluster
base_gower %>%
  count(cluster, pais) %>%
  group_by(cluster) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

# Distribución de género dentro de cada cluster
base_gower %>%
  count(cluster, genero) %>%
  group_by(cluster) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

# Paso 5: boxplots por cluster para cada variable numérica

base_larga <- base_gower %>%
  select(cluster, i_soc_econ, educ_padres, ef_escuela, ef_mult_asig,
         ef_familia, ef_confianza, ef_conf_tecno, ef_amigos, punt_finan) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "valor")

ggplot(base_larga, aes(x = cluster, y = valor, fill = cluster)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5, outlier.alpha = 0.3) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("#EF8A62", "#91BFDB", "#66C2A5", "#C994C7")) +
  labs(title = "Caracterización de clusters (PAM) por variable",
       x = "Cluster", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggplot(base_gower, aes(x = cluster, y = punt_finan, fill = cluster)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("#EF8A62", "#91BFDB", "#66C2A5", "#C994C7")) +
  labs(title = "Desempeño en alfabetización financiera por cluster",
       x = "Cluster", y = "Puntaje (punt_finan)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))

# PCA sobre variables continuas
vars_pca <- base_gower %>%
  select(i_soc_econ, educ_padres, ef_escuela, ef_mult_asig,
         ef_familia, ef_confianza, ef_conf_tecno, ef_amigos, punt_finan)

# Chequeo de NAs antes de correr el PCA
sapply(vars_pca, function(x) sum(is.na(x)))

pca_res <- prcomp(vars_pca, scale. = TRUE)
summary(pca_res)

nrow(base_gower) == nrow(vars_pca)
identical(nrow(vars_pca), length(pam_res$clustering))

# Proyección de los clusters sobre el ACP

fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  col.ind = base_gower$cluster,
  palette = c("#EF8A62", "#91BFDB", "#66C2A5", "#C994C7"),
  addEllipses = TRUE,
  ellipse.type = "confidence",
  alpha.ind = 0.4,
  legend.title = "Cluster (PAM)"
) +
  labs(title = "Individuos en el plano del ACP, coloreados por cluster (PAM)")

# Biplot con las variables originales superpuestas
fviz_pca_biplot(
  pca_res,
  geom.ind = "point",
  col.ind = base_gower$cluster,
  palette = c("#EF8A62", "#91BFDB", "#66C2A5", "#C994C7"),
  addEllipses = TRUE,
  ellipse.type = "confidence",
  alpha.ind = 0.3,
  col.var = "black",
  repel = TRUE,
  legend.title = "Cluster (PAM)"
) +
  labs(title = "Biplot ACP: individuos por cluster y variables originales")

# Paso 7a: segundo algoritmo — cluster jerárquico (Ward) sobre la misma distancia de Gower
hc_gower <- hclust(dist_gower_completo, method = "ward.D2")

# Cortamos al mismo k que usamos en PAM, para que la comparación sea directa
grupos_hc <- cutree(hc_gower, k = 4)

table(grupos_hc)

# Paso 7b: Índice de Rand Ajustado (ARI)
library(mclust)

ari <- adjustedRandIndex(pam_res$clustering, grupos_hc)
ari

# Paso 7c: Variación de la Información (Meilă, 2003)
library(mcclust)

vi <- vi.dist(pam_res$clustering, grupos_hc)
vi

table(PAM = pam_res$clustering, Ward = grupos_hc)
