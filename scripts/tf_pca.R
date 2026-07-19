library(readr) # Lectura de la BBDD
library(dplyr)
library(naniar)  # Visualizar patrones de NA
library(missMDA) # Imputar valores faltantes
library(ggplot2)
library(GGally)
library(forcats) 

# Leer la base de datos

base_pisa_latam_red <- read_csv("data/base_pisa_latam_red.csv")

# Renombrar variables
base_desc <- base_pisa_latam_red %>%
  rename(
    pais = CNT,
    id = CNTSTUID,
    genero = ST004D01T,
    i_soc_econ = ESCS,
    educ_padres = PAREDINT,
    ef_escuela = FLSCHOOL,
    ef_mult_asig = FLMULTSB,
    ef_familia = FLFAMILY,
    ef_confianza = FLCONFIN,
    ef_conf_tecno = FLCONICT,
    ef_amigos = FRINFLFM
  )

# Ajustar el tipo de dato
base_desc <- base_desc %>%
  mutate(
    id     = as.character(id),
    pais   = as.factor(pais),
    genero = factor(genero, levels = c(1, 2), labels = c("Masculino", "Femenino"))
  )

glimpse(base_desc)

# comportamiento univariado de las variables cualitativas
table(base_desc$genero)
prop.table(table(base_desc$genero))

table(base_desc$pais)
prop.table(table(base_desc$pais))

# análisis exploratorio bivariado

ggplot(base_desc, aes(x = genero, y = PV1FLIT, fill = genero)) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.85,
    outlier.color = "#D64550",
    outlier.size = 2,
    outlier.alpha = 0.6
  ) +
  scale_fill_manual(values = c("Masculino" = "#4C72B0", "Femenino" = "#DD8452")) +
  labs(
    title = "Desempeño financiero en función del género",
    x = "Género",
    y = "Puntaje de alfabetización financiera (PV1FLIT)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(base_desc, aes(x = fct_reorder(pais, PV1FLIT, .fun = median, .na_rm = TRUE),
                      y = PV1FLIT, fill = pais)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.85,
    outlier.color = "#D64550",
    outlier.size = 1.8,
    outlier.alpha = 0.5
  ) +
  labs(
    title = "Desempeño financiero según país",
    x = "País",
    y = "Puntaje de alfabetización financiera (PV1FLIT)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

vars_numericas <- base_desc %>%
  select(i_soc_econ, educ_padres, PV1FLIT,
         ef_escuela, ef_mult_asig, ef_familia,
         ef_confianza, ef_conf_tecno, ef_amigos)

ggpairs(
  vars_numericas,
  upper = list(continuous = wrap("cor", size = 3.5, color = "#2C3E50")),
  lower = list(continuous = wrap("points", alpha = 0.15, size = 0.6, color = "#4C72B0")),
  diag  = list(continuous = wrap("densityDiag", fill = "#DD8452", alpha = 0.6))
) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank()
  )

# Selección de las variables cuantitativas

vars_pca <- base_desc %>%
  select(i_soc_econ, educ_padres, PV1FLIT,
         ef_escuela, ef_mult_asig, ef_familia,
         ef_confianza, ef_conf_tecno, ef_amigos)

# Guardar el archivo como .csv
write.csv(
  vars_pca,
  file.path(
    "C:/Análisis multivariado/Trabajo final/Multivariate-analysis/data",
    "vars_pca.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Chequeo de NAs antes de correr el PCA
sapply(vars_pca, function(x) sum(is.na(x)))

# Proporción de NA por variable
vars_pca %>%
  summarise(across(everything(), ~ mean(is.na(.x)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "prop_na") %>%
  arrange(desc(prop_na))

# Visualizar el patrón de NAs (¿faltan juntos o independientes?)
vis_miss(vars_pca)
gg_miss_upset(vars_pca)  # combinaciones de variables que faltan juntas

# Estimar el número óptimo de componentes para la imputación
ncomp <- estim_ncpPCA(vars_pca, ncp.max = 5, method.cv = "Kfold")

# Imputar valores faltantes
imputado <- imputePCA(vars_pca, ncp = ncomp$ncp)
vars_pca_imp <- as.data.frame(imputado$completeObs)

# Verificación
sapply(vars_pca_imp, function(x) sum(is.na(x)))

# Cálculo de los coeficientes de variación
sd <- sapply(vars_pca_imp, sd)
mean <- sapply(vars_pca_imp, mean)
cv <- sd/mean
cv

