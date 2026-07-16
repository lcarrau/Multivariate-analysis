
# Análisis exploratorio

setwd("C:/Análisis multivariado/Trabajo final/Multivariate-analysis")

# install.packages(c(
#   "haven",
#   "tidyverse",
#   "janitor",
#   "labelled",
#   "skimr"
# ))

library(haven)
library(tidyverse)
library(janitor)
library(labelled)
library(skimr)

unzip(
  zipfile = "data/FLT_SPSS.zip",
  exdir = "data/PISA2022"
)

list.files(
  "data/PISA2018",
  recursive = TRUE,
  full.names = TRUE
)

sav_files <- list.files(
  "data/PISA2018/FLT",
  pattern = "\\.[Ss][Aa][Vv]$",
  full.names = TRUE
)

sav_files

# Leer todos los archivos automáticamente

pisa_list <- lapply(sav_files, read_sav)

names(pisa_list) <- basename(sav_files)

names(pisa_list)

# Explorar dimensiones

map_df(
  pisa_list,
  ~ tibble(
    filas = nrow(.x),
    columnas = ncol(.x)
  ),
  .id = "archivo"
)

# Revisar las variables
# Por ejemplo para el primer archivo:

names(pisa_list[[1]])
glimpse(pisa_list[[1]])
str(pisa_list[[1]])

################################################################################

# Cargar los tres archivos


cog <- read_sav("data/PISA2022/CY08MSP_FLT_COG.SAV")

qqq <- read_sav("data/PISA2022/CY08MSP_FLT_QQQ.SAV")

# tim <- read_sav("data/PISA2022/CY08MSP_FLT_TIM.SAV")

dim(cog)
dim(qqq)
# dim(tim)

names(cog)[1:100]

names(qqq)[1:20]

# Buscar las variables de educación financiera

grep(
  "FL",
  names(qqq),
  value = TRUE
)

grep(
  "FIN",
  names(qqq),
  value = TRUE
)

grep(
  "PV",
  names(qqq),
  value = TRUE
)

tail(names(qqq), 150)

# Identificar variables demográficas
grep("ST004", names(qqq), value = TRUE)
grep("ESCS", names(qqq), value = TRUE)
grep("PARED", names(qqq), value = TRUE)
grep("AGE", names(qqq), value = TRUE)
grep("ST003", names(qqq), value = TRUE)

# Verificar etiquetas / library(labelled)

var_label(qqq$ST004D01T)
val_labels(qqq$ST004D01T)

summary(qqq$AGE)

summary(qqq$ESCS)

summary(qqq$PAREDINT)

# Crear una base reducida
base <- qqq %>%
  select(
    CNT,
    CNTSTUID,
    ST004D01T,
    AGE,
    ESCS,
    PAREDINT,
    PV1FLIT
  )

glimpse(base)

summary(base)

# Revisar faltantes
colSums(is.na(base))

round(
  100 * colMeans(is.na(base)),
  2
)

# Renombrar variables
base <- base %>%
  rename(
    pais = CNT,
    id = CNTSTUID,
    sexo = ST004D01T,
    edad = AGE,
    escs = ESCS,
    educ_padres = PAREDINT,
    flit = PV1FLIT
  )

# Exploración descriptiva
# Educación financiera
summary(base$flit)

sd(base$flit, na.rm = TRUE)

hist(
  base$flit,
  main = "Educación financiera",
  xlab = "PV1FLIT"
)

# Edad
summary(base$edad)

# ESCS
summary(base$escs)

# Educación de los padres
summary(base$educ_padres)

# Comparación entre países
base %>%
  group_by(pais) %>%
  summarise(
    promedio_flit = mean(flit, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(promedio_flit))

# Correlaciones
# Tomando las variables numéricas:

base %>%
  select(
    edad,
    escs,
    educ_padres,
    flit
  ) %>%
  cor(
    use = "complete.obs"
  )

# Preparar base para PCA
# Primero eliminar faltantes:

base_pca <- base %>%
  select(
    edad,
    escs,
    educ_padres,
    flit
  ) %>%
  na.omit()

dim(base_pca)

# PCA
pca <- prcomp(
  base_pca,
  scale. = TRUE
)
summary(pca)

# Cargas
pca$rotation

# Explorar otras variables financieras

var_label(qqq$FLSCHOOL)
var_label(qqq$FLMULTSB)
var_label(qqq$FLFAMILY)
var_label(qqq$FLCONFIN)
var_label(qqq$FLCONICT)
var_label(qqq$FRINFLFM)

# Base ampliada
base_fin <- qqq %>%
  select(
    CNT,
    CNTSTUID,
    ST004D01T,
    AGE,
    ESCS,
    PAREDINT,
    PV1FLIT,
    FLSCHOOL,
    FLMULTSB,
    FLFAMILY,
    FLCONFIN,
    FLCONICT,
    FRINFLFM
  )

glimpse(base_fin)

round(
  100 * colMeans(is.na(base_fin)),
  2
)

# Sacando la variable que tiene muchos datos faltantes

# única base completa con todas las variables
base_modelo <- base_fin %>%
  select(
    AGE,
    ESCS,
    PAREDINT,
    ST004D01T,
    PV1FLIT,
    FLSCHOOL,
    FLFAMILY,
    FLCONFIN,
    FLCONICT,
    FRINFLFM
  ) %>%
  na.omit()

datos_pca <- base_modelo %>%
  select(
    PV1FLIT,
    FLSCHOOL,
    FLFAMILY,
    FLCONFIN,
    FLCONICT,
    FRINFLFM
  )
pca <- prcomp(
  datos_pca,
  scale. = TRUE
)
scores <- cbind(
  base_modelo %>%
    select(
      AGE,
      ESCS,
      PAREDINT,
      ST004D01T
    ),
  as.data.frame(pca$x)
)

summary(pca)

round(pca$rotation, 3)
