
# Análisis exploratorio

install.packages(c(
  "haven",
  "tidyverse",
  "janitor",
  "labelled",
  "skimr"
))

library(haven)
library(tidyverse)
library(janitor)
library(labelled)
library(skimr)

unzip(
  zipfile = "data/SPSS_STU_FLT.zip",
  exdir = "data/PISA2018"
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

# Identificar cuál es la base principal de estudiantes

grep(
  "PV1READ",
  names(pisa_list[[2]]),
  value = TRUE
)

# Convertir etiquetas SPSS si es necesario

look_for(pisa_list[[1]])

library(janitor)

# los cuatro data frames tendrán nombres de variables estandarizados:
pisa_list <- lapply(
  pisa_list,
  clean_names
)

lapply(pisa_list, glimpse)
sapply(pisa_list, function(x) ncol(x))
names(pisa_list$CY07_MSU_FLT_QQQ)[1:20]

# el archivo QQQ contiene identificadores típicos de PISA

# Paso siguiente: crear objetos de trabajo
qqq <- pisa_list$CY07_MSU_FLT_QQQ.SAV
cog <- pisa_list$CY07_MSU_FLT_COG.SAV
tim <- pisa_list$CY07_MSU_FLT_TIM.SAV
ttm <- pisa_list$CY07_MSU_FLT_TTM.SAV

# Ver cuántos estudiantes hay
nrow(qqq)

# Ver cuántos países hay
length(unique(qqq$cnt))
sort(unique(qqq$cnt))

# Distribución por país
qqq %>%
  count(cnt, sort = TRUE)

# Revisar tipos de variables
glimpse(qqq)

grep("st003d02t", names(qqq), value = TRUE)
labelled::val_labels(qqq$st004d01t)
