
# install.packages(c(
#   "readxl",
#   "tidyverse",
#   "janitor",
#   "skimr"
# ))

library(readxl)
library(tidyverse)
library(janitor)
library(skimr)

excel_sheets("data/base-de-datos_CAF.xlsx")

caf <- read_excel(
  "data/base-de-datos_CAF.xlsx",
  sheet = "Datos"
)

names(caf)
head(caf)
glimpse(caf)
summary(caf)

# Comprobar valores faltantes
colSums(is.na(caf))

# Limpiar nombres de variables
caf <- caf %>%
  clean_names()

# Crear una versión ordenada
caf <- caf %>%
  arrange(
    pais,
    ano,
    variable
  )

# Inventario de variables
caf %>%
  distinct(variable)
caf %>%
  count(variable, sort = TRUE)

length(unique(caf$variable))

# Crear una tabla resumen de indicadores
catalogo <- caf %>%
  select(
    variable,
    indicador,
    tema,
    subtema
  ) %>%
  distinct()

base_analisis <- caf %>%
  filter(
    variable %in% c(
      "ahorro1",
      "metas1",
      "choques1"
    )
  )


# Pasar a formato ancho
library(tidyr)

base_wide <- base_analisis %>%
  select(
    pais,
    ano,
    variable,
    valor
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )







