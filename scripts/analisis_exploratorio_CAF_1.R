
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

caf <- caf %>%
  clean_names()

names(caf)

glimpse(caf)
summary(caf)
skim(caf)

# ¿Cuántos países hay?
caf %>%
  distinct(pais) %>%
  arrange(pais)

# Explorar anios
caf %>%
  count(ano)

sort(unique(caf$ano))

# Explorar temas y subtemas
caf %>%
  distinct(tema)

caf %>%
  distinct(subtema)

# Variables
caf %>%
  distinct(variable)
caf %>%
  count(variable, sort = TRUE)

# Identificar indicadores interesantes
caf %>%
  filter(variable == "ahorro1")
caf %>%
  filter(variable == "metas1")

# Estadísticos descriptivos
# Variable valor

caf %>%
  summarise(
    minimo = min(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE)
  )

ggplot(caf,
       aes(valor)) +
  geom_histogram(
    bins = 30
  )

caf %>%
  filter(variable == "ahorro1") %>%
  ggplot(
    aes(
      x = reorder(pais, valor),
      y = valor
    )
  ) +
  geom_col() +
  coord_flip()

caf %>%
  filter(variable == "ahorro1") %>%
  ggplot(
    aes(
      x = ano,
      y = valor,
      color = pais
    )
  ) +
  geom_line() +
  geom_point()

caf %>%
  arrange(desc(valor)) %>%
  select(
    pais,
    ano,
    indicador,
    valor
  ) %>%
  head(10)

caf %>%
  filter(variable %in% c(
    "ahorro1",
    "metas1",
    "choques1"
  )) %>%
  ggplot(
    aes(
      x = pais,
      y = valor,
      fill = variable
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip()
