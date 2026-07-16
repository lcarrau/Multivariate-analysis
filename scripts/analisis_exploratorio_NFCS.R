library(haven)
library(dplyr)
library(tidyr)
library(janitor)

ruta_archivo <- "data/NFCS 2024.sav"
nfcs_raw <- read_sav(ruta_archivo) %>% clean_names()

# --- Recodificar 98 y 99 como NA en todas las columnas numéricas ---
nfcs <- nfcs_raw %>%
  mutate(across(where(is.numeric), ~ na_if(na_if(as.numeric(.x), 98), 99)))

# --- Variable de agrupamiento ---
nfcs <- nfcs %>%
  mutate(grupo_edad = as_factor(a3ar_w))

table(nfcs$grupo_edad, useNA = "ifany")

# --- Bloque para PCA: actitudes y bienestar financiero ---
vars_pca <- c("j1", "j2", "j33_40", "b60", "g23", "m1_1", "m4",
              "j41_1", "j41_2", "j41_3", "j42_1", "j42_2", "j43")

nfcs_pca <- nfcs %>% select(all_of(vars_pca))
summary(nfcs_pca)

# --- Bloque para ACM: categóricas nominales ---
vars_acm <- c("b1", "b2", "b4", "c1_2012", "c4_2012", "c5_2012",
              "e7", "e8", "ea_1", "f2_1", "f2_2", "f2_3",
              "g1", "g20", "g60", "g38", "h1", "h30_3",
              "a6", "a9", "a5_2015")

nfcs_acm <- nfcs %>%
  select(all_of(vars_acm)) %>%
  mutate(across(everything(), as_factor))

glimpse(nfcs_acm)

# --- Score de conocimiento financiero objetivo (para comparar luego) ---
vars_conocimiento <- c("m6", "m7", "m8", "m9", "m10", "m31", "m50")
respuestas_correctas <- c(m6 = 1, m7 = 3, m8 = 2, m9 = 1, m10 = 2, m31 = 3, m50 = 1)
# ¡Ojo! confirmar cuál es la opción correcta según el codebook

nfcs <- nfcs %>%
  rowwise() %>%
  mutate(score_conocimiento = sum(c_across(all_of(vars_conocimiento)) ==
                                    respuestas_correctas[vars_conocimiento], na.rm = TRUE)) %>%
  ungroup()

hist(nfcs$score_conocimiento)

