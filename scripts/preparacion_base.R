Preparación base 
# ==========================================================
# 1. LIMPIAR EL ENTORNO DE TRABAJO
# ==========================================================

rm(list = ls())


# ==========================================================
# 2. CARGAR LIBRERÍAS
# ==========================================================

library(haven)
library(dplyr)


# ==========================================================
# 3. DEFINIR LA RUTA DEL PROYECTO
# ==========================================================
#============= Mi archivo:

ruta_proyecto <- "C:/Análisis multivariado/Trabajo final/Multivariate-analysis"


# ==========================================================
# 4. CARGAR LA BASE PISA 2022
# ARCHIVO DEL CUESTIONARIO DE ESTUDIANTES
# ==========================================================

pisa_qqq <- read_sav(
  file.path(
    ruta_proyecto,
    "/data/PISA2022/CY08MSP_FLT_QQQ.SAV"
  )
)


# ==========================================================
# 5. VERIFICAR LAS DIMENSIONES DE LA BASE ORIGINAL
# ==========================================================

cat("\n=============================================\n")
cat("DIMENSIONES DE LA BASE ORIGINAL\n")
cat("=============================================\n")

print(dim(pisa_qqq))


# ==========================================================
# 6. VERIFICAR LA CANTIDAD DE ESTUDIANTES POR PAÍS
# ==========================================================

cat("\n=============================================\n")
cat("CANTIDAD DE ESTUDIANTES POR PAÍS\n")
cat("=============================================\n")

print(table(pisa_qqq$CNT))


# ==========================================================
# 7. SELECCIONAR BRASIL, COSTA RICA Y PERÚ
#
# BRA = Brasil
# CRI = Costa Rica
# PER = Perú
# ==========================================================

base_latam <- pisa_qqq %>%
  filter(
    CNT %in% c(
      "BRA",
      "CRI",
      "PER"
    )
  )


# ==========================================================
# 8. VERIFICAR LA CANTIDAD DE ESTUDIANTES
# DE LOS PAÍSES SELECCIONADOS
# ==========================================================

cat("\n=============================================\n")
cat("ESTUDIANTES DE BRASIL, COSTA RICA Y PERÚ\n")
cat("=============================================\n")

print(table(base_latam$CNT))

print(dim(base_latam))


# ==========================================================
# 9. DEFINIR LOS DIEZ VALORES PLAUSIBLES
# DE ALFABETIZACIÓN FINANCIERA
# ==========================================================

variables_pv <- paste0(
  "PV",
  1:10,
  "FLIT"
)

print(variables_pv)


# ==========================================================
# 10. DEFINIR LAS VARIABLES DEL ESTUDIO
#
# CNT:
# País
#
# CNTSTUID:
# Identificador del estudiante
#
# ST004D01T:
# Género
#
# ESCS:
# Índice económico, social y cultural
#
# HISEI:
# Mayor estatus ocupacional de los padres
#
# PAREDINT:
# Años de educación del progenitor con
# mayor escolaridad
#
# FLSCHOOL:
# Educación financiera recibida en la escuela
#
# FLMULTSB:
# Educación financiera en varias materias
#
# FLFAMILY:
# Participación familiar en asuntos financieros
#
# FLCONFIN:
# Confianza en asuntos financieros
#
# FLCONICT:
# Confianza financiera con dispositivos digitales
#
# FRINFLFM:
# Influencia de los amigos en asuntos financieros
#
# PV1FLIT a PV10FLIT:
# Valores plausibles de alfabetización financiera
# ==========================================================

variables_seleccionadas <- c(
  "CNT",
  "CNTSTUID",
  "ST004D01T",
  "ESCS",
  "HISEI",
  "PAREDINT",
  "FLSCHOOL",
  "FLMULTSB",
  "FLFAMILY",
  "FLCONFIN",
  "FLCONICT",
  "FRINFLFM",
  variables_pv
)


# ==========================================================
# 11. VERIFICAR QUE LAS VARIABLES EXISTAN
# ==========================================================

variables_no_encontradas <- setdiff(
  variables_seleccionadas,
  names(base_latam)
)

if (length(variables_no_encontradas) > 0) {
  
  stop(
    paste(
      "No se encontraron las variables:",
      paste(
        variables_no_encontradas,
        collapse = ", "
      )
    )
  )
  
} else {
  
  cat("\nTodas las variables seleccionadas existen.\n")
  
}


# ==========================================================
# 12. CREAR LA BASE REDUCIDA
# ==========================================================

base_reducida <- base_latam %>%
  select(
    all_of(
      variables_seleccionadas
    )
  )


# ==========================================================
# 13. CONVERTIR LOS VALORES PLAUSIBLES
# A FORMATO NUMÉRICO
# ==========================================================

base_reducida <- base_reducida %>%
  mutate(
    across(
      all_of(variables_pv),
      as.numeric
    )
  )


# ==========================================================
# 14. CREAR EL PUNTAJE FINANCIERO PROMEDIO
#
# Se obtiene como promedio de los diez
# valores plausibles.
#
# Se utiliza como variable exploratoria
# para este trabajo práctico.
# ==========================================================

base_reducida <- base_reducida %>%
  mutate(
    puntaje_financiero = rowMeans(
      select(
        .,
        all_of(variables_pv)
      ),
      na.rm = TRUE
    )
  )


# ==========================================================
# 15. REEMPLAZAR NaN POR NA
# ==========================================================

base_reducida$puntaje_financiero[
  is.nan(base_reducida$puntaje_financiero)
] <- NA


# ==========================================================
# 16. RECODIFICAR EL PAÍS
# ==========================================================

base_reducida <- base_reducida %>%
  mutate(
    pais = factor(
      as.character(CNT),
      levels = c(
        "BRA",
        "CRI",
        "PER"
      ),
      labels = c(
        "Brasil",
        "Costa Rica",
        "Perú"
      )
    )
  )


# ==========================================================
# 17. RECODIFICAR EL GÉNERO
#
# 1 = Mujer
# 2 = Varón
# ==========================================================

base_reducida <- base_reducida %>%
  mutate(
    genero = case_when(
      as.numeric(ST004D01T) == 1 ~ "Mujer",
      as.numeric(ST004D01T) == 2 ~ "Varón",
      TRUE ~ NA_character_
    ),
    genero = factor(
      genero,
      levels = c(
        "Mujer",
        "Varón"
      )
    )
  )


# ==========================================================
# 18. CONVERTIR LAS VARIABLES CUANTITATIVAS
# A FORMATO NUMÉRICO
# ==========================================================

variables_cuantitativas <- c(
  "ESCS",
  "HISEI",
  "PAREDINT",
  "FLSCHOOL",
  "FLMULTSB",
  "FLFAMILY",
  "FLCONFIN",
  "FLCONICT",
  "FRINFLFM",
  "puntaje_financiero"
)

base_reducida <- base_reducida %>%
  mutate(
    across(
      all_of(variables_cuantitativas),
      as.numeric
    )
  )


# ==========================================================
# 19. REORDENAR LAS VARIABLES
# ==========================================================

base_reducida <- base_reducida %>%
  select(
    pais,
    genero,
    CNTSTUID,
    ESCS,
    HISEI,
    PAREDINT,
    FLSCHOOL,
    FLMULTSB,
    FLFAMILY,
    FLCONFIN,
    FLCONICT,
    FRINFLFM,
    puntaje_financiero,
    all_of(variables_pv)
  )


# ==========================================================
# 20. VERIFICAR LAS DIMENSIONES
# DE LA BASE REDUCIDA
# ==========================================================

cat("\n=============================================\n")
cat("DIMENSIONES DE LA BASE REDUCIDA\n")
cat("=============================================\n")

print(dim(base_reducida))


# ==========================================================
# 21. VERIFICAR LA DISTRIBUCIÓN POR PAÍS
# ==========================================================

cat("\n=============================================\n")
cat("DISTRIBUCIÓN POR PAÍS\n")
cat("=============================================\n")

print(
  table(
    base_reducida$pais,
    useNA = "ifany"
  )
)


# ==========================================================
# 22. VERIFICAR LA DISTRIBUCIÓN POR GÉNERO
# ==========================================================

cat("\n=============================================\n")
cat("DISTRIBUCIÓN POR GÉNERO\n")
cat("=============================================\n")

print(
  table(
    base_reducida$genero,
    useNA = "ifany"
  )
)


# ==========================================================
# 23. ANALIZAR LOS DATOS FALTANTES
# ==========================================================

variables_para_faltantes <- c(
  "pais",
  "genero",
  variables_cuantitativas
)

tabla_faltantes <- data.frame(
  variable = variables_para_faltantes,
  
  cantidad_NA = sapply(
    base_reducida[
      variables_para_faltantes
    ],
    function(x) {
      sum(is.na(x))
    }
  ),
  
  porcentaje_NA = round(
    sapply(
      base_reducida[
        variables_para_faltantes
      ],
      function(x) {
        mean(is.na(x)) * 100
      }
    ),
    2
  ),
  
  row.names = NULL
)

tabla_faltantes <- tabla_faltantes %>%
  arrange(
    desc(porcentaje_NA)
  )


# ==========================================================
# 24. MOSTRAR LA TABLA DE DATOS FALTANTES
# ==========================================================

cat("\n=============================================\n")
cat("DATOS FALTANTES POR VARIABLE\n")
cat("=============================================\n")

print(
  tabla_faltantes,
  row.names = FALSE
)


# ==========================================================
# 25. CALCULAR ESTADÍSTICOS DESCRIPTIVOS
# ==========================================================

tabla_descriptiva <- data.frame(
  variable = variables_cuantitativas,
  
  cantidad_valida = sapply(
    base_reducida[
      variables_cuantitativas
    ],
    function(x) {
      sum(!is.na(x))
    }
  ),
  
  media = sapply(
    base_reducida[
      variables_cuantitativas
    ],
    function(x) {
      mean(x, na.rm = TRUE)
    }
  ),
  
  desvio = sapply(
    base_reducida[
      variables_cuantitativas
    ],
    function(x) {
      sd(x, na.rm = TRUE)
    }
  ),
  
  minimo = sapply(
    base_reducida[
      variables_cuantitativas
    ],
    function(x) {
      min(x, na.rm = TRUE)
    }
  ),
  
  maximo = sapply(
    base_reducida[
      variables_cuantitativas
    ],
    function(x) {
      max(x, na.rm = TRUE)
    }
  ),
  
  row.names = NULL
)

tabla_descriptiva <- tabla_descriptiva %>%
  mutate(
    across(
      c(
        media,
        desvio,
        minimo,
        maximo
      ),
      ~ round(.x, 2)
    )
  )


# ==========================================================
# 26. MOSTRAR LOS ESTADÍSTICOS DESCRIPTIVOS
# ==========================================================

cat("\n=============================================\n")
cat("ESTADÍSTICOS DESCRIPTIVOS\n")
cat("=============================================\n")

print(
  tabla_descriptiva,
  row.names = FALSE
)


# ==========================================================
# 27. CALCULAR EL PUNTAJE FINANCIERO POR PAÍS
# ==========================================================

puntaje_por_pais <- base_reducida %>%
  group_by(pais) %>%
  summarise(
    estudiantes = n(),
    
    casos_validos = sum(
      !is.na(puntaje_financiero)
    ),
    
    media = mean(
      puntaje_financiero,
      na.rm = TRUE
    ),
    
    desvio = sd(
      puntaje_financiero,
      na.rm = TRUE
    ),
    
    minimo = min(
      puntaje_financiero,
      na.rm = TRUE
    ),
    
    maximo = max(
      puntaje_financiero,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        media,
        desvio,
        minimo,
        maximo
      ),
      ~ round(.x, 2)
    )
  )


# ==========================================================
# 28. MOSTRAR EL PUNTAJE FINANCIERO POR PAÍS
# ==========================================================

cat("\n=============================================\n")
cat("PUNTAJE FINANCIERO POR PAÍS\n")
cat("=============================================\n")

print(
  puntaje_por_pais,
  row.names = FALSE
)


# ==========================================================
# 29. GUARDAR LA BASE REDUCIDA
# EN FORMATO RDS
# ==========================================================

saveRDS(
  base_reducida,
  file.path(
    ruta_proyecto,
    "base_pisa_latam_reducida.rds"
  )
)


# ==========================================================
# 30. GUARDAR LA BASE REDUCIDA
# EN FORMATO CSV
# ==========================================================

write.csv(
  base_reducida,
  file.path(
    ruta_proyecto,
    "base_pisa_latam_reducida.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ==========================================================
# 31. GUARDAR LA TABLA DE DATOS FALTANTES
# ==========================================================

write.csv(
  tabla_faltantes,
  file.path(
    ruta_proyecto,
    "tabla_faltantes.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ==========================================================
# 32. GUARDAR LA TABLA DESCRIPTIVA
# ==========================================================

write.csv(
  tabla_descriptiva,
  file.path(
    ruta_proyecto,
    "tabla_descriptiva.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ==========================================================
# 33. GUARDAR EL PUNTAJE POR PAÍS
# ==========================================================

write.csv(
  puntaje_por_pais,
  file.path(
    ruta_proyecto,
    "puntaje_por_pais.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ==========================================================
# 34. MENSAJE FINAL
# ==========================================================

cat("\n=============================================\n")
cat("PREPARACIÓN DE LA BASE FINALIZADA\n")
cat("=============================================\n")

cat(
  "\nArchivos creados:\n",
  "- base_pisa_latam_reducida.rds\n",
  "- base_pisa_latam_reducida.csv\n",
  "- tabla_faltantes.csv\n",
  "- tabla_descriptiva.csv\n",
  "- puntaje_por_pais.csv\n"
)
