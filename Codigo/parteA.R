# Parte A: población, decrementos y multiestado
# Para correrlo: source("Codigo/parteA.R")

source("Codigo/00_parametros.R")
library(readxl)

# Lectura de datos

# guess_max alto para que R lea bien la columna Fecha Defuncion (casi vacía)
activos    <- read_excel(archivo_poblacion, sheet = "Activos",    guess_max = 2e5)
fallecidos <- read_excel(archivo_poblacion, sheet = "Fallecidos", guess_max = 2e5)
inactivos  <- read_excel(archivo_poblacion, sheet = "Inactivos",  guess_max = 2e5)
mortalidad <- read.csv(archivo_mortalidad)   # columnas: edad, qx_masculino, qx_femenino

activos <- data.frame(
  id     = activos$`Id Asociado`,
  nac    = as.Date(activos$`Fecha Nacimiento`),
  defun  = as.Date(activos$`Fecha Defuncion`),
  sexo   = activos$Genero,
  cuotas = activos$`Cuotas Fondo Mutual`
)

# Edad al 31/12/2025 

# la fecha de referencia es 31 de diciembre, todos ya cumplieron años
# en 2025, así que la edad es simplemente 2025 - año de nacimiento.
activos$edad <- 2025 - as.integer(format(activos$nac, "%Y"))

# Exclusiones

# Cada registro se excluye por la PRIMERA causa que cumpla, en este orden, para no contar a nadie dos veces.
activos$causa <- NA
activos$causa[duplicated(activos$id)] <- "Id duplicado"
activos$causa[is.na(activos$causa) & is.na(activos$nac)] <- "Sin fecha de nacimiento"
activos$causa[is.na(activos$causa) & is.na(activos$sexo)] <- "Sin sexo"
activos$causa[is.na(activos$causa) & (activos$edad < 0 | activos$edad > 106)] <- "Edad fuera de 0 a 106"
activos$causa[is.na(activos$causa) & !is.na(activos$defun)] <- "Defunción informada"

activos_limp <- activos[is.na(activos$causa), ]
N0 <- nrow(activos_limp)

# Cuadro de registros: total, excluidos por causa y N0
causas <- c("Id duplicado", "Sin fecha de nacimiento", "Sin sexo",
            "Edad fuera de 0 a 106", "Defunción informada")
tabla_registros <- data.frame(
  Concepto  = c("Registros en Activos", paste("Excluido:", causas), "Población inicial N0"),
  Registros = c(nrow(activos), sapply(causas, function(x) sum(activos$causa == x, na.rm = TRUE)), N0)
)
print(tabla_registros)

# Revisión con las otras hojas: solo se reporta, no excluye a nadie
cat("Activos que también están en Fallecidos:", sum(activos$id %in% fallecidos$`Id Asociado`), "\n")
cat("Activos que también están en Inactivos: ", sum(activos$id %in% inactivos$`Id Asociado`), "\n")

# Cuotas

cat("\nAños equivalentes de aportación (cuotas / 12):\n")
print(summary(activos_limp$cuotas / 12))

# Cohortes por edad y sexo

cohortes <- aggregate(id ~ edad + sexo, data = activos_limp, FUN = length)
names(cohortes)[3] <- "N0"

# Probabilidades con muerte y renuncia actuando a la vez 
# Fuerza constante en el año:
# p = (1 - q'muerte)(1 - q'renuncia)
# q(j) = ln(1 - q'(j)) / ln(p) * (1 - p)
probabilidades <- function(edad, sexo, renuncia) {
  q_mort <- ifelse(sexo == "Masculino",
                   mortalidad$qx_masculino[match(edad, mortalidad$edad)],
                   mortalidad$qx_femenino[match(edad, mortalidad$edad)])
  p <- (1 - q_mort) * (1 - renuncia)
  data.frame(p = p,
             q_muerte   = log(1 - q_mort)   / log(p) * (1 - p),
             q_renuncia = log(1 - renuncia) / log(p) * (1 - p))
}

# Proyección de 5 años

# En el año t una cohorte de edad inicial a usa las probabilidades de la edad a + t - 1.
# Se deja como función porque la vamos a usar en la seccion 4 con renuncia del 6 %.

proyectar_poblacion <- function(cohortes, renuncia) {
  resultado <- NULL
  N_inicio <- cohortes$N0
  for (t in 1:5) {
    pr <- probabilidades(cohortes$edad + t - 1, cohortes$sexo, renuncia)
    muertes   <- N_inicio * pr$q_muerte
    renuncias <- N_inicio * pr$q_renuncia
    N_cierre  <- N_inicio * pr$p
    resultado <- rbind(resultado,
                       data.frame(edad = cohortes$edad, sexo = cohortes$sexo, t = t,
                                  N_inicio, muertes, renuncias, N_cierre))
    N_inicio <- N_cierre
  }
  resultado
}

proy_base <- proyectar_poblacion(cohortes, r_renuncia)

# Tabla anual de la cartera osea sumando todas las cohortes
pob_anual <- aggregate(cbind(N_inicio, muertes, renuncias, N_cierre) ~ t, data = proy_base, FUN = sum)
pob_anual$muertes_acum   <- cumsum(pob_anual$muertes)     # estado absorbente Fallecido
pob_anual$renuncias_acum <- cumsum(pob_anual$renuncias)   # estado absorbente Renuncia
print(pob_anual)

# Misma proyección con la matriz de transición 

# Estados: Activo, Renuncia, Fallecido. Las dos salidas son absorbentes.
matriz <- function(p, q_renuncia, q_muerte) {
  matrix(c(p, q_renuncia, q_muerte,
           0, 1,          0,
           0, 0,          1), nrow = 3, byrow = TRUE,
         dimnames = list(c("Activo", "Renuncia", "Fallecido"),
                         c("Activo", "Renuncia", "Fallecido")))
}

N_cierre_matriz <- NULL
for (k in 1:nrow(cohortes)) {
  estado <- c(cohortes$N0[k], 0, 0)
  for (t in 1:5) {
    pr <- probabilidades(cohortes$edad[k] + t - 1, cohortes$sexo[k], r_renuncia)
    estado <- estado %*% matriz(pr$p, pr$q_renuncia, pr$q_muerte)
    N_cierre_matriz <- rbind(N_cierre_matriz,
                             data.frame(edad = cohortes$edad[k], sexo = cohortes$sexo[k],
                                        t = t, activos = estado[1],
                                        renuncias_acum = estado[2], muertes_acum = estado[3]))
  }
} # Para cada cohorte: estado inicial (N0, 0, 0) y se multiplica por la matriz
# de la edad de cada año. Se guardan los activos al cierre de cada año.


# Comparación 
comparacion <- merge(proy_base, N_cierre_matriz, by = c("edad", "sexo", "t"))
cat("\nDiferencia máxima matriz vs. directa:",
    max(abs(comparacion$N_cierre - comparacion$activos)), "\n")

# Controles 

pr_todas <- probabilidades(proy_base$edad + proy_base$t - 1, proy_base$sexo, r_renuncia)

controles <- data.frame(
  Control = c("Suma de N0 por edad y sexo menos N0",
              "Máx. |p + q muerte + q renuncia - 1|",
              "Máx. |activos inicio - muertes - renuncias - activos cierre|",
              "Máx. diferencia proyección matricial vs. directa"),
  Valor = c(sum(cohortes$N0) - N0,
            max(abs(rowSums(pr_todas) - 1)),
            max(abs(proy_base$N_inicio - proy_base$muertes - proy_base$renuncias - proy_base$N_cierre)),
            max(abs(comparacion$N_cierre - comparacion$activos)))
)
print(controles)

controles$Valor <- formatC(controles$Valor, format = "e", digits = 1)
tabla_tex(controles, "A_controles")

# tabla informe

tabla_tex(tabla_registros, "A_registros", dig = 0)

tabla_tex(data.frame(`Año` = pob_anual$t,
                     `Activos al inicio` = pob_anual$N_inicio,
                     Fallecimientos = pob_anual$muertes,
                     Renuncias = pob_anual$renuncias,
                     `Activos al cierre` = pob_anual$N_cierre,
                     `Fallecidos acum.` = pob_anual$muertes_acum,
                     `Renuncias acum.` = pob_anual$renuncias_acum,
                     check.names = FALSE),
          "A_poblacion_anual", dig = c(0, 2, 2, 2, 2, 2, 2))

# Ejemplo de matriz: mujer de 45 años (prueba y luego podria servir para informe)
pr45 <- probabilidades(45, "Femenino", r_renuncia)
M45  <- matriz(pr45$p, pr45$q_renuncia, pr45$q_muerte)
tabla_tex(data.frame(Desde = rownames(M45), M45), "A_matriz_F45", dig = 6)

# Detalle completo por cohorte para los anexos
write.csv2(cohortes,  file.path(ruta_resultados, "A_cohortes.csv"),  row.names = FALSE)
write.csv2(proy_base, file.path(ruta_resultados, "A_proyeccion.csv"), row.names = FALSE)
