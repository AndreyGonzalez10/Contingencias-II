# Parte B: varias vidas (parejas hipotéticas)
# Para correrlo: source("Codigo/parteB.R")
# Usa cohortes y mortalidad de la parte A.

if (!exists("cohortes")) source("Codigo/parteA.R")

# Supuesto menores de 30 sin pareja; de 30 o más con una pareja hipotética de sexo opuesto. La edad del cónyuge se fija al inicio y se mantiene la diferencia los 5 años.
parejas <- cohortes[cohortes$edad >= edad_min_pareja, ]
parejas$sexo_cony <- ifelse(parejas$sexo == "Masculino", "Femenino", "Masculino")
parejas$edad_cony <- edad_conyuge(parejas$edad, parejas$sexo)

# Exclusión cónyuge con edad inicial mayor a 106

excluidos_B <- parejas[parejas$edad_cony > edad_max, ]
parejas     <- parejas[parejas$edad_cony <= edad_max, ]

tabla_parejas <- data.frame(
  Concepto = c("Población inicial N0",
               "Sin pareja (menores de 30)",
               "Con pareja asignada (30 o más)",
               "Excluidos de B: cónyuge mayor de 106",
               "Con pareja admisible en B"),
  Activos = c(N0,
              sum(cohortes$N0[cohortes$edad < edad_min_pareja]),
              sum(cohortes$N0[cohortes$edad >= edad_min_pareja]),
              sum(excluidos_B$N0),
              sum(parejas$N0))
)
print(tabla_parejas)

# Probabilidades de supervivencia (solo mortalidad, sin renuncia)


# Se reutiliza probabilidades() de la parte A con renuncia = 0:
# así p = 1 - q_x, la supervivencia biológica de un año.
# tpx = (1 - q_x)(1 - q_x+1)...(1 - q_x+t-1)

resultado_B <- NULL
tpx <- 1   # supervivencia del activo
tpy <- 1   # supervivencia del cónyuge
for (t in 1:5) {
  tpx <- tpx * probabilidades(parejas$edad + t - 1, parejas$sexo, 0)$p
  tpy <- tpy * probabilidades(parejas$edad_cony + t - 1, parejas$sexo_cony, 0)$p

  # Independencia entre las dos vidas
  ambos     <- tpx * tpy             # vida conjunta
  al_menos1 <- tpx + tpy - tpx * tpy # último sobreviviente
  cony_sobr <- tpy - tpx * tpy       # cónyuge vivo con el activo fallecido

  resultado_B <- rbind(resultado_B,
                       data.frame(edad = parejas$edad, sexo = parejas$sexo,
                                  edad_cony = parejas$edad_cony, N0 = parejas$N0, t = t,
                                  tpx, tpy, ambos, al_menos1, cony_sobr))
}

# Rentas temporales: pagan 1 al cierre de cada año en que se cumple el estatus
# a = suma de v^t * probabilidad del estatus al cierre del año t
resultado_B$desc <- v_d^resultado_B$t   # v_d de los parámetros (4 %)
rentas <- aggregate(cbind(r_ambos = desc * ambos,
                          r_al_menos1 = desc * al_menos1,
                          r_cony = desc * cony_sobr) ~ edad + sexo + edad_cony + N0,
                    data = resultado_B, FUN = sum)

# Valor presente agregado: renta por pareja por número de activos de esa cohorte
vp_B <- data.frame(
  Estatus = c("Ambos vivos (vida conjunta)",
              "Al menos uno vivo (último sobreviviente)",
              "Cónyuge vivo con activo fallecido (cónyuge sobreviviente)"),
  `VP agregado` = c(sum(rentas$N0 * rentas$r_ambos),
                    sum(rentas$N0 * rentas$r_al_menos1),
                    sum(rentas$N0 * rentas$r_cony)),
  check.names = FALSE
)
vp_B$`VP promedio por pareja` <- vp_B$`VP agregado` / sum(parejas$N0)
print(vp_B)

# Controles

# Al menos uno vivo = ambos vivos + solo el cónyuge vivo + solo el activo vivo
solo_activo <- resultado_B$tpx - resultado_B$ambos
cat("\nMáx. |al menos uno - ambos - cónyuge sobreviviente - solo activo|:",
    max(abs(resultado_B$al_menos1 - resultado_B$ambos - resultado_B$cony_sobr - solo_activo)), "\n")
cat("Probabilidades entre 0 y 1:",
    all(resultado_B[, c("ambos", "al_menos1", "cony_sobr")] >= 0 &
        resultado_B[, c("ambos", "al_menos1", "cony_sobr")] <= 1), "\n")

# Tablas para el informe

tabla_tex(tabla_parejas, "B_parejas", dig = 0)
tabla_tex(vp_B, "B_valores_presentes", dig = c(0, 0, 4))

# Ejemplo de edades asignadas y probabilidades al cierre del año 5
ej <- resultado_B[resultado_B$t == 5 & resultado_B$edad %in% c(30, 40, 60), ]
ej <- ej[order(ej$sexo, ej$edad), ]
tabla_tex(data.frame(Sexo = ej$sexo, `Edad x` = ej$edad, `Edad y` = ej$edad_cony,
                     `5px` = ej$tpx, `5py` = ej$tpy, Ambos = ej$ambos,
                     `Al menos uno` = ej$al_menos1, `Cónyuge sobrev.` = ej$cony_sobr,
                     check.names = FALSE),
          "B_ejemplo_probabilidades", dig = c(0, 0, 0, 6, 6, 6, 6, 6))

# Detalle completo para los anexos
write.csv2(resultado_B, file.path(ruta_resultados, "B_probabilidades.csv"), row.names = FALSE)
write.csv2(rentas,      file.path(ruta_resultados, "B_rentas_por_cohorte.csv"), row.names = FALSE)
