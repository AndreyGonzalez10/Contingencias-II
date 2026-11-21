# Parte C: flujos, reservas suministradas y profit testing
# De una la D (escenario adverso) y la sección 4 lo propio.
# Para correrlo: source("Codigo/parteC-D.R")
# Usa pob_anual, cohortes y proyectar_poblacion() de la parte A.

if (!exists("pob_anual")) source("Codigo/parteA.R")

# Función de profit testing

profit_test <- function(esc, pob) {
  flujos   <- NULL
  recursos <- NULL
  F_ant <- (recursos_0 - k_adq) * N0   # recursos iniciales después de adquisición

  for (t in 1:5) {
    N_ini <- pob$N_inicio[t]   # activos al cierre del año anterior
    D     <- pob$muertes[t]    # fallecimientos esperados del año
    N_fin <- pob$N_cierre[t]   # activos al cierre del año

    # Flujos del año (prima, gasto y comisión al inicio; beneficio al cierre)
    primas      <- esc$prima * N_ini
    gastos      <- esc$gasto * N_ini
    comisiones  <- alpha_com * primas
    beneficios  <- b_benef * D

    # Reservas: v_res[t] es la del cierre del año anterior, v_res[t + 1] la del cierre de este año
    V_ini <- unname(v_res[t]) * N_ini
    V_fin <- unname(v_res[t + 1]) * N_fin

    rendimiento <- (V_ini + primas - gastos - comisiones) * esc$rend
    resultado   <- V_ini + primas - gastos - comisiones + rendimiento - beneficios - V_fin

    # Resultado efectivo: solo el efectivo de las pólizas, sin movimientos de reserva
    efectivo <- (primas - gastos - comisiones) * (1 + esc$rend) - beneficios

    flujos <- rbind(flujos, data.frame(t, N_ini, N_fin, primas, gastos, comisiones,
                                       V_ini, rendimiento, beneficios, V_fin,
                                       resultado, efectivo))

    # Recursos, saldo anterior + flujos de inicio, rendimiento, menos beneficios
    F_tilde <- (F_ant + primas - gastos - comisiones) * (1 + esc$rend) - beneficios
    aporte  <- max(0, -F_tilde)   # solo si el saldo queda negativo
    F_fin   <- F_tilde + aporte
    recursos <- rbind(recursos, data.frame(t, F_ant, flujo_inicio = primas - gastos - comisiones,
                                           rend_recursos = (F_ant + primas - gastos - comisiones) * esc$rend,
                                           beneficios, F_tilde, aporte, F_fin,
                                           V_fin, respaldo = F_tilde - V_fin))
    F_ant <- F_fin
  }

  # NPV: adquisición una sola vez en t = 0; resultados al cierre de cada año
  NPV <- -k_adq * N0 + sum(flujos$resultado * v_d^flujos$t)
  # Margen sobre primas descontadas (primas al inicio del año, en t - 1)
  EPV_primas <- sum(flujos$primas * v_d^(flujos$t - 1))

  list(flujos = flujos, recursos = recursos,
       NPV = NPV, EPV_primas = EPV_primas, margen = NPV / EPV_primas,
       saldo_min = min(recursos$F_tilde), aportes = sum(recursos$aporte),
       respaldo1 = recursos$respaldo[1], activos5 = pob$N_cierre[5])
}

# Caso base (sección C)

base <- profit_test(esc_base, pob_anual)
print(base$flujos)
print(base$recursos)
cat("\nNPV base:", base$NPV, "\nMargen base:", base$margen, "\n")

# Control: resultado económico = efectivo + efecto de las reservas
efecto_reservas <- base$flujos$V_ini * (1 + esc_base$rend) - base$flujos$V_fin
cat("Máx. |resultado - efectivo - efecto reservas|:",
    max(abs(base$flujos$resultado - base$flujos$efectivo - efecto_reservas)), "\n")

# Escenario adverso (sección D): misma población, gasto 11 000 y rendimiento 4 %

adverso <- profit_test(esc_adverso, pob_anual)

# En la sección 4: prima 55 000 y renuncia 6 %
# La renuncia cambia, así que se recalcula la población con la función de la parte A

proy_g2 <- proyectar_poblacion(cohortes, esc_grupo2$renuncia)
pob_g2  <- aggregate(cbind(N_inicio, muertes, renuncias, N_cierre) ~ t, data = proy_g2, FUN = sum)
grupo2  <- profit_test(esc_grupo2, pob_g2)

# Tablas para el informe (montos en millones)

mill <- 1e6

tabla_tex(data.frame(`Año` = base$flujos$t,
                     Primas = base$flujos$primas / mill,
                     Gastos = base$flujos$gastos / mill,
                     Comisiones = base$flujos$comisiones / mill,
                     `Reserva inicial` = base$flujos$V_ini / mill,
                     Rendimiento = base$flujos$rendimiento / mill,
                     Beneficios = base$flujos$beneficios / mill,
                     `Reserva final` = base$flujos$V_fin / mill,
                     Resultado = base$flujos$resultado / mill,
                     Efectivo = base$flujos$efectivo / mill,
                     check.names = FALSE),
          "C_flujos_base", dig = c(0, rep(2, 9)))

tabla_tex(data.frame(`Año` = base$recursos$t,
                     `Saldo inicial` = base$recursos$F_ant / mill,
                     `Flujo de inicio` = base$recursos$flujo_inicio / mill,
                     Rendimiento = base$recursos$rend_recursos / mill,
                     Beneficios = base$recursos$beneficios / mill,
                     `Saldo antes de aporte` = base$recursos$F_tilde / mill,
                     Aporte = base$recursos$aporte / mill,
                     `Saldo final` = base$recursos$F_fin / mill,
                     `Reserva` = base$recursos$V_fin / mill,
                     check.names = FALSE),
          "C_recursos_base", dig = c(0, rep(2, 8)))

tabla_tex(data.frame(Indicador = c("NPV (millones de u.m.)",
                                   "VP de primas (millones de u.m.)",
                                   "Margen sobre primas descontadas"),
                     Valor = c(fmt(base$NPV / mill, 2), fmt(base$EPV_primas / mill, 2),
                               paste0(fmt(100 * base$margen, 2), "%"))),
          "C_indicadores_base")

# Comparación base / adverso (sección D)
tabla_tex(data.frame(`Indicador (millones de u.m.)` = c("NPV", "Saldo mínimo de recursos antes de aportes",
                                   "Total de aportes adicionales (5 años)",
                                   "Recursos menos reserva al cierre del año 1"),
                     Base    = c(base$NPV, base$saldo_min, base$aportes, base$respaldo1) / mill,
                     Adverso = c(adverso$NPV, adverso$saldo_min, adverso$aportes, adverso$respaldo1) / mill,
                     check.names = FALSE),
          "D_base_adverso", dig = c(0, 2, 2))

# Comparación base / escenario del grupo 2 (sección 4)
tabla_tex(data.frame(Indicador = c("Activos al cierre del año 5",
                                   "NPV (millones de u.m.)",
                                   "Margen sobre primas descontadas (%)",
                                   "Total de aportes (millones de u.m.)",
                                   "Recursos menos reserva, año 1 (millones de u.m.)"),
                     Base = c(base$activos5, base$NPV / mill, 100 * base$margen,
                              base$aportes / mill, base$respaldo1 / mill),
                     `Prima 55 000 y renuncia 6%` = c(grupo2$activos5, grupo2$NPV / mill, 100 * grupo2$margen,
                                                       grupo2$aportes / mill, grupo2$respaldo1 / mill),
                     check.names = FALSE),
          "S4_base_grupo2", dig = c(0, 2, 2))

# Descomposición del NPV base: adquisición, efectivo y efecto de las reservas
# NPV = -k N0 + VP(efectivo) + VP(V_ini (1 + i) - V_fin)
f <- base$flujos
vp_efectivo <- sum(f$efectivo * v_d^f$t)
vp_reservas <- sum((f$V_ini * (1 + esc_base$rend) - f$V_fin) * v_d^f$t)

descomp <- data.frame(Componente = c("Gasto de adquisición (t = 0)",
                                     "VP del resultado efectivo (5 años)",
                                     "VP del efecto de las reservas",
                                     "NPV"),
                      Millones = c(-k_adq * N0, vp_efectivo, vp_reservas, base$NPV) / mill)
descomp$`Por contrato inicial` <- descomp$Millones * mill / N0
print(descomp)
cat("Control: suma de componentes - NPV =", sum(descomp$Millones[1:3]) - descomp$Millones[4], "\n")
tabla_tex(descomp, "C_descomposicion_NPV", dig = c(0, 2, 0))

# Brecha por contrato: siniestro esperado contra prima neta de gasto y comisión
brecha <- data.frame(`Año` = f$t,
                     `Siniestro esperado por contrato` = f$beneficios / f$N_ini,
                     `Prima neta de gasto y comisión` = (f$primas - f$gastos - f$comisiones) / f$N_ini,
                     check.names = FALSE)
brecha$Faltante <- brecha$`Prima neta de gasto y comisión` - brecha$`Siniestro esperado por contrato`
print(brecha)
tabla_tex(brecha, "C_brecha_por_contrato", dig = 0)

# Detalle completo para los anexos
write.csv2(base$flujos,      file.path(ruta_resultados, "C_flujos_base.csv"),     row.names = FALSE)
write.csv2(base$recursos,    file.path(ruta_resultados, "C_recursos_base.csv"),   row.names = FALSE)
write.csv2(adverso$flujos,   file.path(ruta_resultados, "D_flujos_adverso.csv"),  row.names = FALSE)
write.csv2(adverso$recursos, file.path(ruta_resultados, "D_recursos_adverso.csv"), row.names = FALSE)
write.csv2(grupo2$flujos,    file.path(ruta_resultados, "S4_flujos_grupo2.csv"),  row.names = FALSE)
write.csv2(grupo2$recursos,  file.path(ruta_resultados, "S4_recursos_grupo2.csv"), row.names = FALSE)


resumen <- data.frame(Escenario = c("Base", "Adverso", "Grupo 2"),
                      NPV = c(base$NPV, adverso$NPV, grupo2$NPV) / mill,
                      Margen = c(base$margen, adverso$margen, grupo2$margen),
                      Saldo_min = c(base$saldo_min, adverso$saldo_min, grupo2$saldo_min) / mill,
                      Aportes = c(base$aportes, adverso$aportes, grupo2$aportes) / mill,
                      Respaldo1 = c(base$respaldo1, adverso$respaldo1, grupo2$respaldo1) / mill,
                      Activos5 = c(base$activos5, adverso$activos5, grupo2$activos5))
print(resumen)
