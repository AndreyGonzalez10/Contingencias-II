# Parte E: equity-linked y rentabilidad estocástica
# Para correrlo: source("Codigo/parteE.R")
# Usa probabilidades() de la parte A y los parámetros de la cuenta.

if (!exists("probabilidades")) source("Codigo/parteA.R")


p_anual <- probabilidades(edad_E + 0:4, rep(sexo_E, 5), r_renuncia)$p   # rep: ifelse() necesita el sexo del mismo largo que la edad
perm_inicio <- c(1, cumprod(p_anual)[1:4])   # vigente al INICIO del año t: (t-1)p
perm_5      <- prod(p_anual)                 # vigente al CIERRE del año 5: 5p


# Matriz de 1000 filas (trayectorias) por 5 columnas (años); se llena por columnas.
set.seed(semilla)
Z <- matrix(rnorm(n_trayect * 5), nrow = n_trayect, ncol = 5)
factor <- exp(m_logrend + sigma_logrend * Z)   # 1 + rendimiento de cada año

# Cuenta individual

# Cada año se cobra el cargo al inicio sobre el saldo, y luego se aplica el rendimiento.
# cargo_t = c * A_{t-1};  A_t = A_{t-1} (1 - c) * factor_t
A      <- matrix(0, n_trayect, 6)   # columna 1 = A0, columna t + 1 = saldo al cierre del año t
cargos <- matrix(0, n_trayect, 5)
A[, 1] <- A0_cuenta
for (t in 1:5) {
  cargos[, t] <- c_cargo * A[, t]
  A[, t + 1]  <- (A[, t] - cargos[, t]) * factor[, t]
}
A5 <- A[, 6]

# Valor presente de los cargos
vp_cargos <- as.vector(cargos %*% (perm_inicio * v_d^(0:4)))

# Resultados para cada garantía (mismas trayectorias y mismo cargo)

resultados_E <- NULL
npv_G <- list()
for (G in garantias) {
  complemento <- pmax(G - A5, 0)                    # se paga al final del año 5 si sigue vigente
  vp_compl    <- complemento * perm_5 * v_d^5
  npv         <- vp_cargos - vp_compl
  perdida     <- -npv
  npv_G[[paste0("G", G / 1000)]] <- npv

  resultados_E <- rbind(resultados_E, data.frame(
    Garantia          = G,
    VP_complemento    = mean(vp_compl),
    NPV_medio         = mean(npv),
    Activacion        = mean(complemento > 0),    # la garantía se activa
    Frec_perdida      = mean(npv < 0),            # el proveedor pierde
    CTE95             = mean(sort(perdida, decreasing = TRUE)[1:n_cola])
  ))
}
print(resultados_E)

# Comprobación: con mismo cargo y trayectorias, la garantía mayor no puede
# dar menos complemento ni más NPV en ninguna trayectoria
cat("\nRNG usado:", RNGkind()[1], "| semilla:", semilla, "\n")
cat("NPV con G = 110 000 nunca mayor que con G = 100 000:",
    all(npv_G$G110 <= npv_G$G100), "\n")
cat("Complemento con G = 110 000 nunca menor:",
    all(pmax(110000 - A5, 0) >= pmax(100000 - A5, 0)), "\n")

# Tablas para el informe

tabla_tex(data.frame(Indicador = c("VP medio del complemento",
                                   "NPV medio del proveedor",
                                   "Frecuencia de activación de la garantía",
                                   "Frecuencia de pérdida (NPV < 0)",
                                   "CTE95 (media de las 50 mayores pérdidas)"),
                     `G = 100 000` = c(fmt(resultados_E$VP_complemento[1], 2), fmt(resultados_E$NPV_medio[1], 2),
                                       paste0(fmt(100 * resultados_E$Activacion[1], 1), "%"),
                                       paste0(fmt(100 * resultados_E$Frec_perdida[1], 1), "%"),
                                       fmt(resultados_E$CTE95[1], 2)),
                     `G = 110 000` = c(fmt(resultados_E$VP_complemento[2], 2), fmt(resultados_E$NPV_medio[2], 2),
                                       paste0(fmt(100 * resultados_E$Activacion[2], 1), "%"),
                                       paste0(fmt(100 * resultados_E$Frec_perdida[2], 1), "%"),
                                       fmt(resultados_E$CTE95[2], 2)),
                     check.names = FALSE),
          "E_comparacion_garantias")

# Detalle de una trayectoria para verificar cuenta, cargos, complemento y NPV.
# Se toma la primera donde la garantía se activa en ambos casos (saldo final < 100 000).
k <- which(A5 < garantias[1])[1]
cat("\nTrayectoria de detalle:", k, "\n")
tabla_tex(data.frame(`Año` = 1:5,
                     Z = Z[k, ],
                     `Saldo inicial` = A[k, 1:5],
                     Cargo = cargos[k, ],
                     `Factor` = factor[k, ],
                     `Saldo final` = A[k, 2:6],
                     `Perm. al inicio` = perm_inicio,
                     `VP cargo` = cargos[k, ] * perm_inicio * v_d^(0:4),
                     check.names = FALSE),
          "E_trayectoria_detalle", dig = c(0, 4, 2, 2, 4, 2, 6, 2))

detalle_k <- data.frame(Concepto = c("Saldo al cierre del año 5", "Permanencia 5p",
                                     "VP de los cargos",
                                     "Complemento G = 100 000", "VP complemento G = 100 000", "NPV G = 100 000",
                                     "Complemento G = 110 000", "VP complemento G = 110 000", "NPV G = 110 000"),
                        Valor = c(A5[k], perm_5, vp_cargos[k],
                                  max(100000 - A5[k], 0), max(100000 - A5[k], 0) * perm_5 * v_d^5,
                                  npv_G$G100[k],
                                  max(110000 - A5[k], 0), max(110000 - A5[k], 0) * perm_5 * v_d^5,
                                  npv_G$G110[k]))
print(detalle_k)
tabla_tex(detalle_k, "E_trayectoria_detalle_resumen", dig = 4)

# Histograma comparativo con las mismas escalas

cortes <- seq(floor(min(unlist(npv_G)) / 1000) * 1000,
              ceiling(max(unlist(npv_G)) / 1000) * 1000, by = 1000)
h1 <- hist(npv_G$G100,  breaks = cortes, plot = FALSE)
h2 <- hist(npv_G$G110, breaks = cortes, plot = FALSE)
y_max <- max(h1$counts, h2$counts)

pdf(file.path(ruta_figuras, "E_histograma_NPV.pdf"), width = 8, height = 3.6)
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 1))
plot(h1, col = "gray75", border = "gray30", ylim = c(0, y_max),
     main = "G = 100 000", xlab = "NPV del proveedor (u.m.)", ylab = "Trayectorias")
abline(v = 0, lwd = 2)
abline(v = mean(npv_G$G100), lty = 2, lwd = 2)
plot(h2, col = "gray75", border = "gray30", ylim = c(0, y_max),
     main = "G = 110 000", xlab = "NPV del proveedor (u.m.)", ylab = "Trayectorias")
abline(v = 0, lwd = 2)
abline(v = mean(npv_G$G110), lty = 2, lwd = 2)
dev.off()

# Detalle completo para los anexos
write.csv2(data.frame(trayectoria = 1:n_trayect, Z, A5, vp_cargos,
                      npv_G100 = npv_G$G100, npv_G110 = npv_G$G110),
           file.path(ruta_resultados, "E_simulacion.csv"), row.names = FALSE)
