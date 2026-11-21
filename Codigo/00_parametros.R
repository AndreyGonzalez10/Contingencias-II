# En este archivo vamos a poner todos los parametros que se van a usar en los ejercicios
# con el fin de facilitar y terner mas ordenadas las cosas, por lo que todo se va 
# a tomar de aqui.
# Para usarlo los demás scripts lo cargan con  source("Codigo/00_parametros.R")

# Rutas

ruta_datos     <- "Datos"
ruta_salidas   <- file.path("Informe", "salidas")       # todo lo que exporta R
ruta_tablas    <- file.path(ruta_salidas, "tablas")      # .tex para Overleaf
ruta_figuras   <- file.path(ruta_salidas, "figuras")     # .pdf para Overleaf
ruta_resultados <- file.path(ruta_salidas, "resultados") # .csv intermedios

# Nombres de los datos

archivo_poblacion  <- file.path(ruta_datos, "Poblacion_CA-0309.xlsx")
archivo_mortalidad <- file.path(ruta_datos, "CA0309_Mortalidad_CR2023.csv")

# carpetas de salida 
for (r in c(ruta_salidas, ruta_tablas, ruta_figuras, ruta_resultados)) {
  dir.create(r, recursive = TRUE, showWarnings = FALSE)
}

# Base inicial (sección 1)

fecha_referencia <- as.Date("2025-12-31")  # edad en años cumplidos a esta fecha
edad_min         <- 0
edad_max         <- 106                    # edades admisibles: 0 a 106
horizonte        <- 5                      # años de proyección

# Parámetros del seguro colectivo de fallecimiento (secciones 1 y C)

r_renuncia <- 0.03       # renuncia anual de decremento único (asociada)
C_prima    <- 50000      # prima anual por contrato, al inicio del año
b_benef    <- 5000000    # beneficio por muerte, al cierre del año
e_gasto    <- 10000      # gasto anual por contrato vigente, al inicio
alpha_com  <- 0.05       # comisión sobre la prima cobrada, al inicio
k_adq      <- 75000      # gasto de adquisición por contrato, una sola vez (t = 0)
recursos_0 <- 100000     # recursos por contrato ANTES de adquisición: 100 000 N0
i_rend     <- 0.05       # rendimiento anual 
d_desc     <- 0.04       # tasa efectiva anual de descuento 
v_d <- 1 / (1 + d_desc)   # factor de descuento anual

# Reserva por contrato vigente al CIERRE de cada año (cuadro de la sección C).
# v_res[1] = inicio antes de emisión (t = 0), v_res[t + 1] = cierre del año t.
v_res <- c(0, 40000, 35000, 25000, 15000, 0)
names(v_res) <- paste0("t", 0:5)

# Escenarios del seguro colectivo
# Cada escenario es una lista con los parámetros que cambian.

esc_base <- list(nombre = "Base",
                 prima = C_prima, renuncia = r_renuncia,
                 gasto = e_gasto, rend = i_rend)

# para la seccion D: escenario adverso (misma demografía que la base)
esc_adverso <- modifyList(esc_base, list(nombre = "Adverso",
                                         gasto = 11000, rend = 0.04))

# para la Sección 4 : prima 55 000 y renuncia 6 %
esc_grupo2 <- modifyList(esc_base, list(nombre = "Grupo 2: prima 55 000 y renuncia 6%",
                                        prima = 55000, renuncia = 0.06))

# Varias vidas (sección B)

edad_min_pareja <- 30    # Ipareja(x) = 1 si x >= 30

# Diferencia de edad delta(x) según la edad inicial del activo

delta_edad <- function(x) {
  ifelse(x < 35, 0, ifelse(x < 55, 3, 5))
}
# Edad del cónyuge: hombre -> x - delta ; mujer -> x + delta

edad_conyuge <- function(x, sexo) {
  ifelse(sexo == "Masculino", x - delta_edad(x), x + delta_edad(x))
}

# Cuenta individual equity-linked (para la E)

A0_cuenta    <- 100000            # saldo inicial
c_cargo      <- 0.02              # cargo anual, deducido al INICIO del año
garantias    <- c(100000, 110000) # se comparan ambas con las mismas trayectorias
m_logrend    <- 0.04              # media del logrendimiento anual
sigma_logrend <- 0.15             # desviación del logrendimiento anual
n_trayect    <- 1000
semilla      <- 2026
edad_E       <- 45                # hombre representativo de 45 años
sexo_E       <- "Masculino"
n_cola       <- 50                # CTE95 = media de las 50 mayores pérdidas



# Formato de números para tablas (OJO OJOOOOOOOOOOO registrar IA esto, es para el informe)

fmt <- function(x, dig = 0) {
  formatC(x, format = "f", digits = dig, big.mark = " ")
}
# Exporta un data.frame como tabla LaTeX (booktabs) para usar con \input{} en Overleaf.
# Solo genera el tabular; el caption y el \begin{table} se ponen en Overleaf.
#   df     : data.frame con las columnas en el orden deseado
#   nombre : nombre del archivo sin extensión (se guarda en ruta_tablas)
#   dig    : decimales por columna numérica (un número o un vector)
#   alin   : alineación, p. ej. "lrrr"; por defecto l para texto y r para números
tabla_tex <- function(df, nombre, dig = 2, alin = NULL) {
  num <- vapply(df, is.numeric, logical(1))
  dig <- rep_len(dig, ncol(df))
  cuerpo <- df
  for (j in seq_along(df)) {
    if (num[j]) cuerpo[[j]] <- fmt(df[[j]], dig[j])
    cuerpo[[j]] <- gsub("%", "\\\\%", as.character(cuerpo[[j]]))
  }
  if (is.null(alin)) alin <- paste(ifelse(num, "r", "l"), collapse = "")
  enc <- gsub("%", "\\\\%", names(df))
  lineas <- c(paste0("\\begin{tabular}{", alin, "}"), "\\toprule",
              paste0(paste(enc, collapse = " & "), " \\\\"), "\\midrule",
              apply(cuerpo, 1, function(f) paste0(paste(f, collapse = " & "), " \\\\")),
              "\\bottomrule", "\\end{tabular}")
  writeLines(lineas, file.path(ruta_tablas, paste0(nombre, ".tex")), useBytes = TRUE)
  invisible(lineas)
}

# Por si algo no se carga bien
stopifnot("No se encuentra el Excel de población" =
            file.exists(archivo_poblacion),
          "No se encuentra el CSV de mortalidad" =
            file.exists(archivo_mortalidad))

message("Parámetros cargados. Horizonte: ", horizonte, " años; semilla E: ", semilla, ".")
