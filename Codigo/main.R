# main.R: corre todo el proyecto en orden
# Abrir Contingencias-II.Rproj y ejecutar: source("Codigo/main.R")
# Requiere el paquete readxl. Lenguaje: R; semilla de la simulación: 2026.

source("Codigo/00_parametros.R")
source("Codigo/parteA.R")    # población, decrementos y multiestado
source("Codigo/parteB.R")    # varias vidas
source("Codigo/parteC-D.R")  # profit testing, escenario adverso y escenario del grupo 2
source("Codigo/parteE.R")    # equity-linked y simulación

message("Listo: tablas en Informe/salidas/tablas, figuras en Informe/salidas/figuras")
