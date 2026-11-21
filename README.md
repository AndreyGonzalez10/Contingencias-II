# Proyecto final CA-0309 Contingencia de Vida II — Grupo 2

Universidad de Costa Rica, II ciclo 2026. Prof. Yerling Ramírez Morera.
Rol del grupo: Auditoría o Control (ronda 1).

Proyección a cinco años de un seguro colectivo de fallecimiento y de una cuenta
equity-linked para la población del Fondo mutual, según la guía del grupo 2.

## Cómo correr el proyecto

1. Instalar el paquete `readxl`:
   ```r
   install.packages("readxl")
   ```
2. Abrir el proyectoen `Contingencias-II.Rproj`. Así el directorio de
   trabajo queda en la raíz del repositorio.
3. Ejecutar en la consola:
   ```r
   source("Codigo/main.R")
   ```
   Corre todas las partes en orden y exporta tablas, figuras y resultados.

## Estructura

```
Codigo/
├── 00_parametros.R   Parámetros de la guía (sección 1), rutas y funciones comunes
├── parteA.R          A: limpieza de la base, exclusiones, N0, decrementos y matriz de transición
├── parteB.R          B: varias vidas (parejas hipotéticas y tres rentas)
├── parteC-D.R        C: profit testing base; D: escenario adverso; sección 4: escenario del grupo 2
├── parteE.R          E: cuenta equity-linked y simulación (G = 100 000 y 110 000)
└── main.R            Corre todo en orden
Datos/
├── Poblacion_CA-0309.xlsx         Base suministrada (hojas Activos, Fallecidos, Inactivos)
└── CA0309_Mortalidad_CR2023.csv   Mortalidad Costa Rica 2023 por edad y sexo
Informe/
└── salidas/
    ├── tablas/       Tablas .tex generadas por R (se incluyen en el informe con \input{})
    ├── figuras/      Figuras .pdf generadas por R
    └── resultados/   Detalle en .csv (separador ;) para los anexos
Instrucciones/        Guía del proyecto
```

## Parámetros

Todos los parámetros del ejercicio están en `Codigo/00_parametros.R` (prima, beneficio,
gastos, comisión, adquisición, rendimiento, descuento, reservas por contrato, parámetros
de la cuenta y de la simulación). Los escenarios se definen ahí como listas:
`esc_base`, `esc_adverso` (sección D) y `esc_grupo2` (prima 55 000 y renuncia 6 %).

## Reproducibilidad

- Lenguaje: R. Simulación con el generador por defecto de R (Mersenne-Twister),
  semilla 2026, 1 000 trayectorias de cinco años.
- Las tablas se exportan desde el código; ningún resultado se edita a mano.
- Fecha de referencia para las edades: 31/12/2025.

## Resultados principales (caso base)

- Población inicial utilizada: N0 = 104 004 activos.
- NPV del seguro colectivo: −12 912.86 millones de u.m.; margen sobre primas descontadas: −57.88 %.
- Equity-linked: NPV medio de 4 054.40 con G = 100 000 y de 1 093.46 con G = 110 000.
