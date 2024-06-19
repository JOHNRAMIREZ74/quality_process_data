# global.R

# Cargar los paquetes necesarios
library(shiny)
library(shinydashboard)

# Verificar la existencia de setup.R antes de cargarlo
if (file.exists("setup.R")) {
  source("setup.R")
} else {
  stop("El archivo setup.R no se encuentra en el directorio de trabajo.")
}
