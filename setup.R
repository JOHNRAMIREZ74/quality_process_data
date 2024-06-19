library(DBI)
library(RSQLite)
library(dplyr)
library(dotenv)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dbplyr)
library(digest)
library(lubridate)
library(rsconnect)



if (!require("futile.logger")) {
  install.packages("futile.logger")
  library("futile.logger")
}

# Instalar y cargar dotenv si no está instalado
if (!requireNamespace("dotenv", quietly = TRUE)) {
  install.packages("dotenv")
}
library(dotenv)

# Cargar variables de entorno
load_dot_env(file = ".env")

# Configurar la zona horaria
Sys.setenv(TZ = Sys.getenv("TZ", "America/Bogota"))

# Obtener las variables de entorno necesarias
DB_PATH <- Sys.getenv("DB_PATH")
DF_PATH <- Sys.getenv("DF_PATH")
WD_PATH <- Sys.getenv("WD_PATH")


# Verificar si la variable de entorno está configurada
if (is.null(DB_PATH) || DB_PATH == "") {
  stop("La variable de entorno DB_PATH no está configurada.")
}

tryCatch({
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  dbListTables(con)
  cat("Conexión a la base de datos establecida con éxito.\n")
}, error = function(e) {
  stop(paste("Error al conectar con la base de datos:", e$message))
})

# Definir los inputs numéricos posibles
numeric_inputs <- c("o_w_ppm", "tss_mg_l", "ph", "cl_mg_l", "conduc_us_cm",
                    "dureza_total_mg_l_caCO3", "hierro_total_mg_l", "bario_mg_l",
                    "sulfato_mg_l")

# Cargar datos iniciales
tryCatch({
  locations <- reactiveVal(unique(dbGetQuery(con, "SELECT DISTINCT location FROM locations")$location))
  year <- reactiveVal(unique(dbGetQuery(con, "SELECT DISTINCT fecha FROM quality_process_data")$year))
  analysts <- reactiveVal(unique(dbGetQuery(con, "SELECT username FROM users")$username))

  cat("Datos iniciales cargados con éxito.\n")
}, error = function(e) {
  warning(paste("Error al cargar datos iniciales:", e$message))
  locations <- character(0)
  analysts <- character(0)
})

