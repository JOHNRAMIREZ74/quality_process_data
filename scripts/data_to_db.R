# Script de Preparación (Preprocessing Script)
# El script de preparación se encargará de todas las tareas relacionadas con la configuración inicial de la base de datos, como la creación de tablas y la inserción de datos predeterminados.

library(dplyr)
library(DBI)
library(RSQLite)
library(digest)
library(dotenv)

# Cargar variables de entorno
load_dot_env(file = ".env")

# Acceder a las variables
DB_PATH <- Sys.getenv("DB_PATH")
DF_PATH <- Sys.getenv("DF_PATH")

# Verificar si la variable de entorno está configurada
if (DB_PATH == "") {
  stop("La variable de entorno DB_PATH no está configurada.")
}

# Cargar el dataframe
load(DF_PATH)

# Crear la conexión a la base de datos SQLite
con <- dbConnect(RSQLite::SQLite(), DB_PATH)

# Eliminar la tabla si existe
dbExecute(con, "DROP TABLE IF EXISTS quality_process_data")

# Crear la tabla si no existe
dbExecute(con, "
CREATE TABLE IF NOT EXISTS quality_process_data (
    id INTEGER PRIMARY KEY,
    id_muestra TEXT,
    fecha DATE,
    hora_muestra TEXT,
    location TEXT,
    o_w_ppm REAL,
    tss_mg_l REAL,
    ph REAL,
    cl_mg_l REAL,
    conduc_us_cm REAL,
    dureza_total_mg_l_caCO3 REAL,
    hierro_total_mg_l REAL,
    bario_mg_l REAL,
    sulfato_mg_l REAL,
    analista TEXT,
    observaciones TEXT,
    fecha_registro TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)")

# Crear la tabla de usuarios si no existe
dbExecute(con, "
CREATE TABLE IF NOT EXISTS users (
    username TEXT PRIMARY KEY,
    password TEXT,
    role TEXT
)")

# Usuarios por defecto
users <- data.frame(
  username = c("admin", "analyst", "operator"),
  password = sapply(c("admin123", "analyst123", "operator123"), digest::digest, algo = "sha256"),
  role = c("admin", "analyst", "operator")
)

# Insertar usuarios por defecto si no existen
existing_users <- dbGetQuery(con, "SELECT username FROM users")
new_users <- users[!users$username %in% existing_users$username,]
if (nrow(new_users) > 0) {
  dbWriteTable(con, "users", new_users, append = TRUE, row.names = FALSE)
}

# Crear la tabla de localizaciones si no existe
dbExecute(con, "
CREATE TABLE IF NOT EXISTS locations (
    id INTEGER PRIMARY KEY,
    location TEXT
)")

# Definir las ubicaciones específicas
locations <- c("ECOTECNIA (Out TK-W-01)", "ECOTECNIA (Out TK-W-02)", "ECOTECNIA (Out TK-W-03)", 
               "ECOTECNIA (Out TK-WT-01)", "ECOTECNIA (Out TK-WT-02)", "101-A (ABARCO)", "102-A (ABARCO)",
               "AGUA CRUDA", "AGUA CRUDA DE POZO", "AGUA DOMINION (Out TK-WT03)", "AGUA ECOTECNIA (Out TK-WT01)",
               "AGUA FILTRADA (GRIFO)", "AGUA FILTRADA DEPURAR", "AGUA FILTRADA EDOSPINA", "BOMBEO", "FWKO 250 B",
               "GENERADORES", "IN CAPTACION DEPURAR.", "IN CAPTACION EDOSPINA.", "IN FIL", "IN FILTRO",  
               "IN FILTRO A", "IN FILTRO C", "IN SK", "OUT 101-A (ABARCO)", "OUT 101-A / 102-A (ABARCO)", 
               "OUT 102-A (ABARCO)", "OUT FIL", "OUT FILTRO", "OUT FILTRO A", "OUT MTB", "OUT SUAVIZADA",
               "OUT TTO", "OUT-TKWT-03", "P DEPURAR # 1", "P DEPURAR # 2", "P DEPURAR # 3", "PLANTA 101 EIS ABA",
               "PLANTA 102 EIS ABA", "PLANTA DEP 3 B-1", "PLANTA DEP 3 B-2", "PLANTA DEPURAR 1", "PLANTA DEPURAR 2",
               "PLANTA DEPURAR 3", "PLANTA DEPURAR 3 - BANCO-1", "PLANTA DEPURAR 3 - BANCO-2", "PLANTA DEPURAR 3 B-1",
               "PLANTA DEPURAR 3 B2", "PLANTA DEPURAR 3 BANCO 1", "PLANTA DEPURAR 3 BANCO 2", "PLANTA EDOSPINA 2",
               "PLANTA EDOSPINA 1", "PLANTA EDOSPINA 2", "POCETA (ABARCO) PH2", "TK WT 03 (DOMINION)",  
               "TK WT-03", "TK-103 (DOMINION)", "TK-252", "TK-3-WT (DOMINION)", "TK-5-02", "TK-503-WT (DOMINION)",
               "TK-7-03", "TK-W-01 (ECOTECNIA)", "TK-W03", "TK-WT-01 (ECOTECNIA)", "TK-WT-02", 
               "TK-WT-02 (ECOTECNIA)", "TK-WT-503", "TK-WT03 (DOMINION)", "TK-WT503")

# Insertar localizaciones por defecto si no existen
existing_locations <- dbGetQuery(con, "SELECT location FROM locations")
new_locations <- locations[!locations %in% existing_locations$location]
if (length(new_locations) > 0) {
  data_frame_locations <- data.frame(location = new_locations,
                                     stringsAsFactors = FALSE)
  dbWriteTable(con, "locations", data_frame_locations, append = TRUE, row.names = FALSE)
}

# Transformar datos para coincidir con la estructura de la base de datos
data_to_insert <- dataqa_db %>%
  mutate(
    fecha = as.character(fecha),
    hora_muestra = hora, # Convertir hora a formato "HH:MM:SS"
    cl_mg_l = Cl,
    conduc_us_cm = conductance,
    dureza_total_mg_l_caCO3 = CaCO3,
    hierro_total_mg_l = Fe,
    bario_mg_l = Ba,
    sulfato_mg_l = SO4,
    location = muestra,
    o_w_ppm = o.w,
    tss_mg_l = tss,
    fecha_registro = as.character(datetime) # Usar datetime como fecha_registro
  ) %>%
  select(
    id_muestra, fecha, hora_muestra, location, o_w_ppm, tss_mg_l, ph, cl_mg_l, conduc_us_cm, 
    dureza_total_mg_l_caCO3, hierro_total_mg_l, bario_mg_l, sulfato_mg_l, analista, 
    observaciones, fecha_registro
  )

# Crear la consulta de inserción
query <- "
INSERT INTO quality_process_data (
  id_muestra, fecha, hora_muestra, location, o_w_ppm, tss_mg_l, ph, cl_mg_l, conduc_us_cm, 
  dureza_total_mg_l_caCO3, hierro_total_mg_l, bario_mg_l, sulfato_mg_l, analista, 
  observaciones, fecha_registro
) VALUES (
  :id_muestra, :fecha, :hora_muestra, :location, :o_w_ppm, :tss_mg_l, :ph, :cl_mg_l, :conduc_us_cm, 
  :dureza_total_mg_l_caCO3, :hierro_total_mg_l, :bario_mg_l, :sulfato_mg_l, :analista, 
  :observaciones, :fecha_registro
)
"

# Preparar la consulta
stmt <- dbSendStatement(con, query)

# Ejecutar la consulta para cada fila
for (i in 1:nrow(data_to_insert)) {
  dbBind(stmt, as.list(data_to_insert[i, ]))
}

# Confirmar la transacción
dbClearResult(stmt)

# Cerrar la conexión
dbDisconnect(con)
