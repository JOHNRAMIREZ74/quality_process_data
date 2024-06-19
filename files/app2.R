# Código de la Aplicación Shiny

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(dbplyr)
library(DBI)
library(RSQLite)
library(dbplyr)
library(digest)
library(lubridate)
library(dotenv)

# setup.R

library(DBI)
library(RSQLite)
library(dplyr)

# Instalar y cargar dotenv si no está instalado
if (!requireNamespace("dotenv", quietly = TRUE)) {
  install.packages("dotenv")
}
library(dotenv)

# Cargar variables de entorno
load_dot_env(file = ".env")

# Obtener la variable DB_PATH
DB_PATH <- Sys.getenv("DB_PATH")

# Verificar si la variable de entorno está configurada
if (is.null(DB_PATH) || DB_PATH == "") {
  stop("La variable de entorno DB_PATH no está configurada.")
}

# Conexión a la base de datos
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
  analysts <- reactiveVal(unique(dbGetQuery(con, "SELECT username FROM users")$name))
  
  cat("Datos iniciales cargados con éxito.\n")
}, error = function(e) {
  warning(paste("Error al cargar datos iniciales:", e$message))
  locations <- character(0)
  analysts <- character(0)
})

# ui.R

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "JR ENGINEERING"),
    dashboardSidebar(
      sidebarMenuOutput("menu")
    ),
    dashboardBody(
      uiOutput("body")
    )
  )
)
# Este bloque define la interfaz de usuario (UI) de la aplicación Shiny. Utiliza el diseño dashboardPage de shinydashboard y define un encabezado, una barra lateral y un cuerpo principal. La barra lateral contiene un elemento sidebarMenuOutput llamado "menu" que se renderizará dinámicamente. El cuerpo principal contiene un uiOutput llamado "body" que también se renderizará dinámicamente.

# Bloque 3. Server
server <- function(input, output, session) {
  user_role <- reactiveVal(NULL)
  
# Este bloque define la función server de la aplicación Shiny. La función toma tres argumentos: input, output y session. Dentro de la función, se define una variable reactiva user_role inicializada como NULL.

# Función para mostrar la página de login
showLoginPage <- function() {
  output$body <- renderUI({
    fluidRow(
      column(4, offset = 4,
             wellPanel(
               h3("Moriche CPF Process Quality Monitoring"),
               textInput("username", "User"),
               passwordInput("password", "Password"),
               actionButton("login_button", "Login"),
               p("Enter your username and password to access the system.")
               )
             )
      )
  })
  }
# Este bloque define una función llamada showLoginPage dentro de la función server. Esta función renderiza la página de inicio de sesión en el output$body de la aplicación. La página de inicio de sesión contiene un encabezado, un campo de entrada de texto para el nombre de usuario, un campo de entrada de contraseña y un botón de acción para iniciar sesión.

# Mostrar la página de login al inicio
showLoginPage()
# Este bloque llama a la función showLoginPage definida anteriormente, lo que significa que la página de inicio de sesión se mostrará al iniciar la aplicación.

# Bloque 3.3. Observador para el botón de login

# Bloque 3.3. Observador para el botón de login

observeEvent(input$login_button, {
  user <- input$username
  pass <- digest(input$password, algo = "sha256")
  user_info <- dbGetQuery(con, "SELECT role FROM users WHERE username = ? AND password = ?", params = list(user, pass))
  
  if (nrow(user_info) == 1) {
    user_role(user_info$role)
    output$menu <- renderMenu({
      sidebarMenu(
        menuItem("Datos Existentes", tabName = "home", icon = icon("database")),
        if (user_info$role %in% c("analyst", "admin")) {
          menuItem("Ingresar Datos", tabName = "form", icon = icon("edit"))
        },
        menuItem("Visualizaciones", tabName = "visualizations", icon = icon("chart-line")),
        menuItem("Logout", icon= icon("sign-out-alt"), tabName = "logout", actionButton("logout_button", "Logout"))
      )
    })
    
    output$body <- renderUI({
      tabItems(
        tabItem(tabName = "home",
                h2("Datos Existentes"),
                DTOutput("data_table"),
                p("Visualización de los datos existentes en la base de datos.")),
        tabItem(tabName = "form",
                h2("Ingresar Datos Manualmente"),
                fluidRow(
                  column(4, textInput("id_muestra", "ID Muestra")),
                  column(4, dateInput("fecha", "Fecha", format = "yyyy-mm-dd")),
                  column(4, textInput("hora_muestra", "Hora Muestra (HH:MM)"))),
                fluidRow(
                  column(4, selectInput("location", "Ubicación", choices = locations)),
                  column(4, numericInput("o_w_ppm", "O/W (ppm)", value = NA, min = 0)),
                  column(4, numericInput("tss_mg_l", "TSS (mg/L)", value = NA, min = 0))),
                fluidRow(
                  column(4, numericInput("ph", "pH", value = NA, min = 0, max = 14)),
                  column(4, numericInput("cl_mg_l", "Cl- (mg/L)", value = NA, min = 0)),
                  column(4, numericInput("conduc_us_cm", "CONDUC. (µS/cm)", value = NA, min = 0))),
                fluidRow(
                  column(4, numericInput("dureza_total_mg_l_caCO3", "DUREZA TOTAL mg/L CaCO3", value = NA, min = 0)),
                  column(4, numericInput("hierro_total_mg_l", "HIERRO TOTAL (mg/L)", value = NA, min = 0)),
                  column(4, numericInput("bario_mg_l", "BARIO (mg/L)", value = NA, min = 0))),
                fluidRow(
                  column(4, numericInput("sulfato_mg_l", "SULFATO (mg/L)", value = NA, min = 0)),
                  column(4, selectInput("analista", "Analista", choices = analysts)),
                  column(4, textAreaInput("observaciones", "Observaciones", ""))),
                actionButton("submit", "Enviar"),
                h4("Último registro"),
                tableOutput("last_entry")
        ),
        tabItem(tabName = "visualizations",
                h2("Visualizaciones"),
                fluidRow(
                  column(4, selectInput("var_select", "Seleccionar Variable", choices = numeric_inputs)),
                  column(4, selectInput("location_select", "Seleccionar Localización", choices = locations)),
                  column(4, selectInput("graph_type", "Tipo de Gráfico", choices = c("Serie de Tiempo", "Boxplot", "Histograma")))),
                fluidRow(
                  column(4, selectInput("year_select", "Seleccionar Año", choices = unique(format(as.Date(dbGetQuery(con, "SELECT DISTINCT fecha FROM quality_process_data")$fecha), "%Y")))),
                  column(4, selectInput("period_type", "Tipo de Periodo", choices = c("Trimestre", "Bimestre"))),
                  column(4, uiOutput("period_number_ui"))),
                fluidRow(
                  column(12, plotOutput("plot"))
                )
        )
      )
    })
  }
  else {
    showNotification("Usuario o contraseña incorrectos", type = "error")
  }
})

    
# Este es un bloque largo de código que maneja el evento de inicio de sesión. Aquí está lo que hace:
# Observa el evento de hacer clic en el botón "login_button".
# Obtiene el nombre de usuario y la contraseña ingresados por el usuario.
# Cifra la contraseña usando el algoritmo SHA-256.
# Consulta la base de datos para obtener el rol del usuario con el nombre de usuario y la contraseña proporcionados.
# Si se encuentra un único usuario con esas credenciales:
  #   Establece el rol del usuario en la variable reactiva user_role.
  #   Renderiza el menú lateral con diferentes elementos según el rol del usuario.
  #   Renderiza el cuerpo de la aplicación con diferentes pestañas:
    # "Datos Existentes": muestra una tabla con los datos existentes en la base de datos.
    # "Ingresar Datos": muestra un formulario para ingresar nuevos datos manualmente.
    # "Visualizaciones": muestra controles para seleccionar una variable, ubicación, tipo de gráfico, año y período, y un área para graficar los datos.
# Si no se encuentra un usuario con esas credenciales, muestra una notificación de error.

# Bloque 3.4. Actualizar el UI de periodo según la selección de tipo de periodo
output$period_number_ui <- renderUI({
  req(input$period_type)
  if (input$period_type == "Trimestre") {
    selectInput("period_number", "Número de Trimestre", choices = 1:4)
    } else {
      selectInput("period_number", "Número de Bimestre", choices = 1:6)
      }
  })

# Este bloque define una salida reactiva output$period_number_ui que renderiza un selectInput para seleccionar el número de período (trimestre o bimestre) según la selección de "Tipo de Periodo" realizada por el usuario.
# Si el usuario selecciona "Trimestre", se muestra un menú desplegable con opciones del 1 al 4 para seleccionar el número de trimestre. Si selecciona "Bimestre", se muestra un menú desplegable con opciones del 1 al 6 para seleccionar el número de bimestre.
# La función req(input$period_type) asegura que el código solo se ejecute cuando el usuario haya seleccionado un "Tipo de Periodo".

# Bloque 3.5. Añadimos un observador para manejar la acción del botón de logout.
observeEvent(input$logout_button, {
  user_role(NULL)
  showLoginPage()  # Función que muestra la página de inicio de sesión
})

# Este bloque define un observador que se activa cuando el usuario hace clic en el elemento de menú "Logout". Cuando se activa, establece la variable reactiva user_role en NULL (esencialmente, cierra la sesión del usuario) y muestra la página de inicio de sesión nuevamente llamando a la función showLoginPage().

# Bloque 3.6. Subir datos manuales
observeEvent(input$submit, {
# Validar la fecha
  if (is.na(as.Date(input$fecha))) {
    showNotification("Fecha inválida. Use el formato yyyy-mm-dd.", type = "error")
    return()
    }
# Validar la hora
  if (!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$hora_muestra)) {
    showNotification("Hora inválida. Use el formato HH:MM.", type = "error")
    return()
    }
# Actualizar la lista de localizaciones después de insertar nuevos datos
  locations <<- reactiveVal(unique(dbGetQuery(con, "SELECT DISTINCT location FROM locations")$location))
  updateSelectInput(session, "location", choices = locations)
  
# Este bloque define un observador que se activa cuando el usuario hace clic en el botón "Enviar" del formulario de ingreso de datos.
  # Valida que la fecha ingresada sea válida en el formato "yyyy-mm-dd". Si no es válida, muestra una notificación de error y sale de la función.
  # Valida que la hora ingresada sea válida en el formato "HH:MM". Si no es válida, muestra una notificación de error y sale de la función.
  # Actualiza la lista de ubicaciones (locations) obteniendo las ubicaciones distintas de la tabla "locations" de la base de datos.
  # Actualiza las opciones del selectInput de "Ubicación" con la lista de ubicaciones actualizada.


# Bloque 3.7. Validar y transformar puntos decimales
numeric_inputs <- c("o_w_ppm", "tss_mg_l", "ph", "cl_mg_l", "conduc_us_cm", "dureza_total_mg_l_caCO3", "hierro_total_mg_l", "bario_mg_l", "sulfato_mg_l")
for (input_name in numeric_inputs) {
  value <- input[[input_name]]
  if (!is.na(value)) {
    if (!grepl("^[0-9]*\\.?[0-9]+$", as.character(value))) {
      showNotification(paste("El valor de", input_name, "es inválido. Use punto como separador decimal."), type = "error")
      return()
    }
  }
}
# Este bloque de código valida y transforma los valores numéricos ingresados por el usuario en el formulario. Realiza los siguientes pasos:
  # Define un vector numeric_inputs con los nombres de las entradas numéricas del formulario.
  # Itera sobre cada nombre de entrada numérica en numeric_inputs.
  # Obtiene el valor ingresado por el usuario para esa entrada numérica.
  # Si el valor no es NA (es decir, el usuario ingresó un valor), verifica que el valor cumpla con el formato numérico utilizando una expresión regular ^[0-9]*\\.?[0-9]+$. Esta expresión regular permite números enteros y números decimales con punto como separador decimal.
  # Si el valor no cumple con el formato numérico esperado, muestra una notificación de error al usuario indicando que debe usar el punto como separador decimal y sale de la función.
# Este bloque de código se ejecuta antes de insertar los datos en la base de datos para garantizar que los valores numéricos ingresados por el usuario sean válidos y estén en el formato correcto.

# Bloque 3.8. Insertar datos en la base de datos
dbExecute(con, "
INSERT INTO quality_process_data (
          id_muestra, fecha, hora_muestra, location, o_w_ppm, tss_mg_l, ph, cl_mg_l, conduc_us_cm, dureza_total_mg_l_caCO3, hierro_total_mg_l, bario_mg_l, sulfato_mg_l, analista, observaciones
          ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", 
          params = list(input$id_muestra, 
                        format(input$fecha, "%Y-%m-%d"), # Formatear la fecha antes de insertarla a la DB
                        input$hora_muestra, 
                        input$location, 
                        input$o_w_ppm, 
                        input$tss_mg_l, 
                        input$ph, 
                        input$cl_mg_l, 
                        input$conduc_us_cm, 
                        input$dureza_total_mg_l_caCO3, 
                        input$hierro_total_mg_l, 
                        input$bario_mg_l, 
                        input$sulfato_mg_l, 
                        input$analista, 
                        input$observaciones))
# Este bloque inserta los datos ingresados por el usuario en el formulario en la tabla quality_process_data de la base de datos SQLite. Ejecuta una consulta SQL INSERT INTO con los valores de los campos del formulario como parámetros.
# Los valores de los campos se obtienen de las entradas del usuario (input$campo) y se pasan como una lista a la función dbExecute del paquete DBI.
})

# Bloque 3.9. Se genera una consulta para visualización del ultimo registro ingresado
new_data <- dbGetQuery(con, "SELECT * FROM quality_process_data")
new_data[is.na(new_data)] <- "NA"
new_data$fecha <- format(as.Date(new_data$fecha), "%Y-%m-%d") # Cambia el formato de la fecha
data <- reactiveVal(new_data)
last_entry <- reactiveVal(new_data[nrow(new_data), ])

output$last_entry <- renderTable({
  last_entry()
})
# Este bloque realiza las siguientes acciones:
  # Obtiene todos los datos de la tabla quality_process_data de la base de datos SQLite utilizando la función dbGetQuery.
  # Reemplaza cualquier valor NA en los datos obtenidos por la cadena "NA" utilizando una asignación con indexación lógica: new_data[is.na(new_data)] <- "NA".
  # Crea una variable reactiva llamada data y le asigna los datos obtenidos de la base de datos.
  # Crea una variable reactiva llamada last_entry y le asigna la última fila de los datos obtenidos.
  # Define una salida reactiva output$last_entry que renderiza una tabla con la última entrada utilizando la función renderTable y la variable reactiva last_entry.
# Este bloque se encarga de obtener los datos actualizados de la base de datos después de insertar una nueva entrada, y muestra la última entrada en una tabla en la interfaz de usuario.

# Bloque 3.10. 
updateTextInput(session, "id_muestra", value = "")
updateDateInput(session, "fecha", value = Sys.Date())
updateTextInput(session, "hora_muestra", value = "")
updateSelectInput(session, "location", selected = unique(dbGetQuery(con, "SELECT DISTINCT location FROM quality_process_data")$location)[1])
updateNumericInput(session, "o_w_ppm", value = NA)
updateNumericInput(session, "tss_mg_l", value = NA)
updateNumericInput(session, "ph", value = NA)
updateNumericInput(session, "cl_mg_l", value = NA)
updateNumericInput(session, "conduc_us_cm", value = NA)
updateNumericInput(session, "dureza_total_mg_l_caCO3", value = NA)
updateNumericInput(session, "hierro_total_mg_l", value = NA)
updateNumericInput(session, "bario_mg_l", value = NA)
updateNumericInput(session, "sulfato_mg_l", value = NA)
updateSelectInput(session, "analista", selected = "")
updateTextAreaInput(session, "observaciones", value = "")

# Este bloque de código actualiza los valores de los campos del formulario de ingreso de datos después de que se haya insertado una nueva entrada en la base de datos. Se realizan las siguientes acciones:
  # updateTextInput(session, "id_muestra", value = ""): Limpia el campo "ID Muestra" del formulario.
  # updateDateInput(session, "fecha", value = Sys.Date()): Establece el valor del campo "Fecha" del formulario a la fecha actual.
  # updateTextInput(session, "hora_muestra", value = ""): Limpia el campo "Hora Muestra" del formulario.
  # updateSelectInput(session, "location", selected = unique(dbGetQuery(con, "SELECT DISTINCT location FROM quality_process_data")$location)[1]): Selecciona la primera ubicación de la lista actualizada de ubicaciones en el campo "Ubicación" del formulario.
  # Establece todos los campos numéricos (o_w_ppm, tss_mg_l, ph, cl_mg_l, conduc_us_cm, dureza_total_mg_l_caCO3, hierro_total_mg_l, bario_mg_l, sulfato_mg_l) a NA (valor faltante).
  # updateSelectInput(session, "analista", selected = ""): Desmarca la selección del campo "Analista" del formulario.
  # updateTextAreaInput(session, "observaciones", value = ""): Limpia el campo "Observaciones" del formulario.
# Este bloque se encarga de dejar el formulario en un estado limpio y listo para ingresar una nueva entrada después de que se haya insertado la entrada anterior en la base de datos.

# Bloque 3.11. Modificado: Crea una variable reactiva para almacenar los datos de la tabla:
data_table_data <- reactiveVal({
  data <- dbGetQuery(con, "SELECT * FROM quality_process_data")
  data[is.na(data)] <- "NA"  # Reemplaza los valores NA con "NA"
  
  data$fecha <- format(as.Date(data$fecha), "%Y-%m-%d")  # Cambiar el formato de la fecha
  
})

# Modifica la definición de output$data_table para usar la variable reactiva:
output$data_table <- renderDT({
  datatable(data_table_data(),
            options = list(
              searching = TRUE,
              rownames = TRUE,
              pageLength = 10,
              scrollX = TRUE,
              scrollY = TRUE
            ),
            colnames = c("ID", "ID Muestra", "Fecha", "Hora Muestra", "Ubicación", "O/W (ppm)", "TSS (mg/L)", "pH", "Cl- (mg/L)", "Conduct. (µS/cm)", "Dureza Total (mg/L)", "Fe (mg/L)", "Ba (mg/L)", "SO4 (mg/L)", "Analista", "Observaciones", "Registro")
  )
})

# Actualiza la variable reactiva data_table_data:
observeEvent(input$submit, {
  data <- dbGetQuery(con, "SELECT * FROM quality_process_data")
  data$fecha <- format(as.Date(data$fecha), "%d-%m-%Y")  # Cambiar el formato de la fecha
  
# Ocultar la columna de identificación
  data <- data[-1]  # Eliminar la primera columna (ID)
  data_table_data(data)
})


# Este bloque define una salida reactiva output$data_table que renderiza una tabla interactiva (datatable) con los datos de la tabla quality_process_data de la base de datos.
  # Primero, obtiene los datos de la tabla quality_process_data utilizando dbGetQuery.
  # Luego, crea una tabla interactiva con los datos utilizando la función datatable del paquete DT.
  # Se configuran algunas opciones para la tabla, como habilitar la búsqueda, establecer la longitud de página en 10 filas y habilitar el desplazamiento horizontal y vertical.
  # También se asignan nombres más descriptivos a las columnas utilizando el argumento colnames.
# Esta tabla interactiva mostrará todos los datos existentes en la base de datos y permitirá al usuario explorarlos de manera más conveniente.

# Bloque 3.12.
output$plot <- renderPlot({
  req(input$var_select, input$location_select, input$graph_type, input$year_select, input$period_type, input$period_number)
  
  period_length <- ifelse(input$period_type == "Trimestre", 3, 2)
  period_number <- as.integer(input$period_number)
  year <- as.integer(input$year_select)
  var_select <- input$var_select
  
  data_to_plot <- dbGetQuery(con, sprintf("
     SELECT fecha, %s, location
     FROM quality_process_data
     WHERE location = '%s'
       AND strftime('%%Y', fecha) = '%d'
       AND ((strftime('%%m', fecha) - 1) / %d) + 1 = %d
     ORDER BY fecha;
   ", var_select, input$location_select, year, period_length, period_number))
  
  
  if (input$graph_type == "Serie de Tiempo") {
    ggplot(data_to_plot, aes(x = as.Date(fecha), y = get(input$var_select))) + 
      geom_line() + 
      geom_smooth(method = "loess") + 
      theme_minimal() +
      labs(title = paste("Serie de Tiempo de", input$var_select), x = "Fecha", y = input$var_select)
  } else if (input$graph_type == "Boxplot") {
    ggplot(data_to_plot, aes(x = location, y = get(input$var_select))) + 
      geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.5) + 
      theme_minimal() +
      labs(title = paste("Boxplot de", input$var_select, "por Localización"), x = "Localización", y = input$var_select)
  } else if (input$graph_type == "Histograma") {
    ggplot(data_to_plot, aes(x = get(input$var_select)), fill= "orange", color = "black") + 
      geom_histogram(binwidth = NULL, position = "identity", alpha = 0.5, bins = 30, fill = "darkgreen", color = "black") + 
      theme_minimal() +
      labs(title = paste("Histograma de", input$var_select), x = input$var_select, y = "Frecuencia")
  }
})
# Este bloque define una salida reactiva output$plot que renderiza un gráfico utilizando la librería ggplot2. El tipo de gráfico y los datos a graficar dependen de las selecciones realizadas por el usuario en la interfaz.
  # Primero, se verifica que el usuario haya realizado todas las selecciones necesarias (input$var_select, input$location_select, input$graph_type, input$year_select, input$period_type, input$period_number) utilizando la función req.
  # Se calcula la longitud del período (period_length) basada en si el usuario seleccionó "Trimestre" o "Bimestre".
  # Se obtiene el número de período (period_number) y el año (year) seleccionados por el usuario.
  # Se obtiene la variable a graficar (var_select) seleccionada por el usuario.
  # Se consulta la base de datos para obtener los datos necesarios para graficar, filtrando por la ubicación (location), el año y el período seleccionados.
  # Dependiendo del tipo de gráfico seleccionado por el usuario ("Serie de Tiempo", "Boxplot" o "Histograma"), se crea un objeto ggplot con los datos filtrados y se agregan las capas correspondientes (línea, suavizado, boxplot o histograma).
  # Se asignan etiquetas y títulos descriptivos al gráfico.
# Este bloque permite al usuario visualizar los datos de diferentes maneras según sus selecciones en la interfaz de usuario.

# Bloque 3.12:
onSessionEnded(function() {
  dbDisconnect(con)
})

}

# Bolque 4. Ejecuta la aplicación
shinyApp(ui, server)




