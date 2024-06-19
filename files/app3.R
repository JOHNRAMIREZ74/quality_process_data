# Código de la Aplicación Shiny
# Bloque 1. Preparación del entorno

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(dbplyr)
library(DBI)
library(RSQLite)

# Acceder a las variables de entorno
DB_PATH <- Sys.getenv("DB_PATH")

# Verificar si la variable de entorno está configurada
if (DB_PATH == "") {
  stop("La variable de entorno DB_PATH no está configurada.")
}

# Definir los inputs numéricos posibles
numeric_inputs <- c("o_w_ppm", "tss_mg_l", "ph", "cl_mg_l", "conduc_us_cm", 
                    "dureza_total_mg_l_caCO3", "hierro_total_mg_l", "bario_mg_l", 
                    "sulfato_mg_l")

# Bloque 2: Interfaz de usuaio
# UI
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

# Bloque 3: Server

server <- function(input, output, session) {
  user_role <- reactiveVal(NULL)
  
# Este bloque define la función server de la aplicación Shiny. La función toma tres argumentos: input, output y session. Dentro de la función, se define una variable reactiva user_role inicializada como NULL.

  # Bloque 3.1: Función para mostrar la página de login
  
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
  
  # Bloque 3.2:   Mostrar la página de login al inicio

    showLoginPage()
  # Este bloque llama a la función showLoginPage definida anteriormente, lo que significa que la página de inicio de sesión se mostrará al iniciar la aplicación.

  # Bloque 3.3: Observador para el botón de login
    
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
        output$body <- renderUI(
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
                      column(4, textInput("hora_muestra", "Hora Muestra (HH:MM)"))
                      ),
                    fluidRow(
                      column(4, selectInput("location", "Ubicación", choices = unique(dbGetQuery(con, "SELECT DISTINCT location FROM locations")$location))),
                      column(4, numericInput("o_w_ppm", "O/W (ppm)", value = NA, min = 0)),
                      column(4, numericInput("tss_mg_l", "TSS (mg/L)", value = NA, min = 0))
                      ),
                    fluidRow(
                      column(4, numericInput("ph", "pH", value = NA, min = 0, max = 14)),
                      column(4, numericInput("cl_mg_l", "Cl- (mg/L)", value = NA, min = 0)),
                      column(4, numericInput("conduc_us_cm", "CONDUC. (µS/cm)", value = NA, min = 0))
                      ),
                    fluidRow(
                      column(4, numericInput("dureza_total_mg_l_caCO3", "DUREZA TOTAL mg/L CaCO3", value = NA, min = 0)),
                      column(4, numericInput("hierro_total_mg_l", "HIERRO TOTAL (mg/L)", value = NA, min = 0)),
                      column(4, numericInput("bario_mg_l", "BARIO (mg/L)", value = NA, min = 0))
                      ),
                    fluidRow(
                      column(4, numericInput("sulfato_mg_l", "SULFATO (mg/L)", value = NA, min = 0)),
                      column(4, selectInput("analista", "Analista", choices = analysts)),
                      column(4, textAreaInput("observaciones", "Observaciones", ""))
                      ),
                    actionButton("submit", "Enviar"),
                    h4("Último registro"),
                    tableOutput("last_entry")
                    ),
            tabItem(tabName = "visualizations",
                    h2("Visualizaciones"),
                    fluidRow(
                      column(4, selectInput("var_select", "Seleccionar Variable", choices = numeric_inputs)),
                      column(4, selectInput("location_select", "Seleccionar Localización", choices = unique(dbGetQuery(con, "SELECT DISTINCT location FROM quality_process_data")$location))),
                      column(4, selectInput("graph_type", "Tipo de Gráfico", choices = c("Serie de Tiempo", "Boxplot", "Histograma"))
                             )),
                    fluidRow(
                      column(4, selectInput("year_select", "Seleccionar Año", choices = unique(format(as.Date(dbGetQuery(con, "SELECT DISTINCT fecha FROM quality_process_data")$fecha), "%Y")))),
                      column(4, selectInput("period_type", "Tipo de Periodo", choices = c("Trimestre", "Bimestre"))),
                      column(4, uiOutput("period_number_ui"))
                      ),
                    fluidRow(
                      column(12, plotOutput("plot"))
                      )
                    )
            )
          )
        } else {
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
  #     "Datos Existentes": muestra una tabla con los datos existentes en la base de datos.
  #     "Ingresar Datos": muestra un formulario para ingresar nuevos datos manualmente.
  #     "Visualizaciones": muestra controles para seleccionar una variable, ubicación, tipo de gráfico, año y período, y un área para graficar los datos.
  # Si no se encuentra un usuario con esas credenciales, muestra una notificación de error.
  
  
  # Bloque 3.4:   # Actualizar el UI de periodo según la selección de tipo de periodo

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
  
  # Bloque 3.5: Añadimos un observador para manejar la acción del botón de logout.
  
    observeEvent(input$logout_button, {
      user_role(NULL)
      showLoginPage()  # Función que muestra la página de inicio de sesión
      })
  # Este bloque define un observador que se activa cuando el usuario hace clic en el elemento de menú "Logout". Cuando se activa, establece la variable reactiva user_role en NULL (esencialmente, cierra la sesión del usuario) y muestra la página de inicio de sesión nuevamente llamando a la función showLoginPage().
  
  # Bloque 3.6: Subir datos manuales
  
    observeEvent(input$submit, { 
      
      # Validar la fecha
      if (is.na(as.Date(input$fecha))) {
        showNotification("Fecha inválida. Use el formato yyyy-mm-dd.", type = "error")
        return()
        }
      # Validar la hora
      if (!grepl("^\\d{2}:\\d{2}$", input$hora_muestra)) {
        showNotification("Hora inválida. Use el formato HH:MM.", type = "error")
        return()
        }
      # Validar los valores numéricos
      for (field in c("o_w_ppm", "tss_mg_l", "ph", "cl_mg_l", "conduc_us_cm", "dureza_total_mg_l_caCO3", "hierro_total_mg_l", "bario_mg_l", "sulfato_mg_l")) {
        if (!is.numeric(input[[field]]) || is.na(input[[field]])) {
          showNotification(paste("Valor inválido en el campo", field, "."), type = "error")
          return()
        }
        }
      # Si todas las validaciones pasan, inserte los datos en la base de datos
      query <- "INSERT INTO quality_process_data (id_muestra, fecha, hora_muestra, location, o_w_ppm, tss_mg_l, ph, cl_mg_l, conduc_us_cm, dureza_total_mg_l_caCO3, hierro_total_mg_l, bario_mg_l, sulfato_mg_l, analista, observaciones) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      dbExecute(con, query, params = list(input$id_muestra, as.character(input$fecha), input$hora_muestra, input$location, input$o_w_ppm, input$tss_mg_l, input$ph, input$cl_mg_l, input$conduc_us_cm, input$dureza_total_mg_l_caCO3, input$hierro_total_mg_l, input$bario_mg_l, input$sulfato_mg_l, input$analista, input$observaciones))
      showNotification("Datos ingresados exitosamente.", type = "success")
      output$last_entry <- renderTable({
        query <- "SELECT * FROM quality_process_data ORDER BY id DESC LIMIT 1"
        last_entry <- dbGetQuery(con, query)
        last_entry
        })
      })
  # Este bloque define un observador que maneja el evento de hacer clic en el botón "submit" en el formulario de ingreso de datos. Aquí está lo que hace:
  # Valida la fecha ingresada, asegurándose de que esté en el formato correcto.
  # Valida la hora ingresada, asegurándose de que esté en el formato correcto.
  # Valida los valores numéricos ingresados, asegurándose de que sean números y no sean NA.
  # Si todas las validaciones pasan, inserta los datos en la base de datos usando una consulta SQL.
  # Muestra una notificación de éxito si los datos se ingresaron correctamente.
  # Renderiza una tabla con el último registro ingresado en la base de datos.
  
  # Bloque 3.7: Renderizar gráficos en función de la selección del usuario
    output$plot <- renderPlot({
      req(input$var_select, input$location_select, input$graph_type, input$year_select, input$period_type, input$period_number)
    
    data <- dbGetQuery(con, paste0("SELECT fecha, ", input$var_select, " FROM quality_process_data WHERE location = ? AND strftime('%Y', fecha) = ?", collapse = ""), params = list(input$location_select, input$year_select))
    
    data$fecha <- as.Date(data$fecha)
    
    if (input$period_type == "Trimestre") {
      data <- data[quarter(data$fecha) == input$period_number, ]
      } else {
        data <- data[ceiling(month(data$fecha) / 2) == input$period_number, ]
        }
    if (nrow(data) == 0) {
      showNotification("No hay datos para el período seleccionado.", type = "warning")
      return(NULL)
      }
    if (input$graph_type == "Serie de Tiempo") {
      ggplot(data, aes(x = fecha, y = get(input$var_select))) +
        geom_line() +
        labs(title = paste("Serie de Tiempo de", input$var_select), x = "Fecha", y = input$var_select)
      } else if (input$graph_type == "Boxplot") {
        ggplot(data, aes(x = factor(1), y = get(input$var_select))) +
          geom_boxplot() +
          labs(title = paste("Boxplot de", input$var_select), x = input$var_select, y = input$var_select)
        } else if (input$graph_type == "Histograma") {
          ggplot(data, aes(x = get(input$var_select))) +
            geom_histogram(binwidth = 1) +
            labs(title = paste("Histograma de", input$var_select), x = input$var_select, y = "Frecuencia")
          }
    })
  # Este bloque define una salida reactiva output$plot que renderiza un gráfico en función de las selecciones del usuario.
  # Aquí está lo que hace:
  # Requiere que el usuario haya seleccionado una variable, una ubicación, un tipo de gráfico, un año, un tipo de período y un número de período.
  # Consulta la base de datos para obtener los datos correspondientes a la ubicación y el año seleccionados.
  # Convierte la columna "fecha" a un objeto de fecha.
  # Filtra los datos según el período seleccionado (trimestre o bimestre).
  # Si no hay datos para el período seleccionado, muestra una notificación de advertencia y no renderiza ningún gráfico.
  # Si hay datos, renderiza el gráfico seleccionado (serie de tiempo, boxplot o histograma) usando ggplot2.


  # Bloque 4:
    onSessionEnded(function() {
      dbDisconnect(con)
    })
    }

  # Bloque 5:
    shinyApp(ui, server)
# Este es el bloque final del código:
  # onSessionEnded(function() { dbDisconnect(con) }): Esta línea define una función que se ejecutará cuando la sesión de Shiny finalice. La función cierra la conexión a la base de datos SQLite utilizando dbDisconnect(con).
  # shinyApp(ui, server): Esta línea inicia la aplicación Shiny pasando los objetos ui y server definidos anteriormente. Esta es la línea final y es donde la aplicación Shiny comienza a ejecutarse.
# En resumen, este último bloque se asegura de cerrar la conexión a la base de datos cuando la aplicación Shiny se detenga y luego inicia la aplicación Shiny con la interfaz de usuario y la lógica del servidor definidas.




