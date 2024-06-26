library(DBI)
library("futile.logger")
library(shiny)


# Server
server <- function(input, output, session) {

  user_role <- reactiveVal(NULL)

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


  showLoginPage ()

  observeEvent(input$login_button, {
    user <- input$username
    pass <- digest(input$password, algo = "sha256")
    query <- "SELECT role FROM users WHERE username = ? AND password = ?"
    user_info <- dbGetQuery(con, query, params = list(user, pass))

    if (nrow(user_info) == 1) {
      flog.info(sprintf("Usuario %s ha iniciado sesión exitosamente", user))
      user_role(user_info$role)
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Datos Existentes", tabName = "home", icon = icon("database")),
          if (user_info$role %in% c("analyst", "admin")) {
            menuItem("Ingresar Datos", tabName = "form", icon = icon("edit"))
          },
          menuItem("Visualizaciones", tabName = "visualizations", icon = icon("chart-line")),
          menuItem("Logout", icon = icon("sign-out-alt"), tabName = "logout", actionButton("logout_button", "Logout"))
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
                    column(4, textInput("hora_muestra", "Hora Muestra (HH:MM)"))
                  ),
                  fluidRow(
                    column(4, selectInput("location", "Ubicación", choices = locations())),
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
                    column(4, selectInput("analista", "Analista", choices = analysts())),
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
                    column(4, selectInput("location_select", "Seleccionar Localización", choices = locations())),
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
      })
    } else {
      flog.warn(sprintf("Intento de inicio de sesión fallido para el usuario: %s", user))
      showNotification("Usuario o contraseña incorrectos", type = "error")
    }
  })

  # Actualizar el UI de periodo según la selección de tipo de periodo
  output$period_number_ui <- renderUI({
    req(input$period_type)
    if (input$period_type == "Trimestre") {
      selectInput("period_number", "Número de Trimestre", choices = 1:4)
    } else {
      selectInput("period_number", "Número de Bimestre", choices = 1:6)
    }
  })

  # Añadimos un observador para manejar la acción del botón de logout.
  observeEvent(input$logout_button, {
    user_role(NULL)
    showLoginPage()
  })

  # Subir datos manuales
  observeEvent(input$submit, {

    # Validar formato de fecha
    if (is.na(as.Date(input$fecha))) {
      showNotification("Fecha inválida. Use el formato yyyy-mm-dd.", type = "error")
      return()
    }

    # Validar formato de hora
    if (!grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", input$hora_muestra)) {
      showNotification("Hora inválida. Use el formato HH:MM.", type = "error")
      return()
    }

    # Validar formato de separador decimal
    for (input_name in numeric_inputs) {
      value <- input[[input_name]]
      if (!is.na(value)) {
        if (!grepl("^[0-9]*\\.?[0-9]+$", as.character(value))) {
          showNotification(paste("El valor de", input_name, "es inválido. Use punto como separador decimal."), type = "error")
          return()

    # Actualizar la lista de localizaciones después de insertar nuevos datos
    locations <<- reactiveVal(unique(dbGetQuery(con, "SELECT DISTINCT location FROM locations")$location))
    updateSelectInput(session, "location", choices = locations)

        }
      }
    }

    query <- "
      INSERT INTO quality_process_data (
        id_muestra, fecha, hora_muestra, location, o_w_ppm, tss_mg_l, ph, cl_mg_l, conduc_us_cm, dureza_total_mg_l_caCO3, hierro_total_mg_l, bario_mg_l, sulfato_mg_l, analista, observaciones
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    dbExecute(con, query,
              params = list(input$id_muestra,
                            as.character(input$fecha),
                            as.character(input$hora_muestra),
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
                            input$observaciones)
    )

    showNotification("Datos ingresados correctamente", type = "message")
  })

  # Se genera una consulta para visualización del ultimo registro ingresado
  new_data <- dbGetQuery(con, "SELECT * FROM quality_process_data")
  new_data[is.na(new_data)] <- "NA"
  new_data$fecha <- format(as.Date(new_data$fecha), "%Y-%m-%d") # Cambia el formato de la fecha
  data <- reactiveVal(new_data)
  last_entry <- reactiveVal(new_data[nrow(new_data), ])

  # Se prepara el formulario para un nuevo ingreso

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

  output$last_entry <- renderTable({
    last_entry()

  flog.info("Insertando nuevos datos en la base de datos")
  dbExecute(con, query, params = list(...))
  flog.info("Datos insertados correctamente")

  showNotification("Datos ingresados correctamente", type = "message")

  })

  output$data_table <- renderDT({
    query <- "SELECT * FROM quality_process_data"
    data <- dbGetQuery(con, query)
    #data[is.na(data)] <- "NA"  # Reemplaza los valores NA con "NA"
    datatable(data, options = list(scrollX =TRUE, scrollY = TRUE),
              colnames = c("ID", "ID Muestra", "Fecha", "Hora Muestra", "Ubicación", "O/W (ppm)", "TSS (mg/L)", "pH", "Cl- (mg/L)", "Conduct. (µS/cm)", "Dureza Total (mg/L)", "Fe (mg/L)", "Ba (mg/L)", "SO4 (mg/L)", "Analista", "Observaciones", "Registro")
              )
  })

  output$last_entry <- renderTable({
    query <- "SELECT * FROM quality_process_data ORDER BY id DESC LIMIT 1"
    dbGetQuery(con, query)
  })

  output$plot <- renderPlot({
    req(input$var_select, input$location_select, input$year_select, input$period_type, input$period_number)

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
}

