# app.R

source("setup.R")
source("ui.R")
source("server.R")

# Definir la aplicación Shiny
shinyApp(ui, server, options = list(shiny.launch.browser = FALSE))

