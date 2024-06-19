library(shiny)


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

