#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)


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


