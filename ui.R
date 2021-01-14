library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(shinycssloaders)
library(highcharter)
library(lubridate)

sidebar <- dashboardSidebar(tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),
  sidebarMenu(
    menuItem("Inicio", tabName = "inicio", icon = icon("align-left")),
    menuItem("Test", icon = icon("algolia"), tabName = "test"),
    menuItem("Contacto", icon = icon("envelope-square"), tabName = "contacto"),
    menuItem("Base de datos", icon = icon("database"), tabName = "basededatos")
  )
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  includeCSS("app.css"),
  tabItems(
    tabItem(tabName = "inicio",
            box(title = h1(tags$strong("Quiénes somos"), align = "center"),
                width = 12,
                solidHeader = TRUE,
                tags$h3("Hola somos la gente de la cebrita uwu"),
                HTML('<center><img src="gif_prueba.gif"></center>'))
    ),
    
    tabItem(tabName = "test",
            h2("Widgets tab content")
    ),
    
    tabItem(tabName = "contacto",
            h2("contacto")
    ),
    
    tabItem(tabName = "basededatos",
            fluidRow(
              column(11,
                     offset = 3,
                     box(title = "",
                         HTML('<center><img src="login.png" width = 100px height = 100px></center>'),
                         tags$br(),
                         tags$br(),
                         textInput("user", placeholder = "Usuario", label = tagList(icon("user"), "")),
                         passwordInput("password", placeholder = "Contraseña", label = tagList(icon("unlock-alt"), "")),
                         div(
                           style = "text-align: center;",
                           actionButton("login", "Entrar", style = "color: white; background-color:#808b96;
                                 padding: 10px 15px; width: 100px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;")
                           ),
                         shinyjs::hidden(
                           div(id = "text_login",
                               uiOutput("login_text")
                               )
                         )
                     )
              )
            ),
            shinyjs::hidden(
              div(id = "upload_button",
                  style = "text-align: left;",
                  fileInput("uploaded", 'Escoge un archivo en formato CSV:', accept = ".csv", buttonLabel = "Buscar...", placeholder = "No hay archivo seleccionado...", multiple = FALSE),
              ),
              tags$br(),
              div(id = "upload_wrong",
                  style = "text-align: left;",
                  tags$p("No se ha actualizado la base de datos debido a un error con el formato del archivo subido. Verifica que su terminación es .csv y que son correctos tanto los nombres de las columnas como sus tipos.", style = "color: red; font-weight: 600;  padding-top: 5px;font-size:16px;",  class = "text-center")
                  ),
              tags$br(),
            div(id = "match_tabsetpanel",
            tabsetPanel(
    tabPanel(h4("Búsqueda por paciente"),
             tags$br(),
             tags$br(),
             sidebarLayout(position = "left",
                           sidebarPanel(tags$style(".well {background-color:#ffffff;}"),
                                        textInput("nombre_paciente", "Ingrese nombre del paciente:", "Nombre"),
                                        textInput("apellidos_paciente", "Ingrese apellidos del paciente:", "Apellido1 Apellido2"),
                                        dateInput("fecha", "Ingrese fecha de la prueba:", value = toString(Sys.Date()), language = "es")),
                           mainPanel(
                             uiOutput("salida_busqueda")
                                     )
             )
             ),
    tabPanel(h4("Vista general"),
             fluidRow(
               column(11,
                      offset = 3,
                      shinyjs::hidden(
                        div(id = "match",
                            tags$br(),
                            tags$br(),
                            fluidRow(
                              column(4,
                                     offset = 1,
                                     div(
                                       style = "text-align: center;",
                                       actionButton("visualizar", "Visualizar", style = "color: white; background-color:#808b96;
                                   padding: 10px 15px; width: 150px; cursor: pointer;
                                   font-size: 18px; font-weight: 600;")
                                     ),
                                     div(
                                       style = "text-align: center;",
                                       downloadButton('downloadData', "", style = "color: white; background-color:#808b96;
                                   padding: 10px 15px; width: 150px; cursor: pointer;
                                   font-size: 18px; font-weight: 600;")
                                     ),
                                     tags$br(),
                                     tags$br()
                              )
                            )
                        )
                      )
               )
             ),
             fluidRow(
               column(12,
                      offset = 0,
                      shinyjs::hidden(
                        div(id = "database_hidden",
                            box(title = "",
                                align = "center",
                                width = 12,
                                tableOutput("table_database") %>% withSpinner(type = 6, color = "#585858")
                            )
                        )
                      )
               )
             )
    )
    )
    )
    )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "", disable = TRUE),
  skin = "black",
  sidebar,
  body
)