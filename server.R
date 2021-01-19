library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(shinycssloaders)
library(highcharter)
library(lubridate)

users <- list(
  username = c("cebrita", "jluque"),
  password = c("cebritalamaswapa123", "metpsi123")
)

text <- reactiveValues(text = "inicializado")

server <- function(input, output) {
  #datos_base <- datos_raw()
  #datos_base$Fecha <- ymd(datos_base$Fecha)
  #colnames(datos_base) <- c("Nombre", "Apellidos", "Fecha", "F1.5", "F3", "F6", "F12", "F18")
  datos <- reactiveValues(datos = read.csv("database.csv"))
  #datos$datos$Fecha <- ymd(datos$datos$Fecha)
  #colnames(datos$datos) <- c("Nombre", "Apellidos", "Fecha", "F1.5", "F3", "F6", "F12", "F18")
  
  datos_raw_fecha_string <- reactive({
    data <- datos$datos
    data <- data[-1,]
    colnames(data) <- c("Nombre", "Apellidos", "Fecha", "F1.5", "F3", "F6", "F12", "F18")
    data$Fecha <- ymd(data$Fecha)
    data <- data %>% arrange(desc(Fecha))
    data$Fecha <- unlist(lapply(data$Fecha, toString))
    return(data)
  })
  
  observeEvent(input$login, {
    if(input$user %in% users$username){
      if(input$password == users$password[users$username == input$user]){
        text$text <- tags$p("Identificado correctamente.", style = "color: green; font-weight: 600;  padding-top: 5px;font-size:16px;",  class = "text-center")
        shinyjs::show("text_login")
        shinyjs::show("match")
        shinyjs::show("match_tabsetpanel")
        if(input$user == "cebrita"){
          shinyjs::show("upload_button")
        }
        else{
          shinyjs::hide("upload_button")
        }
      }
      else{
        text$text <- tags$p("¡Usuario o contraseña incorrectos!", style = "color: red; font-weight: 600;  padding-top: 5px;font-size:16px;",  class = "text-center")
        shinyjs::show("text_login")
        shinyjs::hide("match")
        shinyjs::hide("match_tabsetpanel")
        shinyjs::hide("upload_button")
      }
    }
    else{
      text$text <- tags$p("¡Usuario o contraseña incorrectos!", style = "color: red; font-weight: 600;  padding-top: 5px;font-size:16px;",  class = "text-center")
      shinyjs::show("text_login")
      shinyjs::hide("match")
      shinyjs::hide("match_tabsetpanel")
      shinyjs::hide("upload_button")
    }
  })
  
  observeEvent(input$uploaded, {
    result <- tryCatch({
      infile <- input$uploaded
      if (is.null(infile)) {
        print("No se actualiza nada.")
      }
      else{
        if(grepl("\\.csv$", infile$datapath) == TRUE){
          prueba <- read.csv(infile$datapath)
          colnames(prueba) <- c("Nombre", "Apellidos", "Fecha", "F1.5", "F3", "F6", "F12", "F18")
          prueba$Fecha <- ymd(prueba$Fecha)
          if(ncol(prueba) == 8){
            if(sum(colnames(prueba) == colnames(datos$datos)) == ncol(prueba)){
              if(nrow(prueba) >= 1){
                verifier <- 0
                for(i in seq_len(nrow(prueba))){
                  if(is.character(prueba[i,][[1]]) & is.character(prueba[i,][[2]]) & is.Date(prueba[i,][[3]]) & is.numeric(prueba[i,][[4]]) & is.numeric(prueba[i,][[5]]) & is.numeric(prueba[i,][[6]]) & is.numeric(prueba[i,][[7]]) & is.numeric(prueba[i,][[8]])){
                    verifier <- verifier + 1
                  }
                }
                if(verifier == nrow(prueba)){
                  datos_new <- read.csv(infile$datapath)
                  colnames(datos_new) <- c("Nombre", "Apellidos", "Fecha", "F1.5", "F3", "F6", "F12", "F18")
                  datos_new$Fecha <- ymd(datos_new$Fecha)
                  datos_new <- unique(rbind(datos$datos, datos_new))
                  datos$datos <- datos_new
                  write.csv(datos_new, "database.csv", row.names = FALSE)
                  shinyjs::hide("upload_wrong")
                }
                else{
                  shinyjs::show("upload_wrong")
                }
              }
              else{
                shinyjs::show("upload_wrong")
              }
            }
            else{
              shinyjs::show("upload_wrong")
            }
          }
          else{
            shinyjs::show("upload_wrong")
          }
        }
        else{
          shinyjs::show("upload_wrong")
        }
      }
    }, error = function(error_condition){
      shinyjs::show("upload_wrong")
    })
  })
  
  output$login_text <- renderUI({
    text$text
  })
  
  output$table_database <- renderTable(datos_raw_fecha_string(),
                                  options = list(
                                    pageLength = 5
                                  )
  )
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("database-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(datos$datos, file)
    })
  
  observeEvent(input$visualizar, {
    if(input$visualizar[1] %% 2 != 0){
      shinyjs::show("database_hidden")
    }
    else{
      shinyjs::hide("database_hidden")
    }
  })
  
  output$salida_busqueda <- renderUI({
    req(input$nombre_paciente)
    fecha <- input$fecha
    indice <- which(datos$datos$Apellidos == input$apellidos_paciente & datos$datos$Nombre == input$nombre_paciente & datos$datos$Fecha == input$fecha)
    if(length(indice) >= 1){
      indice <- indice[1]
      nombre <- datos$datos$Nombre[indice]
      apellidos <- datos$datos$Apellidos[indice]
      fecha <- datos$datos$Fecha[indice]
      frecuencias <- readr::parse_number(colnames(datos$datos)[4:ncol(datos$datos)])
      S <- round(1/(unname(unlist(datos$datos[indice,4:ncol(datos$datos)]))), digits = 3)
      df <- as.data.frame(matrix(data = c(frecuencias, S), ncol = 2, nrow = 5))
      fluidRow(
        column(12,
               wellPanel(
                 align = "left",
                 width = 12,
                 hc <- highchart() %>% 
                   hc_add_series(data = df,
                                 type = "spline",
                                 hcaes(x = V1, y = V2),
                                 name = "CSF") %>%
                   hc_xAxis(title = list(text = "Frecuencia espacial (cpg)")) %>%
                   hc_yAxis(title = list(text = "S (dB)"), type = "logarithmic") %>%
                   hc_title(text = paste0("CSF de ", nombre, " ", apellidos, ", ", fecha), align = "center") %>%
                   hc_legend(enabled = FALSE)
               )
        )
      )
    }
    else{
      if(input$nombre_paciente == "Nombre" & input$apellidos_paciente == "Apellido1 Apellido2" & format(input$fecha, "%d-%m-%Y") == format(Sys.Date(), "%d-%m-%Y")){
        print("")
      }
      else{
        tags$p("No se ha encontrado ninguna coincidencia.", style = "color: red; font-weight: 600;  padding-top: 5px;font-size:16px;",  class = "text-center")
      }
    }
  })
}