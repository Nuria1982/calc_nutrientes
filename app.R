library(rsconnect)
library(bslib)
library(shiny)
library(shinycssloaders)
library(bsicons)
library(flexdashboard)
library(DT)
library(tidyr) 
library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)
library(shinythemes)
library(shinyauthr)
library(shinyjs)
library(bs4Dash)
library(fresh)
library(lubridate)
library(png)
library(readxl)
library(writexl)
library(openxlsx)
library(DBI)
library(RSQLite)
library(sodium)
library(kableExtra)


# Simulación de una base de datos de usuarios
if (file.exists("base_usuarios")) {
  db <- dbConnect(SQLite(), "base_usuarios")
} else {
  db <- dbConnect(SQLite(), "base_usuarios")
  dbCreateTable(db, "user_base", c(
    user = "TEXT",
    password = "TEXT",
    name = "TEXT",
    email = "TEXT"
  ))
  dbWriteTable(db, "user_base", tibble(
    user = c("user1", "user2"),
    password = c(
      sodium::password_store("pass1"),
      sodium::password_store("pass2")
    ),
    name = c("User One", "User Two"),
    email = c("user1@example.com", "user2@example.com")
  ), append = TRUE)
}

credentials_data <- reactiveVal(NULL)

get_user_base <- function() {
  db <- dbConnect(RSQLite::SQLite(), "base_usuarios")
  user_base <- dbReadTable(db, "user_base")
  return(user_base)
}

cookie_expiry <- 7

add_sessionid_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

save_new_user <- function(user, password, name, email, conn = db) {
  db <- dbConnect(RSQLite::SQLite(), "base_usuarios")
  dbWriteTable(conn, "user_base", tibble(
    user = user,
    password = sodium::password_store(password),
    name = name,
    email = email
  ), append = TRUE)

  
  dbDisconnect(db)
}

 saveRDS(user_base, "user_base.rds")

# file.exists("base_usuarios")
# db <- dbConnect(RSQLite::SQLite(), "base_usuarios")
# dbListTables(db)
# dbReadTable(db, "user_base")
# # 
#  dbGetQuery(db, "PRAGMA table_info(user_base)")
#  dbGetQuery(db, "SELECT * FROM user_base LIMIT 10")
# dbDisconnect(db)

#######################


ui <- fluidPage(
  useShinyjs(),
  
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),

  
  titlePanel("Plataforma de Recomendación Nutricional para Cultivos Extensivos."),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
    body {
        background-image: url('imagen2.jpg'); /* Ruta relativa a la carpeta www */
        background-size: cover; /* Ajusta la imagen al tamaño de la pantalla */
        background-attachment: fixed; /* Mantiene la imagen fija al hacer scroll */
        background-position: center;
        opacity: 0.9; /* Ajusta la opacidad para añadir transparencia */
      }
      .container-fluid {
        background-color: rgba(255, 255, 255, 0.8); /* Fondo semitransparente para el contenido */
        border-radius: 10px;
        padding: 20px;
      }
      .gauge-box-container {
        display: flex;
        justify-content: space-between;
        flex-direction: column;
        align-items: center; /* Centra los elementos horizontalmente */
        gap: 15px; /* Espaciado entre los elementos */
        width: 100%;
      }
      .gauge-box {
        border: 2px solid #dcdcdc;
        border-radius: 10px;
        padding: 15px;
        background-color: #f7f7f7;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        width: 100%; 
        text-align: center; /* Centra el contenido dentro del gauge */
      }
      .gauge-title {
        font-size: 1.2em;
        font-weight: bold;
        color: #333;
        margin-bottom: 15px;
        text-align: center;
      }
      .ui-output-box {
        text-align: center; 
        width: 100%; 
      }
      /* Cambiar SOLO el valor central del gauge */
      .gauge svg text:nth-of-type(1) { 
        font-size: 2.5em !important; 
        font-weight: bold;
        fill: #333; 
      }
    "))
    ),
  
  # Módulo de autenticación
  shinyauthr::loginUI(id = "login",
                      user_title = "Usuario",
                      pass_title = "Contraseña",
                      login_title = "Iniciar sesión",
                      error_message = "Usuario o contraseña incorrectos",
                      cookie_expiry = cookie_expiry,
                      br(),
                      tags$p("Si usted no está registrado, por favor ",
                             class = "text-center",
                        actionLink("abrir_registro", "ingrese aquí",
                                   style = "text-align: center; font-size: 0.85em; margin-top: -10px;",))
                      ),
  
  div(
    id = "formulario_registro", 
    style = "display: none;",
    br(),
    h3("Formulario de Registro"),
    textInput("nombre", "Nombre Completo"),
    textInput("email", "Correo Electrónico"),
    passwordInput("password", "Contraseña"),
    passwordInput("confirmar_password", "Confirmar Contraseña"),
    actionButton("enviar_registro", "Enviar Registro"),
  ),

  br(),
  br(),
  tabsetPanel(id = "main_tabs",
    tabPanel("Principal",
             br(),
             h5(HTML("El manejo responsable de nutrientes y de los fertilizantes, en los sistemas agrícolas 
                      se basa en aplicar la fuente correcta, en la dosis, el momento y el lugar correctos, constituyendo 
                      el concepto de los 4 Requisitos (4R) para las mejores prácticas de manejo de nutrientes y fertilizantes. 
                      Este enfoque considera dimensiones económicas, sociales y ambientales del manejo de nutrientes y 
                      es esencial para la sostenibilidad de los sistemas agrícolas. La implementación de los 4Rs es intensiva 
                      en cuanto a conocimiento y específica para cada sitio.")),
             h4(HTML("<strong>La plataforma tiene como principal objetivo facilitarles a los usuarios el cálculo 
                    de las dosis de nutrientes (uno de los 4R) requeridas por los principales cultivos extensivos.
                    Para lo cual, la misma está desarrollada en base a modelos publicados en diferentes revistas científicas 
                    y validados a nivel de lotes de producción.</strong>")),
             br(),
             h4(HTML("<strong>Trayectoria</strong>")),
             h5(HTML("La plataforma fue desarrollada y es actualizada por investigadores del grupo de 
                     Relación Suelo - Cultivo de la Unidad Integrada Balcarce, los cuales presentan una amplia 
                     trayectoria en investigación, docencia y transferencia en temas de fertilidad de suelos y 
                     nutrición de cultivos. Además, el grupo es responsable del Laboratorio de Suelo de INTA Balcarce 
                     (link a servicios) que hace más de 30 años brinda no solo servicios de análisis de suelo y 
                     planta sino también actividades como experimentación a campo, asesoramiento en fertilidad de suelos, 
                     charlas técnicas, jornadas y cursos de actualización profesional.")),
             br(),
             div(
               style = "text-align: center;",
               tags$img(src = "imagen1.jpg", width = "50%", alt = "integrantes")
             )
    ),
    
    tabPanel("Nitrógeno",
             br(),
             h4(HTML("Definición dosos de nitrógeno")),
             
             # Subpestañas dentro de Nitrógeno
             tabsetPanel(
               tabPanel("Lote único",
                        br(),
                        fluidRow(
                          column(6,
                                 selectInput("cultivo",
                                             label = strong("Seleccione el cultivo:"),
                                             choices = list(
                                               "Maíz" = "Maiz",
                                               "Trigo/Cebada" = "Trigo",
                                               "Girasol" = "Girasol",
                                               "Papa" = "Papa"
                                             ),
                                             selected = "Maiz"
                                 )
                          )
                        ),
                        fluidRow(
                            column(4, 
                                 div(style = "background-color: #06A77D80; padding: 15px; border-radius: 10px;",
                                     h3(HTML(("<strong>Datos para el cálculo de la demanda de nitrógeno</strong>"))),
                                     br(),
                                     numericInput("rendimiento",  
                                                  label = strong(HTML("Rendimiento objetivo (t/ha)")),
                                                  value = 1
                                     ),
                                     br(),
                                     uiOutput("proteina_ui"),
                                     br(),
                                     numericInput("req_N_planta",  
                                                  label = strong(HTML("Requerimiento en planta (kg N/t)")), 
                                                  value = 0),
                                     numericInput("req_N_sistema",  
                                                  label = strong(HTML("Demanda del sistema (kg N/t)")), 
                                                  value = 0)
                                 )
                          ),
                          
                          column(8,
                                 div(style = "background-color: #FF991480; padding: 15px; border-radius: 10px;",
                                     h3(strong("Datos para el cálculo de la oferta de nitrógeno")),
                                     fluidRow(
                                       column(4,
                                              numericInput("nitrato_20",  
                                                           label = strong(HTML("N-nitrato (ppm) (0-20cm)")),
                                                           value = 0),
                                              numericInput("nitrato_40",  
                                                           label = strong(HTML("N-nitrato (ppm) (20-40cm)")),
                                                           value = 0),
                                              numericInput("nitrato_60",  
                                                           label = strong(HTML("N-nitrato (ppm) (40-60cm)")),
                                                           value = 0),
                                              numericInput("dens_ap",  
                                                           label = strong(HTML("Densidad aparente (g / cm3)")), 
                                                           value = 1.2)
                                       ),
                                       column(4,
                                              numericInput("nan",  
                                                           label = strong(HTML("Nan (0-20cm, ppm)")),
                                                           value = 0),
                                              uiOutput("zonas_ui")
                                              ),
                                       column(4,
                                              conditionalPanel(
                                                condition = "input.cultivo == 'Trigo'",  
                                                selectInput("antecesor", "Efecto Antecesor", 
                                                            choices = c("Soja", "Maiz", "Otros")),
                                                conditionalPanel(
                                                  condition = "input.antecesor == 'Otros'",  
                                                  numericInput("valor_otros", "Ingrese el valor", value = 0)
                                                )
                                              )
                                       )
                                       ),
                                     hr(),
                                     fluidRow(
                                       column(4, 
                                              div(style = "background-color: #FF991490; padding: 10px; border-radius: 10px; text-align: center;",
                                                  h6(HTML("<strong>Nitrógeno Disponible</strong><br><small>(0-60cm, kg N/ha)</small>")),
                                                  div(style = "font-size: 18px; font-weight: bold;",
                                                  uiOutput("nitrogeno_disp"))
                                              )
                                       ),
                                       column(4, 
                                              div(style = "background-color: #FF991490; padding: 10px; border-radius: 10px; text-align: center;",
                                                  h6(HTML("<strong>Mineralización de N</strong><br><small>(0-20cm, kg N/ha)</small>")),
                                                  div(style = "font-size: 18px; font-weight: bold;",
                                                  uiOutput("nan_total"))
                                              )
                                       ),
                                       column(4, 
                                              div(style = "background-color: #FF991490; padding: 10px; border-radius: 10px; text-align: center;",
                                                  h6(HTML("<strong>Efecto Antecesor</strong><br><small>(kgN/ha)</small>")),
                                                  div(style = "font-size: 18px; font-weight: bold;",
                                                  uiOutput("efecto_antecesor"))
                                              )
                                       )
                                     )
                                     )
                                 )
                          ),
               br(),
               br(),
               fluidRow(
                 column(9, offset = 1,  
                        div(style = "display: flex; justify-content: space-between; align-items: center;",
                            div(style = "flex: 1; padding-right: 20px; padding-right: 10px;",
                                uiOutput("demandaN")
                            ),
                            div(style = "flex: 1; padding-right: 20px; padding-right: 10px;",
                                uiOutput("ofertaN")
                            ),
                            div(class = "gauge-title", "Dosis de N (kg N / ha)",
                                flexdashboard::gaugeOutput("dosis_nitrogeno", width = "100%", height = "100px")
                                
                            )
                        )
                 )
                 
               )
             ),
               
               tabPanel("Múltiple lotes",
                        br(),
                        h3(strong("Para calcular la dosis recomendada de nitrógeno para cada lote, ")),
                        br(),
                        
                        div(style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
                            h5("Puede descargar una tabla modelo y completar los datos solicitadios para los cálculos"),
                            downloadButton("descarga_modelo_nitrogeno", "Descargar Modelo de Tabla"),
                            br(),
                            br(),
                            h5("Cargue la tabla con los datos solicitados:"),
                            fileInput("archivo_usuario", "Subir archivo de datos",
                                      accept = c(".csv", ".xlsx")
                            ),
                            br(),
                            fluidRow(  
                              column(4, uiOutput("zonas_maiz")),
                              column(4, uiOutput("proteina_trigo"))  
                            )
                        ),
                        br(),
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            h3(HTML(("<strong>Cálculo de dosis de N por Lote</strong>"))),
                            uiOutput("resultados_tabla"),
                            br(),
                            downloadButton("download_data", "Descargar resultados (.CSV)")
                        ),
                        br(),
                        fluidRow(
                          column(
                            10, offset = 1,
                              withSpinner(plotOutput("multi_lotes", height = "700px"),
                                          type = 5,
                                          color = "#0dc5c1",
                                          size = 0.5),
                            div(style = "text-align: right; margin-top: 10px;",
                                downloadButton("download_plot", "Descargar Gráfico", class = "btn btn-success")
                            )
                          )
                        )
               )
             )
    ),
    
    tabPanel("Fósforo",
             br(),
             h2(HTML("<strong>Fósforo</strong>")),
             h4(HTML("xxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx"))
    ),
    
    tabPanel("Azufre",
             br(),
             h2(HTML("<strong>Azufre</strong>")),
             h4(HTML("xxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx"))
    ),
    
    tabPanel("Recomendaciones",
             br(),
             h2(HTML("<strong>Recomendaciones</strong>")),
             h4(HTML("xxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx"))
    )
  )
)





# Define server logic ----
server <- function(input, output, session) {
  
  ####### Registro de usuario
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = get_user_base(),
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessionids_from_db,
    cookie_setter = add_sessionid_to_db,
    log_out = reactive(logout_init())
  )
  

  
  observeEvent(input$abrir_registro, {
    # Mostrar el formulario de registro en un modal
    showModal(
      modalDialog(
        title = "Formulario de Registro",
        textInput("nombre", "Nombre Completo"),
        textInput("usuario", "Nombre de Usuario"),
        textInput("email", "Correo Electrónico"),
        passwordInput("password", "Contraseña"),
        passwordInput("confirmar_password", "Confirmar Contraseña"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("enviar_registro", "Registrar")
        )
      )
    )
  })
  
  # Registrar el nuevo usuario
  observeEvent(input$enviar_registro, {
    if (input$password != input$confirmar_password) {
      showNotification("Las contraseñas no coinciden.", type = "error")
      return()
    }
    if (input$usuario %in% get_user_base()$user) {
      showNotification("El usuario ya está registrado.", type = "error")
      return()
    }
    
    save_new_user(
      user = input$usuario,
      password = input$password,
      name = input$nombre,
      email = input$email
    )
    
    print(get_user_base())
    credentials_data(get_user_base())
    
    session$reload() 
    
    removeModal()
    showNotification("Registro exitoso. Ahora puede iniciar sesión.", type = "message")
  })
  

  observe({
    if (credentials()$user_auth) {
      updateTabsetPanel(session, "main_tabs", selected = "Principal")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = NULL)
    }
  })
  
  observeEvent(input$main_tabs, {
    if (!credentials()$user_auth && input$main_tabs != "Principal") {
      showModal(modalDialog(
        title = "Acceso restringido",
        "Por favor inicie sesión para acceder a esta pestaña.",
        easyClose = TRUE,
        footer = NULL
      ))
      updateTabsetPanel(session, "main_tabs", selected = "Principal")
    }
  })

  
 
  
  # # Guardar datos del usuario al salir
  # observeEvent(logout_init(), {
  #   save_user_data(credentials()$info$user, user_data$data)
  # })


#################
  
  # Lote único
  
  #DEMANDA 
  output$proteina_ui <- renderUI({
    if (input$cultivo == "Trigo") {
      numericInput(
        "proteína", 
        label = strong("Proteína Objetivo (%)"), 
        value = 0
      )
    } else {
      HTML("<strong>Proteína Objetivo (%)</strong>: No corresponde")
    }
  })
  
  observeEvent(input$proteína, {
    if (input$cultivo == "Trigo" && !is.null(input$proteína)) {
      req_N_planta_value <- 30 + (30 * (input$proteína - 10) / 10)  
      req_N_sistema_value <- 50 + (50 * (input$proteína - 10) / 10)  
      
      updateNumericInput(session, "req_N_planta", value = req_N_planta_value)
      updateNumericInput(session, "req_N_sistema", value = req_N_sistema_value)
    }
  })
  
  observeEvent(input$cultivo, {
    if (input$cultivo == "Maiz") {
      updateNumericInput(session, "req_N_planta", value = 20)
      updateNumericInput(session, "req_N_sistema", value = 30)
      
    } else if (input$cultivo == "Girasol") {
      updateNumericInput(session, "req_N_planta", value = 40)
      updateNumericInput(session, "req_N_sistema", value = 60)
      
    } else if (input$cultivo == "Papa") {
      updateNumericInput(session, "req_N_planta", value = 4)
      updateNumericInput(session, "req_N_sistema", value = 6)
    }
  })
  
  demandaN <- reactive({
    req(input$rendimiento, input$req_N_sistema, input$req_N_planta)
    if (input$nan != 0) {
      input$rendimiento * input$req_N_sistema
    } else {
      input$rendimiento * input$req_N_planta
    }
  })

  
  output$demandaN <- renderUI({
    div(
      class = "value-box",
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #06A77D; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
      div(
        style = "font-size: 30px; font-weight: bold;",
        paste(demandaN(), "kg N / ha")
      ),
      div(
        style = "font-size: 18px; font-weight: bold; margin-bottom: 6px; text-align: center;",
        "Demanda"
      ),
      div(
        class = "icon-container",
        style = "font-size: 40px;",
        icon("chart-line")
      )
    )
  })

  
  #OFERTA
  nitrogeno_disp <- reactive({
    densidad_aparente <- ifelse(!is.null(input$dens_ap) && input$dens_ap != 0, input$dens_ap, 1.2)
    if (!is.null(input$nitrato) && input$nitrato > 0) {
      input$nitrato * densidad_aparente * 6
    } else {
      (input$nitrato_20 + input$nitrato_40 + input$nitrato_60) * (densidad_aparente * 2)
    }
  })
  
  output$nitrogeno_disp <- renderUI({
    round(nitrogeno_disp(), 2)
  })
  
  output$zonas_ui <- renderUI({
    if (input$nan > 0 && input$cultivo == "Maiz") {
      selectInput("zona", 
                  label = strong("Seleccione la zona"),
                  choices = c("Sudeste siembra temprana", 
                              "Nucleo siembra temprana", 
                              "Nucleo siembra tardia"),
                  selected = "Sudeste siembra temprana")
    } else {
      # Si no se cumplen las condiciones, no mostrar nada
      NULL
    }
  })
  
  mineralizacion <- reactive({
    if (input$cultivo == "Maiz" && !is.null(input$zona)) {
      # Si el cultivo es maíz, se usa el valor según la zona seleccionada
      switch(input$zona,
             "Sudeste siembra temprana" = 3.2,
             "Nucleo siembra temprana" = 3.6,
             "Nucleo siembra tardia" = 4.2,
             1)  
    } else {
      # Valores para otros cultivos
      switch(input$cultivo,
             "Trigo" = 2.2,       
             "Soja" = 1.8,        
             "Papa" = 3.2,        
             1)  
    }
  })
  
  nan_total <- reactive({
    input$nan * mineralizacion()
  })
  
  output$nan_total <- renderUI({
    if (input$nan > 0) {
      round(nan_total(), 2)
    } else {
      HTML(paste0("<strong>El modelo considera el valor medio de mineralización de la región.</strong> "))
    }
  })
  
  efecto_antecesor <- reactive({
    
    req(input$cultivo, input$antecesor)
    
    if (input$cultivo == "Trigo") {
      if (input$antecesor == "Otros") {
        req(input$valor_otros)  
        input$valor_otros
      } else {
        switch(input$antecesor,
               "Soja" = 20,
               "Maiz" = -30,
               "Otros" = 0)  
      }
    } else {
      1  
    }
  })
  
  output$efecto_antecesor <- renderUI({
    round(efecto_antecesor(), 2)
  })
  
  
  ofertaN <- reactive({
    nitrogeno_disp() + nan_total() + efecto_antecesor()
  })
  
  output$ofertaN <- renderUI({
    div(
      class = "value-box",
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #FF9914; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
      div(
        style = "font-size: 30px; font-weight: bold;",
        paste(ofertaN(), "kg N / ha")
      ),
      div(
        style = "font-size: 18px; font-weight: bold; margin-bottom: 6px; text-align: center;",
        "Oferta"
      ),

      div(
        class = "icon-container",
        style = "font-size: 40px;",
        icon("chart-pie")
      )
    )
  })

           
  dosisN <- reactive({
    req(ofertaN(),demandaN()) # Asegura que ambos valores estén disponibles
    demanda <- demandaN()
    oferta <- ofertaN()
    
    # Si alguno de los valores es NULL, devuelve un valor predeterminado
    if (is.null(demanda) || is.null(oferta)) {
      return(0) 
    }
    
    demanda - oferta
  })

  
  # output$dosisN <- renderUI({
  #   div(
  #     class = "value-box",
  #     style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #e74c3c; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
  # 
  #     div(
  #       style = "font-size: 30px; font-weight: bold;",
  #       paste(round(dosisN(), 1), "kg N / ha")
  #     ),
  #     div(
  #       style = "font-size: 18px; font-weight: bold; margin-bottom: 6px; text-align: center;",
  #       "Dosis"
  #     ),
  #     div(
  #       class = "icon-container",
  #       style = "font-size: 40px;",
  #       icon("tint")
  #     )
  #   )
  # })
    
  
  output$dosis_nitrogeno <- renderGauge({
    req(demandaN(), dosisN())
    
    max_val <- demandaN()
    
    tercio <- max_val / 3
    success_range <- c(0, tercio)                 
    warning_range <- c(tercio + 1, 2 * tercio)    
    danger_range <- c(2 * tercio + 1, max_val)   
    
    gauge(dosisN(), min = 0, max = max_val, 
          gaugeSectors(success = success_range, warning = warning_range, danger = danger_range)
          )
  })


  #Múltiples lotes
  
  output$descarga_modelo_nitrogeno <- downloadHandler(
    filename = function() {
      "data_usuario.xlsx"
    },
    content = function(file) {
      # Crear un dataframe modelo
      modelo <- data.frame(
        Lote = c(NA, NA), 
        Cultivo = c(NA, NA), 
        Rendimiento_objetivo = c(NA, NA), 
        Efecto_antecesor = c(NA, NA),
        N_nitrato_20 = c(NA, NA),
        N_nitrato_40 = c(NA, NA), 
        N_nitrato_60 = c(NA, NA), 
        Nan_20 = c(NA, NA),
        Densidad_aparente = c(NA, NA)
      )
      
      # Escribir el archivo Excel usando writexl
      writexl::write_xlsx(modelo, file)
    }
  )
  
  data_usuario <- reactive({
    if (is.null(input$archivo_usuario)) {
      return(NULL)  # Si no hay archivo subido, devuelve NULL
    }
    
    ext <- tools::file_ext(input$archivo_usuario$name)
    
    # Leer el archivo según su extensión
    if (ext == "csv") {
      data <- read.csv(input$archivo_usuario$datapath)
    } else if (ext == "xlsx") {
      data <- readxl::read_xlsx(input$archivo_usuario$datapath)
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
      return(NULL)
    }
    
    # Verificar si el archivo tiene las columnas requeridas
    required_columns <- c("Lote", "Cultivo", "Rendimiento_objetivo", "Efecto_antecesor", "N_nitrato_20",
                          "N_nitrato_40", "N_nitrato_60", 
                          "Nan_20", "Densidad_aparente")
    missing_columns <- setdiff(required_columns, colnames(data))
    
    if (length(missing_columns) > 0) {
      showNotification(
        paste("El archivo no tiene las columnas requeridas:", 
              paste(missing_columns, collapse = ", ")), 
        type = "error"
      )
      return(NULL)  # Si faltan columnas, devolver NULL
    }
    
    colnames(data) <- tolower(colnames(data))
    
    # Si falta la columna `Densidad aparente`, crearla con el valor predeterminado
    if (!"densidad_aparente" %in% colnames(data)) {
      data$densidad_aparente <- 1.2
    }
    
    # Reemplazar valores vacíos (NA) en `Densidad aparente` por 2.4
    data$densidad_aparente <- ifelse(
      is.na(data$densidad_aparente) | data$densidad_aparente == "", 
      1.2, 
      as.numeric(data$densidad_aparente)
    )
    
    # Confirmar al usuario que el archivo se ha procesado correctamente
    showNotification("Archivo subido correctamente.", type = "message")
    
    
    return(data)
  })
  
  output$zonas_maiz <- renderUI({
    req(data_usuario())
    if ("maiz" %in% data_usuario()$cultivo) {
      selectInput("zona", 
                  label = strong("Seleccione la zona geográfica para el cultivo de MAIZ"),
                  choices = c("Sudeste siembra temprana", 
                              "Nucleo siembra temprana", 
                              "Nucleo siembra tardia"),
                  selected = "Sudeste siembra temprana")
    } else {
      NULL
    }
  })
  
  
  output$proteina_trigo <- renderUI({
    req(data_usuario())
    if ("trigo" %in% data_usuario()$cultivo) {
      numericInput(
        "proteina", 
        label = strong("Para TRIGO ingrese la proteína objetivo (%)"),
        value = 0
      )
    } 
  })
  
  ajustar_requerimiento <- function(req_sistema, req_planta, proteina = NULL) {
    if (!is.null(proteina)) {
      req_N_planta <- req_planta + (req_planta * (proteina - 10) / 10)
      req_N_sistema <- req_sistema + (req_sistema * (proteina - 10) / 10)
    } else {
      req_N_planta <- req_planta
      req_N_sistema <- req_sistema
    }
    return(list(req_N_sistema = req_N_sistema, req_N_planta = req_N_planta))
  }
  
  resultados <- reactive({
    req(data_usuario())
    datos <- data_usuario()
    
    datos$cultivo <- trimws(datos$cultivo)
    datos$antecesor <- trimws(datos$efecto_antecesor)
    
    # Convertir columnas anuméricas
    datos$rendimiento_objetivo <- as.numeric(datos$rendimiento_objetivo)
    datos$n_nitrato_20 <- as.numeric(datos$n_nitrato_20)
    datos$n_nitrato_40 <- as.numeric(datos$n_nitrato_40)
    datos$n_nitrato_60 <- as.numeric(datos$n_nitrato_60)
    datos$nan_20 <- as.numeric(datos$nan_20)
    datos$densidad_aparente <- as.numeric(datos$densidad_aparente)
    
    
    datos <- datos %>%
      mutate(
        N_disponible = ((n_nitrato_20 +
                           n_nitrato_40 +
                           n_nitrato_60) * densidad_aparente * 2),
        Mineralizacion = case_when(
          cultivo == "maiz" ~ {
            if (input$zona == "Sudeste siembra temprana") 3.2
            else if (input$zona == "Nucleo siembra temprana") 3.6
            else if (input$zona == "Nucleo siembra tardia") 4.2
            else 1  # Valor por defecto si no se selecciona una zona válida
          },
          cultivo == "trigo" ~ 2.2,
          cultivo == "girasol" ~ 2.5,
          cultivo == "papa" ~ 3.2,
          TRUE ~ 1.0 
        ),
        Nan_total = ifelse(!is.na(nan_20), nan_20 * Mineralizacion, NA)
      )
    
    req_sistema <- c(maiz = 30, trigo = 50, girasol = 60, papa = 6)
    req_planta <- c(maiz = 20, trigo = 30, girasol = 40, papa = 4)
    
    ajustes <- ajustar_requerimiento(
      req_sistema = req_sistema["trigo"],
      req_planta = req_planta["trigo"],
      proteina = input$proteina
    )
    
    req_sistema["trigo"] <- ajustes$req_N_sistema
    req_planta["trigo"] <- ajustes$req_N_planta
    
    # Calcular Oferta, Demanda y DosisN por Lote
    datos <- datos %>%
      mutate(
        Requerimiento = case_when(
          !is.na(nan_20) ~ case_when(
            cultivo == "maiz" ~ req_sistema["maiz"],
            cultivo == "trigo" ~ req_sistema["trigo"],
            cultivo == "girasol" ~ req_sistema["girasol"],
            cultivo == "papa" ~ req_sistema["papa"],
            TRUE ~ 0
          ),
          is.na(nan_20) ~ case_when(
            cultivo == "maiz" ~ req_planta["maiz"],
            cultivo == "trigo" ~ req_planta["trigo"],
            cultivo == "girasol" ~ req_planta["girasol"],
            cultivo == "papa" ~ req_planta["papa"],
            TRUE ~ 0
          ),
          TRUE ~ 0
        ),
        OfertaN = N_disponible + coalesce(Nan_total, 0) + efecto_antecesor,
        DemandaN = rendimiento_objetivo * Requerimiento,
        DosisN = DemandaN - OfertaN
      )
    
    # Seleccionar columnas relevantes
    datos_resultado <- datos %>%
      select(lote, cultivo, efecto_antecesor, rendimiento_objetivo, DemandaN, Nan_total, N_disponible, OfertaN, DosisN) %>%
      rename(`Lote` = lote,
             `Cultivo` = cultivo,
             `Efecto antecesor` = efecto_antecesor,
             `Rendimiento (tn/ha)` = rendimiento_objetivo,
             `Demanda N (kg N / ha)` = DemandaN,
             `N mineralizable (kg N / ha)` = Nan_total,
             `Nitrogeno Disponible (kg N / ha)` = N_disponible,
             `Oferta N (kg N / ha)` = OfertaN,
             `Dosis N (kg N / ha)` = DosisN)
    
    return(datos_resultado)
  })
  
  # Renderizar tabla con resultados
  output$resultados_tabla <- renderUI({
    data <- resultados()
    
    # Crear tabla HTML con estilos específicos para las columnas
    tabla_html <- paste0(
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<thead><tr>",
      "<th style='background-color: #CCCCCC; padding: 1px;'>Lote</th>",
      "<th style='background-color: #CCCCCC; padding: 1px;'>Cultivo</th>",
      "<th style='background-color: #CCCCCC; padding: 1px;'>Efecto antecesor</th>",
      "<th style='background-color: #A9D18E; padding: 1px;'>Rendimiento (tn / ha)</th>",
      "<th style='background-color: #A9D18E; padding: 1px;'>Demanda (kg N / ha)</th>",
      "<th style='background-color: #F4B183; padding: 1px;'>N mineralizable (kg N / ha)</th>",
      "<th style='background-color: #F4B183; padding: 1px;'>Nitrogeno Disponible (kg N / ha)</th>",
      "<th style='background-color: #F4B183; padding: 1px;'>Oferta (kg N / ha)</th>",
      "<th style='background-color: #FF9999; padding: 1px;'>Dosis (kg N / ha)</th>",
      "</tr></thead>",
      "<tbody>",
      paste(
        apply(data, 1, function(row) {
          paste0(
            "<tr>",
            paste0("<td style='padding: 10px;'>", row, "</td>", collapse = ""),
            "</tr>"
          )
        }),
        collapse = ""
      ),
      "</tbody></table>"
    )
    
    HTML(tabla_html)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("resultados_", Sys.Date(), ".csv", sep = "")  
    },
    content = function(file) {
      write.csv(resultados(), file, row.names = FALSE)  
    }
  )
  
  
  generar_grafico <- function(datos) {
    req("Lote" %in% names(datos), "Cultivo" %in% names(datos))
    
    # Crear una columna Titulo combinando Lote y Cultivo
    datos <- datos %>%
      mutate(Titulo = paste("Lote", Lote, "-", toupper(Cultivo)))
    
    # Filtrar para solo mostrar "Dosis N (kg/ha)" y cambiar el formato de los datos
    datos_dosis <- datos %>%
      select(Lote, Cultivo, `Dosis N (kg N / ha)`, Titulo) %>%
      mutate(Titulo = paste("Lote", Lote, "-", toupper(Cultivo)))
    
    ggplot(datos_dosis, aes(x = Titulo, y = `Dosis N (kg N / ha)`, fill = "#C52233", color = "#C52233" )) +
      geom_bar(stat = "identity", position = "dodge", size = 0.5, fill = alpha("#C52233", 0.6)) +
      geom_text(aes(label = round(`Dosis N (kg N / ha)`, 0)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.1, 
                size = 5,
                fontface = "bold",
                color = "black") + 
      labs(
        title = "",
        x = "",
        y = "kg N /ha"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(face = "bold", size = 16),  
        axis.text.x = element_text(face = "bold", size = 16),  # Ajustar el tamaño del texto en el eje X
        axis.title.x = element_text(face = "bold", size = 16),  
        axis.title.y = element_text(face = "bold", size = 16),  
        legend.position = "none",  # No mostrar leyenda
        strip.text = element_text(face = "bold", size = 16)  
      ) +
      scale_y_continuous(limits = c(0, max(datos_dosis$`Dosis N (kg N / ha)` + 20, na.rm = TRUE)))
  }
  
  # Usar la función en renderPlot
  output$multi_lotes <- renderPlot({
    req(resultados())
    generar_grafico(resultados())
  })
  
  # Usar la función en downloadHandler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("dosisN_lotes", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Guardar el gráfico como archivo
      png(file, width = 700, height = 500, res = 60)
      print(generar_grafico(resultados()))
      dev.off()
    }
  )
}


# Run the app ----
shinyApp(ui, server )

# rsconnect::forgetDeployment("I:/TRABAJO/CERBAS/Proyectos/Web_fertilizar/fertilizar")
# renv::init()
# renv::restore()
# renv::snapshot()

#  rsconnect::setAccountInfo(name='intabalcarce',
#                            token='C0EB33DC639D60FE1930A4CA5CC8141F',
#                            secret='xQn4aq7hXde1aFaoEpJM5BzIoBEKHw247ACHktKH')

#rsconnect::deployApp(appDir = "I:/TRABAJO/CERBAS/Proyectos/Web_fertilizar/fertilizar", appPrimaryDoc = "app.R",
#                     appName = "Nutrientes", account = 'intabalcarce', server = 'shinyapps.io')
  
  