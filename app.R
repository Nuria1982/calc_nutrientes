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
library(stringr)

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

  
  titlePanel("Plataforma de Recomendación Nutricional para Cultivos Extensivos"),
  
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
               tags$img(src = "Imagen1.jpg", width = "50%", alt = "integrantes")
             ),
             br(),
             h6(HTML("Desarrollada por <a href='https://github.com/Nuria1982' target='_blank'>Nuria Lewczuk</a>
                      <br>Chang et al. (2021). <em>shiny: Web Application Framework for R</em>. R package version 1.7.1, 
                      <a href='https://cran.r-project.org/web/packages/shiny/index.html' 
                      target='_blank'>https://CRAN.R-project.org/package=shiny</a>,
                      <br>Sievert C, Iannone R, Allaire J, Borges B (2023). <em>Flexdashboard: R Markdown Format for Flexible 
                     Dashboards</em>. R package version 0.6.1.9000, <a href='https://pkgs.rstudio.com/flexdashboard/', 
                     target='_blank'>https://pkgs.rstudio.com/flexdashboard/</a>"))
    ),
    
    tabPanel("Carga de datos",
             br(),
             div(style = "background-color: #E0E1DD80; padding: 10px; border-radius: 10px;",
                 h5("Por favor descargue la tabla y complete con los datos solicitados"),
                 downloadButton("descarga_modelo", "Descargar Modelo de Tabla"),
                 br(),
                 br(),
                 h5("Cargue la tabla completa:"),
                 fileInput("archivo_usuario", "Subir archivo de datos",
                           accept = c(".csv", ".xlsx")
                 ),
                 br(),
                 fluidRow(  
                   column(4, uiOutput("zonas_maiz"))
                 )
             )
    ),
    
    tabPanel("Nitrógeno",
             
             h4(HTML("Definición dosis de nitrógeno")),
             
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
                                                selectInput("antecesor", strong(HTML("Efecto Antecesor (kg N / ha)")), 
                                                            choices = c("Soja", "Maiz", "Otros")),
                                                conditionalPanel(
                                                  condition = "input.antecesor == 'Otros'",  
                                                  numericInput("valor_otros", "Ingrese el valor", value = 0)
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.cultivo != 'Trigo'",
                                                numericInput("valor_no_trigo", strong(HTML("Ingrese un valor para el efecto del antecesor (kg N / ha)")), value = 0)
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
                 column(11,  offset = 1,
                        div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center;",
                            div(style = "flex: 1; padding-right: 5px; padding-bottom: 10px;", 
                                uiOutput("demandaN")
                            ),
                            div(style = "flex: 1; padding-right: 5px; padding-bottom: 10px;", 
                                uiOutput("ofertaN")
                            ),
                            div(style = "flex: 1; padding-bottom: 10px;", 
                                class = "gauge-title", 
                                "Dosis de N (kg N / ha)",
                                flexdashboard::gaugeOutput("dosis_nitrogeno", width = "100%", height = "100px")
                            )
                        )
                 )
               )
             ),
               
               tabPanel("Múltiple lotes",
                        h3(strong("Cálculo de la dosis recomendada de nitrógeno para cada lote")),
                        br(),
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            
                            uiOutput("resultados_tabla"),
                            uiOutput("mensaje_advertencia"),
                            br(),
                            downloadButton("download_data", "Descargar resultados (.CSV)")
                        ),
                        br(),
                        br(),
                        fluidRow(
                          column(
                            10, offset = 1,
                              withSpinner(plotOutput("multi_lotes", height = "500px"),
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
        showModal(modalDialog(
          title = "Error",
          "Las contraseñas no coinciden.",
          easyClose = TRUE,
          footer = NULL
        ))
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
        value = 10
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
    } else if (input$nan > 0 && input$cultivo != "Maiz") {
      # Mostrar campo numérico para cultivos distintos de Maíz
      numericInput("valor_no_maiz", 
                   label = strong("Ingrese el valor para la mineralización (kg N / ppm)"), 
                   value = switch(input$cultivo,
                                  "Trigo" = 2.2,
                                  "Papa" = 3.2,
                                  0), # Valor por defecto para otros cultivos
                   min = 0)
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
             0)  
    } else if (input$cultivo != "Maiz" && !is.null(input$valor_no_maiz)) {
      # Usar el valor ingresado por el usuario para otros cultivos
      input$valor_no_maiz
    } else {
      # Valor por defecto si no se selecciona nada
      0
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
      req(input$valor_no_trigo)  
      input$valor_no_trigo
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
  
  output$descarga_modelo <- downloadHandler(
    filename = function() {
      "data_usuario.xlsx"
    },
    content = function(file) {
      # Crear un dataframe modelo
      modelo <- data.frame(
        Lote = c(1, NA), 
        Cultivo = c("maiz", NA), 
        Rendimiento_objetivo = c(10, NA), 
        Efecto_antecesor = c(NA, NA),
        Proteina_objetivo = c(NA, NA),
        N_nitrato_20 = c(NA, NA),
        N_nitrato_40 = c(NA, NA), 
        N_nitrato_60 = c(NA, NA), 
        Nan_20 = c(NA, NA),
        Densidad_aparente = c(1.2, NA)
      )
      
      # Crear la segunda hoja: unidades
      unidades <- data.frame(
        Variable = c("Lote", "Cultivo", "Rendimiento_objetivo", "Efecto_antecesor", "Proteina_objetivo",
                     "N_nitrato_20", "N_nitrato_40", "N_nitrato_60", "Nan_20", "Densidad_aparente"),
        Unidad = c("Número de lote", "Nombre de cultivo", "tn/ha", "kg/ha", "%", "mg/kg", "mg/kg", "mg/kg", "mg/kg", "g/cm³")
      )
      
      # Crear la tercera hoja: aclaraciones
      aclaraciones <- data.frame(
        Campo = c("Lote", "Cultivo", "Rendimiento_objetivo"),
        Detalle = c(
          "Identificación del lote (nombre o código)",
          "Cultivo actual sembrado en el lote",
          "Rendimiento objetivo esperado del cultivo"
        )
      )
      
      # Crear el archivo con múltiples hojas
      writexl::write_xlsx(
        x = list(
          "Modelo" = modelo,
          "Unidades" = unidades,
          "Aclaraciones" = aclaraciones
        ),
        path = file
      )
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
    required_columns <- c("Lote", "Cultivo", "Rendimiento_objetivo", "Efecto_antecesor", "Proteina_objetivo", "N_nitrato_20",
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
    
    # Reemplazar valores vacíos (NA) en `Densidad aparente` por 1.2
    data$densidad_aparente <- ifelse(
      is.na(data$densidad_aparente) | data$densidad_aparente == "", 
      1.2, 
      as.numeric(data$densidad_aparente)
    )
    
    # Reemplazar valores vacíos (NA) con 0 en todas las columnas
    data[is.na(data)] <- 0
    
    # Confirmar al usuario que el archivo se ha procesado correctamente
    showNotification("Archivo subido correctamente.", type = "message")
    
    
    return(data)
  })
  
  observe({
    existe_nan_20 <- "nan_20" %in% colnames(data_usuario)
    
    output$mensaje_advertencia <- renderUI({
      if (!existe_nan_20) {
        HTML("<strong>Cuando el valor de Nan no está disponible, el modelo considera el valor medio de mineralización de la región.</strong>")
      } else {
        NULL  
      }
    })
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
    req(input$zona)
    
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
    
    zona_maiz <- input$zona
    
    datos <- datos %>%
      mutate(
        N_disponible = ((n_nitrato_20 +
                           n_nitrato_40 +
                           n_nitrato_60) * densidad_aparente * 2),
        Mineralizacion = case_when(
          cultivo == "maiz" & zona_maiz == "Sudeste siembra temprana" ~ 3.2,
          cultivo == "maiz" & zona_maiz == "Nucleo siembra temprana" ~ 3.6,
          cultivo == "maiz" & zona_maiz == "Nucleo siembra tardia" ~ 4.2,
          cultivo == "maiz" ~ 1,  
          cultivo == "trigo" ~ 2.2,
          cultivo == "girasol" ~ 0,
          cultivo == "papa" ~ 3.2,
          TRUE ~ 0 
        ),
        Nan_total = ifelse(nan_20 > 0, nan_20 * Mineralizacion, 0)
      )
    
    req_sistema <- c(maiz = 30, trigo = 50, girasol = 60, papa = 6)
    req_planta <- c(maiz = 20, trigo = 30, girasol = 40, papa = 4)
    
    
    # Calcular Oferta, Demanda y DosisN por Lote
    datos <- datos %>%
      mutate(
        Requerimiento = case_when(
          nan_20 > 0 ~ case_when(
            cultivo == "maiz" ~ req_sistema["maiz"],
            cultivo == "trigo" ~ req_sistema["trigo"] + (req_sistema["trigo"] * (proteina_objetivo - 10) / 10),  # Ajuste para trigo cuando nan_20 > 0
            cultivo == "girasol" ~ req_sistema["girasol"],
            cultivo == "papa" ~ req_sistema["papa"],
            TRUE ~ 0
          ),
          nan_20 == 0 ~ case_when(
            cultivo == "maiz" ~ req_planta["maiz"],
            cultivo == "trigo" ~ req_planta["trigo"] + (req_planta["trigo"] * (proteina_objetivo - 10) / 10),  # Ajuste para trigo cuando nan_20 == 0
            cultivo == "girasol" ~ req_planta["girasol"],
            cultivo == "papa" ~ req_planta["papa"],
            TRUE ~ 0
          ),
          TRUE ~ 0
        ),
        OfertaN = N_disponible + coalesce(Nan_total, 0) + coalesce(efecto_antecesor, 0),
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
      "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
      "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
      "<th style='background-color: #06A77D60; padding: 5px;'>Rendimiento <br>(tn / ha)</th>",
      "<th style='background-color: #06A77D60; padding: 5px;'>Demanda <br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>Efecto antecesor <br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>N mineralizable <br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>Nitrogeno Disponible <br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>Oferta <br>(kg N / ha)</th>",
      "<th style='background-color: #C5223360; padding: 5px;'>Dosis <br>(kg N / ha)</th>",
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
      mutate(Titulo = paste("Lote", Lote, "-", toupper(Cultivo)),
             Lote_num = as.numeric(gsub("\\D", "", Lote)))
    
    datos <- datos %>%
      arrange(Lote_num)
    
    # Filtrar para solo mostrar "Dosis N (kg/ha)" y cambiar el formato de los datos
    datos_dosis <- datos %>%
      select(Lote, Cultivo, `Dosis N (kg N / ha)`, Titulo) 
    
    ggplot(datos_dosis,  aes(x = factor(Titulo, levels = unique(Titulo)), y = `Dosis N (kg N / ha)`, fill = "#C52233", color = "#C52233" )) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5), 
               width = 0.6, fill = alpha("#C52233", 0.6), color = "#C52233") +
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
        axis.text.x = element_text(face = "bold", size = 16, angle = 45, hjust = 1),  # Ajustar el tamaño del texto en el eje X
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
      png(file, width = 600, height = 400, res = 60)
      print(
        generar_grafico(resultados()) +
          labs(title = "Dosis recomendada de nitrógeno para cada lote",
               caption = Sys.Date()
               ) +  # Agregar el título
          theme(plot.title = element_text(face = "bold", size = 14, hjust = 0, vjust = 1.1),
                plot.caption = element_text(size = 12, hjust = 1, face = "italic"),
                plot.title.position = "plot")  # Alinear el título a la izquierda
      )
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

# rsconnect::setAccountInfo(name='intabalcarce',
#                            token='C0EB33DC639D60FE1930A4CA5CC8141F',
#                            secret='xQn4aq7hXde1aFaoEpJM5BzIoBEKHw247ACHktKH')

#rsconnect::deployApp(appDir = "I:/TRABAJO/CERBAS/Proyectos/Web_fertilizar/fertilizar", appPrimaryDoc = "app.R",
#                     appName = "Nutrientes", account = 'intabalcarce', server = 'shinyapps.io')
  
  