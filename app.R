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
library(purrr)



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

user_base <- get_user_base()
saveRDS(user_base, "user_base.rds")

# file.exists("base_usuarios")
# db <- dbConnect(RSQLite::SQLite(), "base_usuarios")
# dbListTables(db)
# dbReadTable(db, "user_base")
# #
#  dbGetQuery(db, "PRAGMA table_info(user_base)")
#  dbGetQuery(db, "SELECT * FROM user_base LIMIT 10")
# dbDisconnect(db)

 dosis_data <- data.frame(
   cultivoP = c("soja", "soja", "soja", "soja", 
               "trigo", "trigo", "trigo", "trigo", "trigo", "trigo",
               "maiz", "maiz", "maiz", "maiz", 
               "girasol", "girasol", "girasol", "girasol",
               "papa", "papa", "papa", "papa", "papa", "papa"),
   rango_P = c("< 5 ppm", "5-10 ppm", "10-15 ppm", "> 15 ppm", 
               "< 10 ppm", "10-15 ppm", "15-20 ppm","20-25 ppm", "25-30 ppm","> 30 ppm",
               "< 10 ppm", "10-15 ppm", "15-20 ppm", "> 20 ppm",
               "< 5 ppm", "5-10 ppm", "10-15 ppm", "> 15 ppm", 
               "< 10 ppm", "10-15 ppm", "15-20 ppm","20-25 ppm", "25-30 ppm","> 30 ppm"),
   dosis = c("15-20 kg/ha", "10-15 kg/ha", "5-10 kg/ha", "0", 
             "20-25 kg/ha", "15-20 kg/ha", "10-15 kg/ha", "5-10 kg/ha", "0", "0",
             "15-20 kg/ha", "10-15 kg/ha", "8-10 kg/ha", "0", 
             "15-20 kg/ha", "10-15 kg/ha", "5-10 kg/ha", "0",
             "100 kg/ha", "80 kg/ha", "60 kg/ha", "40 kg/ha", "20 kg/ha", "0"),
   P_min = c(0, 5, 10, 15, 
             0, 10, 15, 20, 25, 30,
             0, 10, 15, 20,
             0, 5, 10, 15,
             0, 10, 15, 20, 25, 30),
   P_max = c(5, 10, 15, Inf, 
             10, 15, 20, 25, 30, Inf,
             10, 15, 20, Inf,
             5, 10, 15, Inf,
             10, 15, 20, 25, 30, Inf)
 )
 dosis_data <- dosis_data %>%
   mutate(
     min_dosis = ifelse(dosis == "0", 0, 
                        ifelse(grepl("kg/ha", dosis), as.numeric(gsub(" kg/ha", "", sapply(strsplit(dosis, "-"), "[", 1))), NA)),
 
     max_dosis = case_when(
       dosis == "0" ~ 0,  
       dosis == "100 kg/ha" ~ 100,  
       dosis == "80 kg/ha" ~ 80,  
       dosis == "60 kg/ha" ~ 60,  
       dosis == "40 kg/ha" ~ 40,  
       dosis == "20 kg/ha" ~ 20,  
       grepl("kg/ha", dosis) ~ as.numeric(gsub(" kg/ha", "", sapply(strsplit(dosis, "-"), "[", 2))),  
       TRUE ~ NA_real_  
     )
   )
 
 
################################################################################


ui <- fluidPage(
  useShinyjs(),
  
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),

  
  titlePanel("Plataforma de Recomendación Nutricional para Cultivos Extensivos"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
    
    
    body {
        background-image: url('imagen2.jpg'); 
        background-size: cover; 
        background-attachment: fixed; 
        background-position: center;
        opacity: 0.9; 
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
        gap: 15px; /* Espacio entre los elementos */
        width: 100%;
      }
      .gauge-box {
        border: 2px solid #dcdcdc;
        border-radius: 10px;
        padding: 15px;
        background-color: #f7f7f7;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        width: 100%; 
        text-align: center; 
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
                                   style = "text-align: center; font-size: 0.85em; margin-top: -10px;"))
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
    actionButton("enviar_registro", "Enviar Registro")
  ),

  br(),
  br(),
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Principal",
             br(),
             h5(HTML("El manejo responsable de nutrientes y de los fertilizantes, en los sistemas agrícolas 
                      se basa en aplicar la fuente correcta, la dosis, en el momento y el lugar correctos, constituyendo 
                      el concepto de los 4 Requisitos (4R) para las mejores prácticas de manejo de nutrientes y fertilizantes. 
                      Este enfoque considera dimensiones económicas, sociales y ambientales del manejo de nutrientes y 
                      es esencial para la sostenibilidad de los sistemas agrícolas. La implementación de los 4Rs es intensiva 
                      en cuanto a conocimiento y específica para cada sitio.")),
             h4(HTML("<strong>La plataforma tiene como principal objetivo facilitarles a los usuarios el cálculo 
                    de las dosis orientativa de nutrientes (uno de los 4R) requeridas por los principales cultivos extensivos.
                    Para lo cual, la misma está desarrollada en base a modelos publicados en diferentes revistas científicas 
                    y validados a nivel de lotes de producción.</strong>")),
             br(),
             h4(HTML("<strong>Trayectoria</strong>")),
             h5(HTML("La plataforma fue desarrollada y es actualizada por investigadores del grupo de 
                     Relación Suelo - Cultivo de la Unidad Integrada Balcarce, los cuales presentan una amplia 
                     trayectoria en investigación, docencia y transferencia en temas de fertilidad de suelos y 
                     nutrición de cultivos. Además, el grupo es responsable del Laboratorio de Suelo de INTA Balcarce que 
                     hace más de 30 años brinda no solo servicios de análisis de suelo y planta sino también actividades como 
                     experimentación a campo, asesoramiento en fertilidad de suelos, charlas técnicas, jornadas 
                     y cursos de actualización profesional.")),
             br(),
             h6(HTML("Puede acceder a la lista de servicios ofrecidos por el laboratorio aquí: <a href='servicios_lab_suelos_INTA_Balcarce.pdf' target='_blank'>Laboratorio de suelos</a>")),
             br(),
             div(
               style = "text-align: center;",
               tags$img(src = "Imagen1.jpg", width = "50%", alt = "integrantes")
             ),
             br(),
             br(),
             
             div(
               style = "display: flex; justify-content: center; align-items: center; gap: 80px;",
               tags$img(src = "Logo_suelo_cultivo.png", width = "10%", alt = "Grupo_Suelo_Cultivo"),
               tags$img(src = "logoFCA2.png", width = "10%", alt = "FCA"),
               tags$img(src = "IPADS.png", width = "10%", alt = "IPADS")
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
                 # p("Haga clic en el botón para descargar el archivo PDF con las instrucciones."),
                 h4("Haga clic en el botón para descargar el archivo PDF con las instrucciones."),
                 downloadButton("descargar_instrucciones", "Descargar PDF"),
                 br(),
                 br(),
                 h5("Por favor descargue la tabla y complete con los datos solicitados"),
                 downloadButton("descarga_modelo", "Descargar Modelo de Tabla"),
                 br(),
                 br(),
                 h5("Cargue la tabla con los datos solicitados:"),
                 fileInput("archivo_usuario", "Subir archivo de datos",
                           accept = c(".csv", ".xlsx")
                 ),
                 br(),
                 fluidRow(  
                   column(4, uiOutput("zonas_maiz")),
                   column(4, uiOutput("zona_multi_s"))
                 )
                 
             )
    ),
    
    tabPanel("Nitrógeno",
             
             h4(HTML("Definición dosis de nitrógeno")),
             
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
                                                           value = 1),
                                              numericInput("nitrato_40",  
                                                           label = strong(HTML("N-nitrato (ppm) (20-40cm)")),
                                                           value = 1),
                                              numericInput("nitrato_60",  
                                                           label = strong(HTML("N-nitrato (ppm) (40-60cm)")),
                                                           value = 1),
                                              numericInput("dens_ap",  
                                                           label = strong(HTML("Densidad aparente (g / cm<sup>3</sup>)")), 
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
               
               tabPanel("Múltiples lotes", 
                        value = "seccion_nitrogeno",
                        h3(strong("Cálculo de la dosis recomendada de nitrógeno para cada lote")),
                        
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            
                            uiOutput("tabla_nitrogeno"),
                            
                            br(),
                            downloadButton("descarga_N", "Descargar resultados (.xlsx)")
                        ),
                        br(),
                        br(),
                        fluidRow(
                          column(
                            10, offset = 1,
                              withSpinner(plotOutput("multi_lotes_N", height = "500px"),
                                          type = 5,
                                          color = "#0dc5c1",
                                          size = 0.5),
                            div(style = "text-align: right; margin-top: 10px;",
                                downloadButton("download_plot_N", "Descargar Gráfico", class = "btn btn-success")
                            )
                          )
                        )
               )
             )
    ),
    
    tabPanel("Fósforo",
             h4(HTML("Definición dosis de fósforo")),
             
             tabsetPanel(
               tabPanel("Lote único",
                        fluidRow(
                          column(6,
                                 selectInput("cultivoP",
                                             label = strong("Seleccione el cultivo:"),
                                             choices = list(
                                               "Maíz" = "maiz",
                                               "Soja" = "soja",
                                               "Trigo/Cebada" = "trigo",
                                               "Girasol" = "girasol",
                                               "Papa" = "papa",
                                               "Doble cultivo" = "doble_cultivo"
                                             ),
                                             selected = "maiz"
                                 ),
                                 conditionalPanel(
                                   condition = "input.cultivoP == 'doble_cultivo'",
                                   fluidRow(
                                     column(6,
                                            selectInput(
                                              "cultivoP_1",
                                              label = strong("Seleccione el cultivo 1º:"),
                                              choices = list(
                                                "Maíz" = "maiz",
                                                "Soja" = "soja",
                                                "Trigo/Cebada" = "trigo",
                                                "Girasol" = "girasol",
                                                "Papa" = "papa"
                                              )
                                            )
                                            ),
                                            column(6,
                                                   selectInput(
                                                     "cultivoP_2",
                                                     label = strong("Seleccione el cultivo 2º:"),
                                                     choices = list(
                                                       "Maíz" = "maiz",
                                                       "Soja" = "soja",
                                                       "Trigo/Cebada" = "trigo",
                                                       "Girasol" = "girasol",
                                                       "Papa" = "papa"
                                                     )
                                                   )
                                   )
                                 )
                          )
                        )
                        ),
                        fluidRow(
                          column(4, 
                                 div(style = "background-color: #58815780; padding: 15px; border-radius: 10px;",
                                     h4(strong("Dosis de suficiencia (kg P / ha)")),
                                     fluidRow(
                                       column(12,
                                              h5(strong("P extractable (P Bray 0 -20 cm)")),
                                              numericInput("P_Bray_actual",  
                                                           label = strong(HTML("Ingrese el valor")),
                                                           value = 1
                                              )
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 div(style = "background-color: #BC6C2580; padding: 15px; border-radius: 10px;",
                                     h4(strong("Dosis de construcción y/o mantenimiento")),
                                     fluidRow(
                                       column(4,
                                              h5(strong("Construcción")),
                                              numericInput("NivelP",  
                                                           label = strong(HTML("Nivel de P objetivo (ppm)")), 
                                                           value = 0
                                              ),
                                              numericInput("factor_construccion",  
                                                           label = strong(HTML("Factor de construcción (kg P/ppm)")), 
                                                           value = 0
                                              ),
                                              uiOutput("construir_P")
                                       ),
                                       column(4,
                                              h5(strong("Mantenimiento")),
                                              numericInput("rendimiento_P",  
                                                           label = strong(HTML("Rendimiento objetivo (t/ha)")),
                                                           value = 1
                                              ),
                                              numericInput("factor_mantenimiento",  
                                                           label = strong(HTML("Nutriente en grano (kg P/t)")), 
                                                           value = 0
                                              ),
                                              uiOutput("mantener_P")
                                       ),
                                       column(4,
                                              conditionalPanel(
                                                condition = "input.cultivoP == 'doble_cultivo'",
                                                h5(strong("Mantenimiento")),
                                                numericInput("rendimiento_P_2",  
                                                             label = strong(HTML("Rendimiento objetivo del cultivo 2º (t/ha)")),
                                                             value = 1
                                                ),
                                                numericInput("factor_mantenimiento_2",  
                                                             label = strong(HTML("Nutriente en grano (kg P/t)")), 
                                                             value = 0
                                                ),
                                                uiOutput("mantener_P_2")
                                              )
                                       )
                                     )
                                 )
                          )
                          ),
                        br(),
                        br(),
                        
                        fluidRow(
                          column(5, offset = 1,
                                 div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center;",
                                     div(style = "flex: 1; padding-right: 5px; padding-bottom: 10px;", 
                                         uiOutput("dosis_suficiencia")
                                     )
                                 )
                          ),
                          column(5, offset = 1,
                                 div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center;",
                                     div(style = "flex: 1; padding-right: 5px; padding-bottom: 10px;", 
                                         uiOutput("dosisCyM")
                                     )
                                 )
                          )
                        )
               ),
               tabPanel("Múltiples lotes",
                        h3(strong("Cálculo de la dosis recomendada de fósforo para cada lote")),
                        br(),
                        
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            
                            uiOutput("tabla_fosforo"),
                            br(),
                            downloadButton("descarga_P", "Descargar resultados (.xlsx)")
                        )
               )
             )
    ),
    
    tabPanel("Azufre",
             h4(HTML("Definición dosis de azufre")),
             
             tabsetPanel(
               tabPanel("Lote único",
                        fluidRow(
                          column(6,
                                 selectInput("cultivoS",
                                             label = strong("Seleccione el cultivo:"),
                                             choices = list(
                                               "Maíz" = "maiz",
                                               "Soja" = "soja",
                                               "Trigo/Cebada" = "trigo",
                                               "Girasol" = "girasol",
                                               "Papa" = "papa"
                                             ),
                                             selected = "maiz"
                                 )
                          )
                        ),
                        fluidRow(
                          column(6, 
                                 div(style = "background-color: #DEB84180; padding: 15px; border-radius: 10px;",
                                     
                                     fluidRow(
                                       column(6, 
                                              numericInput("azufre_20",  
                                                           label = strong(HTML("S-sulfato (ppm) (0-20cm)")),
                                                           value = 0),
                                              numericInput("azufre_40",  
                                                           label = strong(HTML("S-sulfato (ppm) (20-40cm)")),
                                                           value = 0),
                                              numericInput("azufre_60",  
                                                           label = strong(HTML("S-sulfato (ppm) (40-60cm)")),
                                                           value = 0),
                                              numericInput("dens_ap_s",  
                                                           label = strong(HTML("Densidad aparente (g / cm<sup>3</sup>)")), 
                                                           value = 1.2),
                                              fluidRow(
                                                column(12, 
                                                       div(style = "background-color: #FF991490; padding: 10px; border-radius: 10px; text-align: center;",
                                                           h6(HTML("<strong>Azufre Disponible</strong><br><small>(0-60cm, kg S/ha)</small>")),
                                                           div(style = "font-size: 18px; font-weight: bold;",
                                                               uiOutput("azufre_disp"))
                                                       )
                                                )
                                              )
                                       ),
                                       column(6,
                                              numericInput("nan_s",  
                                                           label = strong(HTML("Nan (0-20cm, ppm)")),
                                                           value = 0),
                                              
                                              selectInput("zona_s", strong(HTML("Seleccionar zona del cultivo")), 
                                                          choices = c("Sudeste de Bs.As", "Otras zonas")))
                                     )
                                 )
                          ),
                          column(4, offset = 1,
                                 div(style = "background-color: #DE9E3680; padding: 15px; border-radius: 10px;",
                                     h4(strong("Datos para el cálculo de la demanda de azufre")),
                                     fluidRow(
                                       column(12,
                                              numericInput("rendimiento_s",  
                                                           label = strong(HTML("Rendimiento objetivo (t/ha)")),
                                                           value = 1
                                              ),
                                              numericInput("factor_s",  
                                                           label = strong(HTML("Nutriente en grano (kg S/t)")), 
                                                           value = 0
                                              )
                                       )
                                     )
                                     ),
                                 br(),
                                     fluidRow(
                                       column(10, offset = 2,
                                              div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center;",
                                                  div(style = "flex: 1; padding-right: 5px; padding-bottom: 10px;", 
                                                      uiOutput("dosis_s")
                                                  )
                                              )
                                       )
                                     )
                                 
                          )
                        )
               ),
                 tabPanel("Múltiples lotes", 
                          value = "seccion_azufre",
                          h3(strong("Cálculo de la dosis recomendada de azufre para cada lote")),
                         
                          
                          div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                              
                              uiOutput("tabla_azufre"),
                              br(),
                              downloadButton("descarga_S", "Descargar resultados (.xlsx)")
                          )
                 )
               )
             ),
    tabPanel("Zinc",
             h4(HTML("Definición dosis de zinc")),
             
             tabsetPanel(
               tabPanel("Lote único",
                        fluidRow(
                          column(6,
                                 selectInput("cultivoZ",
                                             label = strong("Seleccione el cultivo:"),
                                             choices = list(
                                               "Maíz" = "maiz",
                                               "Soja" = "soja",
                                               "Trigo/Cebada" = "trigo",
                                               "Girasol" = "girasol",
                                               "Papa" = "papa"
                                             ),
                                             selected = "maiz"
                                 )
                          )
                        ),
                        fluidRow(
                          column(4, 
                                 div(style = "background-color: #E5E5E580; padding: 15px; border-radius: 10px;",
                                     
                                     fluidRow(
                                       column(12, 
                                              numericInput("zinc",  
                                                           label = strong(HTML("Zn - DTPA (ppm) (0-20cm)")),
                                                           value = 0,
                                                           step = 0.1)
                                              
                                       )
                                     )
                                 )
                          ),
                          column(4, 
                                 div(style = "background-color: #778DA980; padding: 15px; border-radius: 10px;",
                                     h4(strong("Datos para el cálculo de la demanda de zinc")),
                                     fluidRow(
                                       column(6,
                                              numericInput("rendimiento_z",  
                                                           label = strong(HTML("Rendimiento objetivo (t/ha)")),
                                                           value = 1
                                              )
                                              ),
                                       column(6,
                                              numericInput("factor_z",  
                                                           label = strong(HTML("Nutriente en grano (g Zn/t)")), 
                                                           value = 0
                                              )
                                       )
                                       )
                                     )
                                 ),
                                 br(),
                                 fluidRow(
                                   column(10, offset = 2,
                                          div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center;",
                                              div(style = "flex: 1; padding-right: 5px; padding-bottom: 10px;", 
                                                  uiOutput("dosis_z")
                                              )
                                          )
                                   )
                                 )
                        )
                          
                        
               ),
               tabPanel("Múltiples lotes", 
                        value = "seccion_zinc",
                        h3(strong("Cálculo de la dosis recomendada de zinc para cada lote")),
                        br(),
                        fluidRow(  
                          column(4, 
                                 uiOutput("zona_multi_z"))
                        ),
                        
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            
                            uiOutput("tabla_zinc"),
                            br(),
                            downloadButton("descarga_Z", "Descargar resultados (.xlsx)")
                        )
               )
             )
    ),
    
    tabPanel("Recomendaciones",
             br(),
             h2(HTML("<strong>Recomendaciones</strong>")),
             # br(),
             # h4(HTML("Para obtener recomendaciones sobre la nutrición de sus cultivos:")),
             # h6(HTML("1º: Ingrese los datos solicitados en Carga de datos")),
             # h6(HTML("2º: Ingrese la zona geográfica en la sección Nitrógeno - Múltiples lotes")),
             # h6(HTML("3º: Ingrese la zona geográfica en la sección Azufre - Múltiples lotes")),
             
             br(),
             div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                 
                 uiOutput("tabla_total"),
                 br(),
                 downloadButton("descarga_total", "Descargar resultados (.xlsx)")
             ),
             br(),
             div(style = "background-color: #C5223340; padding: 15px; border-radius: 10px;",
                 
             ),
             br(),
             div(style = "background-color: #BC6C2540; padding: 15px; border-radius: 10px;",
                 
             ),
             br(),
             div(style = "background-color: #DEB84140; padding: 15px; border-radius: 10px;",
                 
             ),
             br(),
             div(style = "background-color: #415A7740; padding: 15px; border-radius: 10px;",
                 
             )
    ),
    tabPanel("Monitoreo",
             
             h4(HTML("Monitoreo de nitrógeno")),
             
             tabsetPanel(
               tabPanel("Lote único",
                        fluidRow(
                          column(4,
                                 selectInput("cultivo_monitoreo",
                                             label = strong("Seleccione el cultivo:"),
                                             choices = list(
                                               "Maíz" = "maiz",
                                               # "Soja" = "soja",
                                               "Trigo/Cebada" = "trigo",
                                               # "Girasol" = "girasol",
                                               "Papa" = "papa"
                                             ),
                                             selected = "maiz"
                                 )
                          ),
                          column(4,
                                 numericInput("índice_monitoreo",  
                                              label = strong(HTML("Índice de vegetación")),
                                              value = 0.1,
                                              step = 0.01
                                 )
                          ),
                          column(4,
                                 numericInput("IFR_monitoreo",  
                                              label = strong(HTML("Índice de franja de referencia")), 
                                              value = 0.1,
                                              step = 0.01
                                 )
                          )
                        ),
                        br(),
                        br(),
                        
                        fluidRow(
                          column(4, offset = 2,
                                 uiOutput("indice_suf_nitrogeno"),
                                 br(),
                                 uiOutput("dosis_monitoreo")
                          ),
                          column(6,
                                 plotOutput("grafico_monitoreo")
                                 
                          )
                        ),
                        
                        
                        
               ),
               tabPanel("Múltiples lotes",
                        h5("Por favor descargue la tabla y complete con los datos solicitados"),
                        downloadButton("tabla_monitoreo", "Descargar tabla para monitoreo"),
                        br(),
                        br(),
                        fileInput("archivo_monitoreo", "Cargue la tabla con los datos solicitados:",
                                  accept = c(".csv", ".xlsx")
                        )
                        ,
                        br(),
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            uiOutput("tabla_monitoreoN"),
                            br(),
                            downloadButton("descarga_monitoreoN", "Descargar resultados (.xlsx)")
                        )
               )
             )
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

################################################################################
  ## Carga de datos ######
  
  output$descarga_modelo <- downloadHandler(
    filename = function() {
      "data_usuario.xlsx"
    },
    content = function(file) {
      # Crear un dataframe modelo
      datos <- data.frame(
        Lote = c(1, 1, 1, 2, 2, 2), 
        Cultivo = c("maiz", "maiz", "maiz", "trigo", "trigo", "trigo"),
        Rendimiento_objetivo = c(NA, NA, NA, NA, NA, NA), 
        Cultivo_segunda = c(NA, NA, NA, "soja", NA, NA),
        Rendimiento_objetivo_cultivo_segunda = c(NA, NA, NA, NA, NA, NA), 
        Efecto_antecesor = c(NA, NA, NA, NA, NA, NA),
        Proteina_objetivo = c(NA, NA, NA, NA, NA, NA),
        Estrato = c("0-20", "20-40", "40-60", "0-20", "20-40", "40-60"),
        Densidad_aparente = c(1.2, NA, NA, 1.0, NA, NA),
        P_Bray_actual = c(NA, NA, NA, NA, NA, NA),
        Zn_DTPA = c(NA, NA, NA, NA, NA, NA),
        Nan = c(NA, NA, NA, NA, NA, NA),
        N_nitrato = c(NA, NA, NA, NA, NA, NA),
        S_sulfato = c(NA, NA, NA, NA, NA, NA),
        nivelP_objetivo = c(NA, NA, NA, NA, NA, NA),
        Nutriente_en_grano_P = c(NA, NA, NA, NA, NA, NA),
        Nutriente_en_grano_S = c(NA, NA, NA, NA, NA, NA),
        Nutriente_en_grano_Z = c(NA, NA, NA, NA, NA, NA)
      )
      
  
      
      # Renombrar las columnas con las unidades correspondientes
      column_units <- c(
        Lote = "Lote",
        Cultivo = "Cultivo",
        Rendimiento_objetivo = "Rendimiento (tn/ha)",
        Cultivo_segunda = "Cultivo segunda",
        Rendimiento_objetivo_cultivo_segunda = "Rendimiento cultivo segunda (tn/ha)",
        Efecto_antecesor = "Efecto antecesor (kg/ha)",
        Proteina_objetivo = "Proteína objetivo (%)",
        Estrato = "Estrato (cm)",
        Densidad_aparente = "Densidad aparente (g/cm³)",
        P_Bray_actual = "P Bray actual (ppm)",
        Zn_DTPA = "Zn - DTPA (ppm)",
        Nan = "Nan (mg/kg)",
        N_nitrato = "N-Nitrato (mg/kg)",
        S_sulfato = "Sulfato (mg/kg)",
        nivelP_objetivo = "Nivel P objetivo (ppm)",
        Nutriente_en_grano_P = "Nutriente en grano P (kg/t)",
        Nutriente_en_grano_S = "Nutriente en grano S (kg/t)",
        Nutriente_en_grano_Z = "Nutriente en grano Z (kg/t)"
      )
      
      # Aplicar los nuevos nombres al data.frame
      colnames(datos) <- column_units[colnames(datos)]
      
      
      
      library(openxlsx)
      
      wb <- createWorkbook()
      
     
      addWorksheet(wb, "Datos")
      
      writeData(wb, "Datos", datos)
      
      
      estilo_general1 <- createStyle(fgFill = "gray90",
                                     textDecoration = "bold",
                                     border = "Bottom",
                                     borderColour = "black",
                                     borderStyle = "thin",
                                     wrapText = TRUE )
      estilo_general2 <- createStyle(fgFill = "gray90",
                                     borderColour = "black",
                                     borderStyle = "thin",
                                     wrapText = TRUE )
      estilo_general3 <- createStyle(border = "Bottom",
                                     borderColour = "black",
                                     borderStyle = "thin",
                                     wrapText = TRUE )
      estilo_general4 <- createStyle(fgFill = "gray90")
      estilo_general5 <- createStyle(border = "Bottom",
                                     fgFill = "gray90")
      estilo_aclaracion <- createStyle(textDecoration = c("bold", "italic"),
                                       fontSize = 16)

     
      addStyle(wb, "Datos", style = estilo_general1, rows = 1, cols = c(1:18), gridExpand = TRUE)
      addStyle(wb, "Datos", style = estilo_general2, rows = c(2, 5), cols = c(1:18), gridExpand = TRUE)
      addStyle(wb, "Datos", style = estilo_general5, rows = c(4, 7), cols = c(8,13), gridExpand = TRUE)
      addStyle(wb, "Datos", style = estilo_general4, rows = c(3, 6), cols = c(8,13), gridExpand = TRUE)
      addStyle(wb, "Datos", style = estilo_general3, rows = c(4, 7), cols = c(1:7, 9:12, 14:18), gridExpand = TRUE)
     
      saveWorkbook(wb, file, overwrite = TRUE)
      
      
    }
  )
  
  
  data_usuario <- reactive({
    if (is.null(input$archivo_usuario)) {
      return(NULL)  
    }
    
    ext <- tools::file_ext(input$archivo_usuario$name)
    
    
    if (ext == "csv") {
      data <- read.csv(input$archivo_usuario$datapath)
    } else if (ext == "xlsx") {
      data <- readxl::read_xlsx(input$archivo_usuario$datapath, sheet = "Datos")
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
      return(NULL)
    }
    
    
    # Verificar si el archivo tiene las columnas requeridas
    required_columns <- c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Cultivo segunda", "Rendimiento cultivo segunda (tn/ha)", "Efecto antecesor (kg/ha)", "Proteína objetivo (%)",
                          "Estrato (cm)", "Densidad aparente (g/cm³)", "P Bray actual (ppm)", "Zn - DTPA (ppm)", "Nan (mg/kg)", "N-Nitrato (mg/kg)", "Sulfato (mg/kg)","Nivel P objetivo (ppm)", 
                          "Nutriente en grano P (kg/t)", "Nutriente en grano S (kg/t)", "Nutriente en grano Z (kg/t)")
    missing_columns <- setdiff(required_columns, colnames(data))
    
    if (length(missing_columns) > 0) {
      showNotification(
        paste("El archivo no tiene las columnas requeridas:", 
              paste(missing_columns, collapse = ", ")), 
        type = "error"
      )
      return(NULL) 
    }
    
    # Renombrar las columnas con las unidades correspondientes
    column_original <- c(
      Lote = "Lote",
      Cultivo = "Cultivo",
      `Rendimiento (tn/ha)` = "Rendimiento_objetivo" ,
      `Cultivo segunda` = "Cultivo_segunda",
      `Rendimiento cultivo segunda (tn/ha)` = "Rendimiento_objetivo_cultivo_segunda",
      `Efecto antecesor (kg/ha)` = "Efecto_antecesor",
      `Proteína objetivo (%)` = "Proteina_objetivo",
      `Estrato (cm)` = "Estrato",
      `Densidad aparente (g/cm³)` = "Densidad_aparente",
      `P Bray actual (ppm)` = "P_Bray_actual",
      `Zn - DTPA (ppm)` = "Zn_DTPA",
      `Nan (mg/kg)` = "Nan",
      `N-Nitrato (mg/kg)` = "N_nitrato",
      `Sulfato (mg/kg)` = "S_sulfato",
      `Nivel P objetivo (ppm)` = "nivelP_objetivo",
      `Nutriente en grano P (kg/t)` = "Nutriente_en_grano_P",
      `Nutriente en grano S (kg/t)` = "Nutriente_en_grano_S",
      `Nutriente en grano Z (kg/t)` = "Nutriente_en_grano_Z"
    )
    
    # Aplicar los nuevos nombres al data.frame
    colnames(data) <- column_original[colnames(data)]
    
    colnames(data) <- tolower(colnames(data))
    data$cultivo <- tolower(data$cultivo)
    data$cultivo_segunda <- tolower(data$cultivo_segunda)
    

    
    data <- data %>%
      tidyr::fill(lote, cultivo, .direction = "down")
    
    
    data <- data %>%
      mutate(
        n_nitrato_20 = ifelse(estrato == "0-20", n_nitrato, 0),
        n_nitrato_40 = ifelse(estrato == "20-40", n_nitrato, 0),
        n_nitrato_60 = ifelse(estrato == "40-60", n_nitrato, 0),
        s_sulfato_20 = ifelse(estrato == "0-20", s_sulfato, 0),
        s_sulfato_40 = ifelse(estrato == "20-40", s_sulfato, 0),
        s_sulfato_60 = ifelse(estrato == "40-60", s_sulfato, 0)
      ) 
    
    
    data <- data %>%
      group_by(lote, cultivo) %>%
      summarise(
        rendimiento_objetivo = first(rendimiento_objetivo),
        cultivo_segunda = first(cultivo_segunda),
        rendimiento_objetivo_cultivo_segunda = first(rendimiento_objetivo_cultivo_segunda),
        efecto_antecesor = first(efecto_antecesor),
        proteina_objetivo = first(proteina_objetivo),
        nan = first(nan),
        densidad_aparente = first(densidad_aparente),
        n_nitrato_20 = max(n_nitrato_20),
        n_nitrato_40 = max(n_nitrato_40),
        n_nitrato_60 = max(n_nitrato_60),
        s_sulfato_20 = max(s_sulfato_20),
        s_sulfato_40 = max(s_sulfato_40),
        s_sulfato_60 = max(s_sulfato_60),
        p_bray_actual = first(p_bray_actual),
        zn_dtpa = first(zn_dtpa),
        nivelp_objetivo = first(nivelp_objetivo),
        nutriente_en_grano_p = first(nutriente_en_grano_p),
        nutriente_en_grano_s = first(nutriente_en_grano_s),
        nutriente_en_grano_z = first(nutriente_en_grano_z)
      ) %>%
      ungroup()
    
    
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
    
    data <- data %>%
      mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))
    
    excluir_columnas <- c("n_nitrato_20", "n_nitrato_40", "n_nitrato_60", "zn_dtpa", "p_bray_actual", "s_sulfato_20", "s_sulfato_40", "s_sulfato_60")
    # Reemplazar valores vacíos (NA) con 0 en todas las columnas
    data <- data %>%
      mutate(across(
        .cols = -all_of(excluir_columnas), # Selecciona todas las columnas excepto las excluidas
        .fns = ~ ifelse(is.na(.), 0, .)   # Reemplaza NA por 0
      ))
   
    # Confirmar al usuario que el archivo se ha procesado correctamente
    showNotification("Archivo subido correctamente.", type = "message")
    
    
    return(data)
  })
  
  output$descargar_instrucciones <- downloadHandler(
    filename = function() {
      "Instrucciones_nutrición_cultivos.pdf"  
    },
    content = function(file) {
      file.copy("www/Instrucciones_nutrición_cultivos.pdf", file)
    }
  )
  
  
############################# Nitrogeno ########################################
  
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
    
    if (!is.na(input$nitrato_20) && !is.na(input$nitrato_40) && !is.na(input$nitrato_60) &&
        !is.null(input$nitrato_20) && !is.null(input$nitrato_40) && !is.null(input$nitrato_60)) {
      (input$nitrato_20 + input$nitrato_40 + input$nitrato_60) * (densidad_aparente * 2)
    } else {
      NULL  # Retorna NULL si hay valores faltantes
    }
  })
  
  output$nitrogeno_disp <- renderUI({
    valor_nitrogeno <- nitrogeno_disp()  # Almacenar el valor reactivo
    
    if (is.null(valor_nitrogeno)) {
      # Mostrar mensaje de error en rojo si no hay datos suficientes
      div(style = "color: red;", "Para calcular la oferta de N debe ingresar los datos de N_nitrato (0-20, 20-40, 40-60).")
    } else {
      # Mostrar el resultado redondeado
      round(valor_nitrogeno, 0)
    }
  })
  
  output$zonas_ui <- renderUI({
    if (input$nan > 0 && input$cultivo == "Maiz") {
      selectInput("zona_maiz", 
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
    if (input$cultivo == "Maiz" && !is.null(input$zona_maiz)) {
      # Si el cultivo es maíz, se usa el valor según la zona seleccionada
      switch(input$zona_maiz,
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
      round(nan_total(), 0)
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
    round(efecto_antecesor(), 0)
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

  
  
  output$dosis_nitrogeno <- renderGauge({
    req(demandaN(), dosisN(), input$cultivo, input$rendimiento)
    
    # Definir el factor para cada cultivo
    factor <- switch(input$cultivo,
                     "Trigo" = 20,
                     "Maiz" = 12,
                     "Papa" = 12,
                     "Girasol" = 24,
                     0)  # Si el cultivo no es reconocido, el factor será 0.
    
    # Calcular el max_val (rendimiento * factor + 80)
    max_val <- input$rendimiento * factor + 80
    
    # Dividir el rango en tres partes iguales (tercio)
    tercio <- max_val / 3
    success_range <- c(0, (input$rendimiento * factor))
    
    danger_range <- c((input$rendimiento * factor), max_val)
    
    # Renderizar la gauge con el valor máximo calculado
    gauge(dosisN(), min = 0, max = max_val, 
          gaugeSectors(success = success_range, danger = danger_range)
    )
  })


  #Múltiples lotes

  
  output$zonas_maiz <- renderUI({
    req(data_usuario())
    if ("maiz" %in% data_usuario()$cultivo) {
      selectInput("zona_multi_maiz", 
                  label = strong("Seleccione la zona geográfica para el cultivo de Maíz"),
                  choices = c("Sudeste siembra temprana", 
                              "Núcleo siembra temprana", 
                              "Núcleo siembra tardia"),
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
  
  resultados_nitrogeno <- reactive({
    req(data_usuario())
    #req(input$zona_multi_maiz)
    
    datos <- data_usuario()
    
    if (nrow(datos) == 0) {
      return(NULL)  # Detenemos la ejecución si no hay datos
    }
    
    datos$cultivo <- trimws(datos$cultivo)
    
    datos$antecesor <- trimws(datos$efecto_antecesor)
    
    # Convertir columnas anuméricas
    datos$rendimiento_objetivo <- as.numeric(datos$rendimiento_objetivo)
    datos$n_nitrato_20 <- as.numeric(datos$n_nitrato_20)
    datos$n_nitrato_40 <- as.numeric(datos$n_nitrato_40)
    datos$n_nitrato_60 <- as.numeric(datos$n_nitrato_60)
    datos$nan <- as.numeric(datos$nan)
    datos$densidad_aparente <- as.numeric(datos$densidad_aparente)

    zona_maiz <- input$zona_multi_maiz
    
    datos <- datos %>%
      mutate(
        Mineralizacion = case_when(
          cultivo == "maiz" & zona_maiz == "Sudeste siembra temprana" ~ 3.2,
          cultivo == "maiz" & zona_maiz == "Núcleo siembra temprana" ~ 3.6,
          cultivo == "maiz" & zona_maiz == "Núcleo siembra tardia" ~ 4.2,
          cultivo == "maiz" ~ 1,  
          cultivo == "trigo" ~ 2.2,
          cultivo == "girasol" ~ 0,
          cultivo == "papa" ~ 3.2,
          TRUE ~ NA_real_  
        ),
        Nan_total = ifelse(nan > 0, nan * Mineralizacion, NA)
      )
    
    datos <- datos %>%
      group_by(lote) %>%
      mutate(
        N_disponible = ifelse(
          !is.na(n_nitrato_20) & !is.na(n_nitrato_40) & !is.na(n_nitrato_60),
          round(((n_nitrato_20 + n_nitrato_40 + n_nitrato_60) * 2 * densidad_aparente),0),
          NA_real_  # Si alguno de ellos es NA, asigna NA
        )
      ) 
      
    req_sistema <- c(maiz = 30, trigo = 50, girasol = 60, papa = 6)
    req_planta <- c(maiz = 20, trigo = 30, girasol = 40, papa = 4)
    

    # Calcular Oferta, Demanda y DosisN por Lote
    datos <- datos %>%
      mutate(
        Requerimiento = case_when(
          nan > 0 ~ case_when(
            cultivo == "maiz" ~ req_sistema["maiz"],
            cultivo == "trigo" ~ req_sistema["trigo"] + (req_sistema["trigo"] * (proteina_objetivo - 10) / 10),  # Ajuste para trigo cuando nan_20 > 0
            cultivo == "girasol" ~ req_sistema["girasol"],
            cultivo == "papa" ~ req_sistema["papa"],
            TRUE ~ 0
          ),
          nan == 0 ~ case_when(
            cultivo == "maiz" ~ req_planta["maiz"],
            cultivo == "trigo" ~ req_planta["trigo"] + (req_planta["trigo"] * (proteina_objetivo - 10) / 10),  # Ajuste para trigo cuando nan_20 == 0
            cultivo == "girasol" ~ req_planta["girasol"],
            cultivo == "papa" ~ req_planta["papa"],
            TRUE ~ 0
          ),
          TRUE ~ 0
        ),
        OfertaN = ifelse(
          is.na(N_disponible),  
          NA,  
          coalesce(efecto_antecesor, 0) + coalesce(Nan_total, 0) + N_disponible  
        ),
        DemandaN = rendimiento_objetivo * Requerimiento,
        DosisN = ifelse(
          !is.na(OfertaN),
          DemandaN - OfertaN,
          NA_real_
        )
      )

    datos <- datos %>%
      mutate(
        Nan_total = ifelse(is.na(Nan_total), "**", as.character(Nan_total)),
        N_disponible = ifelse(is.na(N_disponible), "*", as.character(N_disponible)),
        OfertaN = ifelse(is.na(OfertaN), "*", as.character(OfertaN)),
        DosisN = ifelse(is.na(DosisN), "*", as.character(DosisN))
      )
    
    
    # Seleccionar columnas relevantes
    datos_resultado <- datos %>%
      select(lote, cultivo, rendimiento_objetivo, DemandaN, efecto_antecesor, Nan_total, N_disponible, OfertaN, DosisN) %>%
      rename(`Lote` = lote,
             `Cultivo` = cultivo,
             `Rendimiento (tn/ha)` = rendimiento_objetivo,
             `Demanda N (kg N / ha)` = DemandaN,
             `Efecto antecesor` = efecto_antecesor,
             `N mineralizable (kg N / ha)` = Nan_total,
             `Nitrógeno disponible (kg N / ha)` = N_disponible,
             `Oferta N (kg N / ha)` = OfertaN,
             `Dosis N (kg N / ha)` = DosisN)
    
    return(datos_resultado)
  })
  
  # Renderiza la tabla con resultados
  output$tabla_nitrogeno <- renderUI({
    data <- resultados_nitrogeno()
    
    
    # Crea tabla HTML con estilos específicos para cada columnas
    tabla_html <- paste0(
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<thead><tr>",
      "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
      "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
      "<th style='background-color: #06A77D60; padding: 5px;'>Rendimiento<br>(tn / ha)</th>",
      "<th style='background-color: #06A77D60; padding: 5px;'>Demanda<br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>Efecto antecesor<br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>N mineralizable<br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>Nitrógeno disponible <br>(kg N / ha)</th>",
      "<th style='background-color: #FF991460; padding: 5px;'>Oferta<br>(kg N / ha)</th>",
      "<th style='background-color: #C5223360; padding: 5px;'>Dosis<br>(kg N / ha)</th>",
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
    
    # Agregar la aclaración debajo de la tabla
    aclaracion_html1 <- "<p style='color: red;'>* Los valores de N disponible, Oferta y 
    Dosis no fueron calculados debido a la falta de datos de nitratos (0-20, 20-40, o 40-60).</p>"
    
    # Agregar la aclaración debajo de la tabla
    aclaracion_html2 <- "<p style='color: red;'>** Cuando el valor de Nan no está disponible, 
    el modelo considera el valor medio de mineralización de la región.</p>"
    
    HTML(paste0(tabla_html, aclaracion_html1, aclaracion_html2))
  })
  
  output$descarga_N <- downloadHandler(
    filename = function() {
      paste("resultados_N_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      
      library(openxlsx)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Resultados_N")
      
      # Escribir los resultados en la hoja
      writeData(wb, "Resultados_N", resultados_nitrogeno(), startRow = 1, startCol = 1)
      
      aclaracion_1 <- "*Los valores de N disponible, Oferta y Dosis no fueron calculados debido a la falta de datos de nitratos (0-20, 20-40, o 40-60)."
      
      aclaracion_2 <- "**Cuando el valor de Nan no está disponible, el modelo considera el valor medio de mineralización de la región."
      
      # Calcular la fila donde se escribirá el mensaje (después de los datos)
      start_row <- nrow(resultados_nitrogeno()) + 2
      
      # Escribir las aclaraciones dejando una fila de espacio
      writeData(wb, "Resultados_N", aclaracion_1, startRow = start_row + 1, startCol = 1)
      writeData(wb, "Resultados_N", aclaracion_2, startRow = start_row + 3, startCol = 1)
      
      # Guardar el archivo Excel
      saveWorkbook(wb, file, overwrite = TRUE)
      
      
    }
  )
 
  
  grafico_nitrogeno <- function(datos) {
    req("Lote" %in% names(datos), "Cultivo" %in% names(datos))
    
    datos <- datos %>%
      mutate(Titulo = paste("Lote", Lote, "-", toupper(Cultivo)), 
             Lote_num = as.numeric(gsub("\\D", "", Lote)))
    
    datos <- datos %>%
      arrange(Lote_num)
    
    datos <- datos %>%
      mutate(`Dosis N (kg N / ha)` = as.numeric(ifelse(`Dosis N (kg N / ha)` == "*", NA, `Dosis N (kg N / ha)`)))
    
    datos_dosis <- datos %>%
      filter(!is.na(`Dosis N (kg N / ha)`)) %>%
      select(Lote, Cultivo, `Dosis N (kg N / ha)`, Titulo) 
    
    max_dosis <- max(datos_dosis$`Dosis N (kg N / ha)`, na.rm = TRUE)
    
    
    ggplot(datos_dosis,  aes(x = factor(Titulo, levels = unique(Titulo)), y = `Dosis N (kg N / ha)`, fill = "#C52233", color = "#C52233" )) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5), 
               width = 0.2, fill = alpha("#C52233", 0.6), color = "#C52233") +
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
        axis.text.x = element_text(face = "bold", size = 16, angle = 45, hjust = 1), 
        axis.title.x = element_text(face = "bold", size = 16),  
        axis.title.y = element_text(face = "bold", size = 16),  
        legend.position = "none",  
        strip.text = element_text(face = "bold", size = 16)  
      ) +
      scale_y_continuous(limits = c(0, max_dosis + 20))
  }
  
  output$multi_lotes_N <- renderPlot({
    req(resultados_nitrogeno())
    grafico_nitrogeno(resultados_nitrogeno())
  })
  
  output$download_plot_N <- downloadHandler(
    filename = function() {
      paste("dosisN_lotes", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Guardar el gráfico como archivo
      png(file, width = 600, height = 400, res = 60)
      print(
        grafico_nitrogeno(resultados_nitrogeno()) +
          labs(title = "Dosis recomendada de nitrógeno para cada lote",
               caption = Sys.Date()
          ) +  
          theme(plot.title = element_text(face = "bold", size = 14, hjust = 0, vjust = 1.1),
                plot.caption = element_text(size = 12, hjust = 1, face = "italic"),
                plot.title.position = "plot")  
      )
      dev.off()
    }
  )
  
  ############################ Fosforo #########################################
  
  dosis_suficiencia <- reactive({
    req(input$cultivoP, input$P_Bray_actual)
    
    cultivo_actual <- if (input$cultivoP == "doble_cultivo") {
      input$cultivoP_1
    } else {
      input$cultivoP
    }
    
    filtro <- dosis_data %>% filter(cultivoP == cultivo_actual)
    
    filtro_rango <- filtro %>% filter(input$P_Bray_actual >= P_min & input$P_Bray_actual < P_max)
    
    if (nrow(filtro_rango) > 0) {
      if (cultivo_actual == "papa") {
        
        dosis_recomendada <- filtro_rango$min_dosis  
        return(list(dosis = dosis_recomendada))
      } else {
        dosis_min <- min(filtro_rango$min_dosis, na.rm = TRUE)
        dosis_max <- max(filtro_rango$max_dosis, na.rm = TRUE)
        return(list(min = dosis_min, max = dosis_max))
      }
    } else {
      
      if (cultivo_actual == "papa") {
        return(list(dosis = NA))
      } else {
        return(list(min = NA, max = NA))
      }
    }
  })
  
  output$dosis_suficiencia <- renderUI({
    dosis_vals <- dosis_suficiencia()
    
    cultivo_actual <- if (input$cultivoP == "doble_cultivo") {
      input$cultivoP_1
    } else {
      input$cultivoP
    }
    
    if (cultivo_actual == "papa") {
      div(
        class = "value-box",
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #588157; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 5px;",
        div(
          style = "font-size: 20px; font-weight: bold; margin-bottom: 5px; text-align: center;",
          HTML("<strong>Dosis de suficiencia<br>(kg P / ha):</strong>")
        ),
        div(
          style = "display: flex; justify-content: space-between; width: 40%; align-items: center;",
          div(
            style = "display: flex; flex-direction: column; align-items: flex-start;",
            
            div(
              style = "font-size: 25px; font-weight: bold; margin-top: 10px;",
              if (!is.na(dosis_vals$dosis)) {
                paste(round(dosis_vals$dosis, 0))
              } else {
                "No hay dosis disponible"
              }
            )
          ),
          div(
            class = "icon-container",
            style = "font-size: 40px;",
            icon("droplet")
          )
        )
      )
    } else {
      div(
        class = "value-box",
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #588157; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 5px;",
        div(
          style = "font-size: 20px; font-weight: bold; margin-bottom: 5px; text-align: center;",
          HTML(paste("<strong>Dosis de suficiencia (kg P / ha):</strong> "
          ))),
        
        div(
          style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",
          
          div(
            style = "display: flex; flex-direction: column; align-items: flex-start;",
            div(
              style = "font-size: 25px; font-weight: bold;",
              paste(dosis_vals$min, "-", dosis_vals$max)
            )
          ),
          div(
            class = "icon-container",
            style = "font-size: 40px;",
            icon("droplet")
          )
        )
      )
    }
  })
  
    
 
  
  observe({
    
    cultivo_actual <- if (input$cultivoP == "doble_cultivo") {
      req(input$cultivoP_1)  # Asegurarse de que cultivoP_1 tenga valor
      input$cultivoP_1
    } else {
      input$cultivoP
    }
    
    if (cultivo_actual == "maiz") {
      updateNumericInput(session, "NivelP", value = 20)
      updateNumericInput(session, "factor_construccion", value = 3)
      updateNumericInput(session, "factor_mantenimiento", value = 2.6)
      
    } else if (cultivo_actual == "soja") {
      updateNumericInput(session, "NivelP", value = 20)
      updateNumericInput(session, "factor_construccion", value = 3)
      updateNumericInput(session, "factor_mantenimiento", value = 4.5)
      
    } else if (cultivo_actual == "trigo") {
      updateNumericInput(session, "NivelP", value = 20)
      updateNumericInput(session, "factor_construccion", value = 3)
      updateNumericInput(session, "factor_mantenimiento", value = 3.2)
      
    } else if (cultivo_actual == "girasol") {
      updateNumericInput(session, "NivelP", value = 20)
      updateNumericInput(session, "factor_construccion", value = 3)
      updateNumericInput(session, "factor_mantenimiento", value = 6)
      
    } else if (cultivo_actual == "papa") {
      updateNumericInput(session, "NivelP", value = 30)
      updateNumericInput(session, "factor_construccion", value = 4)
      updateNumericInput(session, "factor_mantenimiento", value = 0.45)}
  })
  
  
  construir_P <- reactive({
    
    cultivo_actual <- if (input$cultivoP == "doble_cultivo") {
      req(input$cultivoP_1)  
      input$cultivoP_1
    } else {
      input$cultivoP
    }
    
    
    req(cultivo_actual, input$NivelP, input$factor_construccion)
    
    max(0, (input$NivelP - input$P_Bray_actual)) * input$factor_construccion
  })
  
  output$construir_P <- renderUI({
    HTML(paste("<strong>Construcción:</strong>", round(construir_P(), 0), "kg P / ha"))
  })
 
  
  mantener_P <- reactive({
    req(input$rendimiento_P, input$factor_mantenimiento)  

    input$rendimiento_P * input$factor_mantenimiento
  })
  
  output$mantener_P <- renderUI({
    HTML(paste("<strong>Mantenimiento:</strong>", round(mantener_P(), 0), "kg P / ha"))
  })
 
  
  # cultivo de 2º
  observeEvent(input$cultivoP_2, {
    if (input$cultivoP_2 == "maiz") {
      updateNumericInput(session, "factor_mantenimiento_2", value = 2.6)
      
    } else if (input$cultivoP_2 == "soja") {
      updateNumericInput(session, "factor_mantenimiento_2", value = 4.5)
      
    } else if (input$cultivoP_2 == "trigo") {
      updateNumericInput(session, "factor_mantenimiento_2", value = 3.2)
      
    } else if (input$cultivoP_2 == "girasol") {
      updateNumericInput(session, "factor_mantenimiento_2", value = 6)
      
    } else if (input$cultivoP_2 == "papa") {
      updateNumericInput(session, "factor_mantenimiento_2", value = 0.45)
    }
  })
  
  mantener_P_2 <- reactive({
    req(input$cultivoP_2, input$rendimiento_P_2, input$factor_mantenimiento_2)  
    
    input$rendimiento_P_2 * input$factor_mantenimiento_2
  })
  
  output$mantener_P_2 <- renderUI({
    HTML(paste("<strong>Mantenimiento:</strong>", round(mantener_P_2(), 0), "kg P / ha"))
  })
  
  dosisCyM <- reactive({
    req(construir_P(), mantener_P()) 
    
    if (input$cultivoP == "doble_cultivo") {
      req(mantener_P_2(), input$cultivoP_2)

      dosisCyM_valor <- construir_P() + mantener_P() + mantener_P_2()
    } else {

      dosisCyM_valor <- construir_P() + mantener_P()
    }
    
    return(dosisCyM_valor)
    })
  
  
  output$dosisCyM <- renderUI({
    dosis_valor <- dosisCyM()  
    
    div(
      class = "value-box",
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #BC6C25; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
      
      # Título
      div(
        style = "font-size: 20px; font-weight: bold; margin-bottom: 6px; text-align: center;",
        HTML("<strong>Dosis de construcción y mantenimiento<br>(kg P / ha):</strong>")
      ),
      div(
        style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",

        div(
          style = "font-size: 30px; font-weight: bold;",
          round(dosis_valor, 0)
        ),
        div(
          class = "icon-container",
          style = "font-size: 40px;",
          icon("gears")
        )
      )
    )
  })
  
  
  ### Multi lotes
  
  resultados_fosforo <- reactive({
    req(data_usuario())      
    datos <- data_usuario()    
    
    datos <- datos %>%
      mutate(
        cultivo = trimws(tolower(cultivo)), 
        rendimiento_objetivo = as.numeric(rendimiento_objetivo),
        cultivo_segunda = trimws(tolower(cultivo_segunda)), 
        rendimiento_objetivo_cultivo_segunda = as.numeric(rendimiento_objetivo_cultivo_segunda),
        p_bray_actual = as.numeric(p_bray_actual),
        nivelp_objetivo = as.numeric(nivelp_objetivo),
        nutriente_en_grano_p = as.numeric(nutriente_en_grano_p)
      )
    
    factores_mantener <- c("soja" = 4.5, "trigo" = 3.2, "maiz" = 2.6, "girasol" = 6, "papa" = 0.45)
    factores_construir <- c("soja" = 3, "trigo" = 3, "maiz" = 3, "girasol" = 3, "papa" = 4)
    niveles_p <- c("soja" = 20, "trigo" = 20, "maiz" = 20, "girasol" = 20, "papa" = 30)
    
    datos <- datos %>%
      mutate(
        factor_mantener = ifelse(nutriente_en_grano_p == 0, factores_mantener[cultivo], nutriente_en_grano_p),
        nivel_p = ifelse(nivelp_objetivo == 0, niveles_p[cultivo], nivelp_objetivo)
      )
    
    
    
    datos <- datos %>%
      mutate(
        filtro_rango = map2(cultivo, p_bray_actual, ~ {
          if (!is.na(.y)) {
            result <- dosis_data %>% filter(cultivoP == .x, .y >= P_min, .y < P_max)
            result
          } else {
            NULL
          }
        }),
        dosis_suficiencia_min = if ("p_bray_actual" %in% colnames(datos)) {
          map_chr(filtro_rango, ~ {
            if (!is.null(.x) && nrow(.x) > 0) {
              as.character(min(.x$min_dosis, na.rm = TRUE))  # Convertir a carácter
            } else {
              "-"
            }
          })
        } else {
          "-"
        },
        dosis_suficiencia_max = if ("p_bray_actual" %in% colnames(datos)) {
          map_chr(filtro_rango, ~ {
            if (!is.null(.x) && nrow(.x) > 0) {
              as.character(max(.x$max_dosis, na.rm = TRUE))  # Convertir a carácter
            } else {
              "-"
            }
          })
        } else {
          "-"
        },
        
        construir_P = if ("p_bray_actual" %in% colnames(datos)) {
          if_else(
            !is.na(p_bray_actual),
            as.character(round(pmax(0, (nivel_p - p_bray_actual) * factores_construir[cultivo]), 0)),  # Convertir a carácter
            "-"
          )
        } else {
          "-"
        },
        
        mantener_P = round(rendimiento_objetivo * factor_mantener, 0),
        
        mantener_P_segunda = ifelse(
          !is.na(cultivo_segunda) & cultivo_segunda != "",
          round(rendimiento_objetivo_cultivo_segunda * factores_mantener[cultivo_segunda], 0),
          0
        ),
        mantener_P_total = mantener_P + mantener_P_segunda,
        
        dosisCyM = if_else(
          construir_P == "-",
          as.character(mantener_P_total),  # Convertir mantener_P_total a carácter
          as.character(mantener_P_total + as.numeric(construir_P))  # Si construir_P tiene dato numérico, sumar
        )
      ) %>%
      ungroup()
    
    
    
    # Seleccionar columnas relevantes
    datos_resultado <- datos %>%
      mutate(
        `Dosis de suficiencia (kg P / ha)` = paste(dosis_suficiencia_min, "-", dosis_suficiencia_max)
      ) %>%
      select(
        lote, cultivo, cultivo_segunda, rendimiento_objetivo, `Dosis de suficiencia (kg P / ha)`,  
        construir_P,  mantener_P_total, dosisCyM
      ) %>%
      rename(Lote = lote,
             Cultivo = cultivo,
             `Cultivo de segunda` = cultivo_segunda,
             `Rendimiento (tn/ha)` = rendimiento_objetivo,
             `Dosis de construcción (kg P / ha)` = construir_P,
             `Dosis de mantenimiento (kg P / ha)` =  mantener_P_total,
             `Dosis de construcción y mantenimiento (kg P / ha)` = dosisCyM
      )
    return(datos_resultado)
    
  })
  
    
    # Renderiza la tabla con resultados
    output$tabla_fosforo <- renderUI({
      data <- resultados_fosforo()
      
      # Crea tabla HTML con estilos específicos para cada columnas
      tabla_html <- paste0(
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<thead><tr>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo antecesor</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Rendimiento<br>(tn / ha)</th>",
        "<th style='background-color: #58815760; padding: 5px;'>Dosis de suficiencia<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de construcción<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de mantenimiento<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de construcción y mantenimiento <br>(kg P / ha)</th>",
        
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
    
    output$descarga_P <- downloadHandler(
      filename = function() {
        paste("resultados_P_", Sys.Date(), ".xlsx", sep = "")  
      },
      content = function(file) {
        write_xlsx(resultados_fosforo(), file)  
      }
    )

    ############################ Azufre #########################################
    
    observeEvent(input$cultivoS, {  
      req(input$cultivoS)  
      
      if (input$cultivoS == "maiz") {
        updateNumericInput(session, "factor_s", value = 1.2)
        
      } else if (input$cultivoS == "soja") {
        updateNumericInput(session, "factor_s", value = 3.5)
        
      } else if (input$cultivoS == "trigo") {
        updateNumericInput(session, "factor_s", value = 1.5)
        
      } else if (input$cultivoS == "girasol") {
        updateNumericInput(session, "factor_s", value = 2.5)
        
      } else if (input$cultivoS == "papa") {
        updateNumericInput(session, "factor_s", value = 1.5)
      }
    })
    
    azufre_disp <- reactive({
      densidad_aparente <- ifelse(!is.null(input$dens_ap_s) && input$dens_ap_s != 0, input$dens_ap_s, 1.2)
      
      # Verificar si azufre_20 es válido
      if (is.null(input$azufre_20) || is.na(input$azufre_20) || input$azufre_20 <= 0) {
        return("Debe ingresar un valor para S-sulfato a 0-20")
      }
      
      # Usar valores predeterminados para azufre_40 y azufre_60 si no se proporcionan
      azufre_40 <- ifelse(!is.null(input$azufre_40) && !is.na(input$azufre_40), input$azufre_40, 0)
      azufre_60 <- ifelse(!is.null(input$azufre_60) && !is.na(input$azufre_60), input$azufre_60, 0)
      
      # Calcular azufre disponible
      (input$azufre_20 + azufre_40 + azufre_60) * (densidad_aparente * 2)
    })
    
    output$azufre_disp <- renderUI({
      resultado <- azufre_disp()
      
      # Mostrar mensaje si el resultado no es numérico (es un mensaje de error)
      if (!is.numeric(resultado)) {
        div(style = "color: red; font-weight: bold;", resultado)
      } else {
        # Mostrar el resultado redondeado si es un número
        div(style = "color: black; font-weight: bold;", round(resultado, 0))
      }
    })
    
    
    dosis_s <- reactive({
      req(input$cultivoS, input$rendimiento_s, input$factor_s)  
      
      input$rendimiento_s * input$factor_s
    })
    
    output$dosis_s <- renderUI({
      # Verificar si se ingresaron valores para azufre_20 y nan_s
      if (is.null(input$azufre_20) || is.na(input$azufre_20) || input$azufre_20 <= 0) {
        return(div(style = "color: red; font-weight: bold;", "Debe ingresar el valor de S-sulfato a 0-20cm"))
      }
      
      if (is.null(input$nan_s) || is.na(input$nan_s) || input$nan_s <= 0) {
        return(div(style = "color: red; font-weight: bold;", "Debe ingresar el valor de Nan"))
      }
      
      # Manejo de valores predeterminados para otros inputs
      azufre_20 <- input$azufre_20
      azufre_40 <- ifelse(!is.null(input$azufre_40) && !is.na(input$azufre_40), input$azufre_40, 0)
      azufre_60 <- ifelse(!is.null(input$azufre_60) && !is.na(input$azufre_60), input$azufre_60, 0)
      dens_ap_s <- ifelse(!is.null(input$dens_ap_s) && !is.na(input$dens_ap_s), input$dens_ap_s, 1.2) # Valor predeterminado de densidad aparente
      
      # Cálculos de condiciones
      suma_sulfato <- (azufre_20 + azufre_40 + azufre_60) * dens_ap_s * 2
      condicion1 <- azufre_20 < 10 
      condicion2 <- suma_sulfato < 45 
      condicion3 <- (input$zona_s == "Sudeste de Bs.As" && input$nan_s < 65) || 
        (input$zona_s == "Otras zonas" && input$nan_s < 40)
      
      if (condicion1 && condicion2 && condicion3) {
        dosis_valor <- dosis_s()
        div(
          class = "value-box",
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #DE9E36; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
          div(
            style = "font-size: 20px; font-weight: bold; margin-bottom: 6px; text-align: center;",
            HTML("<strong>Dosis de azufre<br>(kg S / ha):</strong>")
          ),
          div(
            style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",
            div(style = "font-size: 30px; font-weight: bold;", round(dosis_valor, 0)),
            div(class = "icon-container", style = "font-size: 40px;", icon("droplet"))
          )
        )
      } else {
        div(
          class = "value-box",
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #DEB841; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
          div(
            style = "font-size: 30px; font-weight: bold; margin-bottom: 6px; text-align: center;",
            HTML("<strong>No se recomienda fertilizar con azufre</strong>")
          )
        )
      }
    })
    
    ### Multi lotes
    
    output$zona_multi_s <- renderUI({
      req(data_usuario())
      selectInput("zona_s", 
                  label = strong("Seleccione la zona geográfica para calcular la dosis de S"),
                  choices = c("Sudeste de Bs.As.", 
                              "Otra"),
                  selected = "Sudeste de Bs.As.")
    })
    
    resultados_azufre <- reactive({
      req(data_usuario())      
      datos <- data_usuario()    
      
      datos <- datos %>%
        mutate(
          cultivo = trimws(tolower(cultivo)), 
          rendimiento_objetivo = as.numeric(rendimiento_objetivo),
          nutriente_en_grano_s = as.numeric(nutriente_en_grano_s),
          s_sulfato_20 = as.numeric(s_sulfato_20), 
          s_sulfato_40 = as.numeric(s_sulfato_40),
          s_sulfato_60 = as.numeric(s_sulfato_60),
          nan = as.numeric(nan),
          densidad_aparente = as.numeric(densidad_aparente)
        )
      
      factores_s <- c("soja" = 3.5, "trigo" = 1.5, "maiz" = 1.2, "girasol" = 2.5, "papa" = 1.5)
      
      
      datos <- datos %>%
        mutate(
          factor_s = ifelse(nutriente_en_grano_s == 0, factores_s[cultivo], nutriente_en_grano_s),
          
          datos_completos = !is.na(s_sulfato_20) & !is.na(nan) & nan > 0,
          
          suma_sulfato = if_else(
            datos_completos,
            (s_sulfato_20 + s_sulfato_40 + s_sulfato_60) * densidad_aparente * 2,
            NA_real_
          ),
          
          condiciones_cumplidas = case_when(
            datos_completos & input$zona_s == "Sudeste de Bs.As." & 
              (s_sulfato_20 < 10) & 
              (suma_sulfato < 45) & 
              (nan < 65) ~ TRUE,
            
            datos_completos & input$zona_s == "Otra" & 
              (s_sulfato_20 < 10) & 
              (suma_sulfato < 45) & 
              (nan < 40) ~ TRUE,
            
            TRUE ~ FALSE
          ),
          
          dosis_s = case_when(
            !datos_completos ~ "Debe ingresar el dato de nan y sulfato a 0-20cm",
            condiciones_cumplidas ~ as.character(round(rendimiento_objetivo * factor_s, 0)),
            TRUE ~ "No se recomienda fertilizar con azufre"
          )
          
        ) %>%
        ungroup()
      
      datos_resultado <- datos %>%
        select(
          lote, cultivo, rendimiento_objetivo, dosis_s
        ) %>%
        rename(`Lote` = lote,
               `Cultivo` = cultivo,
               `Rendimiento (tn/ha)` = rendimiento_objetivo,
               `Dosis S (kg S / ha)` = dosis_s
        )
      return(datos_resultado)
      
    })
    
    output$tabla_azufre <- renderUI({
      data <- resultados_azufre()
      
      tabla_html <- paste0(
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<thead><tr>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Rendimiento<br>(tn / ha)</th>",
        "<th style='background-color: #DEB84160; padding: 5px;'>Dosis S <br>(kg S / ha)</th>",
        
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
    
    output$descarga_S <- downloadHandler(
      filename = function() {
        paste("resultados_S_", Sys.Date(), ".xlsx", sep = "")  
      },
      content = function(file) {
        write_xlsx(resultados_azufre(), file)  
      }
    )
    
    
    
    
    ############################ Zinc #########################################
  
    observeEvent(input$cultivoZ, {  
      req(input$cultivoZ)  
      
      if (input$cultivoZ == "maiz") {
        updateNumericInput(session, "factor_z", value = 50)
        
      } else if (input$cultivoZ == "soja") {
        updateNumericInput(session, "factor_z", value = 40)
        
      } else if (input$cultivoZ == "trigo") {
        updateNumericInput(session, "factor_z", value = 30)
        
      } else if (input$cultivoZ == "girasol") {
        updateNumericInput(session, "factor_z", value = 50)
        
      } else if (input$cultivoZ == "papa") {
        updateNumericInput(session, "factor_z", value = 6.5)
      }
    })
    
    
    
    dosis_z <- reactive({
      req(input$cultivoZ, input$rendimiento_z, input$factor_z)  
      
      input$rendimiento_z * input$factor_z
    })
    
    output$dosis_z <- renderUI({
      req(input$zinc)
      
      limite_superior <- if (tolower(input$cultivoZ) == "papa") 2 else 1.2

      dosis_valor <- if (input$zinc >= 0 && input$zinc < 0.5) {
        dosis_z() * 1.3
      } else if (input$zinc >= 0.5 && input$zinc < limite_superior) {
        dosis_z()
      } else {
        NULL
      }
      
      # Renderizamos la UI dependiendo de las condiciones
      if (!is.null(dosis_valor)) {
        div(
          class = "value-box",
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #778DA9; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
          div(
            style = "font-size: 20px; font-weight: bold; margin-bottom: 6px; text-align: center;",
            HTML("<strong>Dosis de zinc<br>(g Zn / ha):</strong>")
          ),
          div(
            style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",
            div(
              style = "font-size: 30px; font-weight: bold;",
              round(dosis_valor, 0)
            ),
            div(
              class = "icon-container",
              style = "font-size: 40px;",
              icon("droplet")
            )
          )
        )
      } else {
        div(
          class = "value-box",
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #415A77; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
          div(
            style = "font-size: 30px; font-weight: bold; margin-bottom: 6px; text-align: center;",
            HTML("<strong>No se recomienda fertilizar con zinc</strong>")
          )
        )
      }
    })
    
    ### Multi lotes
    
    resultados_zinc <- reactive({
      req(data_usuario())      
      datos <- data_usuario()    
      
      datos <- datos %>%
        mutate(
          cultivo = trimws(tolower(cultivo)), 
          rendimiento_objetivo = as.numeric(rendimiento_objetivo),
          nutriente_en_grano_z = as.numeric(nutriente_en_grano_z),
          zn_dtpa = as.numeric(zn_dtpa)
        )
      
      factores_z <- c("soja" = 40, "trigo" = 30, "maiz" = 50, "girasol" = 50, "papa" = 6.5)
      
      
      datos <- datos %>%
        mutate(
          factor_z = ifelse(nutriente_en_grano_z == 0, factores_z[cultivo], nutriente_en_grano_z)
        )
      
      datos <- datos %>%
        mutate(
          dosis_z = case_when(
            cultivo == "papa" & zn_dtpa >= 0 & zn_dtpa < 0.5 ~ as.character(round(rendimiento_objetivo * factor_z * 1.3, 0)),
            cultivo == "papa" & zn_dtpa >= 0.5 & zn_dtpa < 2 ~ as.character(round(rendimiento_objetivo * factor_z, 0)),
            cultivo == "papa" & zn_dtpa >= 2 ~ "No se recomienda fertilizar con zinc",
            zn_dtpa >= 0 & zn_dtpa < 0.5 ~ as.character(round(rendimiento_objetivo * factor_z * 1.3, 0)),
            zn_dtpa >= 0.5 & zn_dtpa < 1.2 ~ as.character(round(rendimiento_objetivo * factor_z, 0)),
            zn_dtpa >= 1.2 ~ "No se recomienda fertilizar con zinc"
          )
        ) %>%
        ungroup()
      
      datos <- datos %>%
        mutate(
          dosis_z = ifelse(is.na(dosis_z), "-", as.character(dosis_z))
          )
      
      datos_resultado <- datos %>%
        select(
          lote, cultivo, rendimiento_objetivo, dosis_z
        ) %>%
        rename(`Lote` = lote,
               `Cultivo` = cultivo,
               `Rendimiento (tn/ha)` = rendimiento_objetivo,
               `Dosis Zn (g Zn / ha)` = dosis_z
        )
      return(datos_resultado)
      
    })
    
    output$tabla_zinc <- renderUI({
      data <- resultados_zinc()
      
      tabla_html <- paste0(
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<thead><tr>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Rendimiento<br>(tn / ha)</th>",
        "<th style='background-color: #415A7760; padding: 5px;'>Dosis Zn <br>(g Zn / ha)</th>",
        
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
    
    output$descarga_Z <- downloadHandler(
      filename = function() {
        paste("resultados_Zn_", Sys.Date(), ".xlsx", sep = "")  
      },
      content = function(file) {
        write_xlsx(resultados_zinc(), file)  
      }
    )
    ######################## RECOMENDACIONES ############################
    
    resultados_total <- reactive({

      nitrogeno <- resultados_nitrogeno()
      fosforo <- resultados_fosforo()
      azufre <- resultados_azufre()
      zinc <- resultados_zinc()
      
      # Verifica si los datos existen
      if (is.null(nitrogeno) | is.null(fosforo) | is.null(azufre) | is.null(zinc)) {
        return(NULL)  
      }
      
      nitrogeno_seleccionada <- nitrogeno[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis N (kg N / ha)")]
      fosforo_seleccionada <- fosforo[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis de suficiencia (kg P / ha)", "Dosis de construcción y mantenimiento (kg P / ha)")]
      azufre_seleccionada <- azufre[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis S (kg S / ha)")]
      zinc_seleccionada <- zinc[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis Zn (g Zn / ha)")]

      
      fosforo_seleccionada <- fosforo_seleccionada[, !colnames(fosforo_seleccionada) %in% c("Cultivo", "Rendimiento (tn/ha)")]
      azufre_seleccionada <- azufre_seleccionada[, !colnames(azufre_seleccionada) %in% c("Cultivo", "Rendimiento (tn/ha)")]
      zinc_seleccionada <- zinc_seleccionada[, !colnames(zinc_seleccionada) %in% c("Cultivo", "Rendimiento (tn/ha)")]

      tabla_general <- nitrogeno_seleccionada %>%
        left_join(fosforo_seleccionada, by = "Lote") %>%
        left_join(azufre_seleccionada, by = "Lote") %>%
        left_join(zinc_seleccionada, by = "Lote")

      return(tabla_general)

    })

    output$tabla_total <- renderUI({
      data <- resultados_total()

      if (is.null(data)) {
        return(tags$div(
          style = "color: red; font-weight: bold;",
          "Por favor ingrese los datos en Carga de datos"
        ))
      }
      
      tabla_html <- paste0(
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<thead><tr>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Rendimiento<br>(tn / ha)</th>",
        "<th style='background-color: #C5223360; padding: 5px;'>Dosis de nitrogeno<br>(kg N / ha)</th>",
        "<th style='background-color: #58815760; padding: 5px;'>Dosis de suficiencia<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de construccion y mantenimiento<br>(kg P / ha)</th>",
        "<th style='background-color: #DEB84160; padding: 5px;'>Dosis de azufre <br>(kg S / ha)</th>",
        "<th style='background-color: #415A7760; padding: 5px;'>Dosis de zinc <br>(g Zn / ha)</th>",

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

    output$descarga_total <- downloadHandler(
      filename = function() {
        paste("resultados_total_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        
        library(openxlsx)
        
        wb <- createWorkbook()
        addWorksheet(wb, "resultados_total")
        
        # Escribir los resultados en la hoja
        writeData(wb, "resultados_total", resultados_total(), startRow = 1, startCol = 1)
        
        aclaracion_1 <- "*Los valores de N disponible, Oferta y Dosis no fueron calculados debido a la falta de datos de nitratos (0-20, 20-40, o 40-60)."
        
        # Calcular la fila donde se escribirá el mensaje (después de los datos)
        start_row <- nrow(resultados_total()) + 2
        
        # Escribir las aclaraciones dejando una fila de espacio
        writeData(wb, "resultados_total", aclaracion_1, startRow = start_row + 1, startCol = 1)
        
        # Guardar el archivo Excel
        saveWorkbook(wb, file, overwrite = TRUE)
        
      }
    )
    
    ################## MONITOREO NITRÓGENO #####################################
    #### Lote único
    
    indice_suf_nitrogeno <- reactive({
      req(input$índice_monitoreo, input$IFR_monitoreo)  
      
      input$índice_monitoreo / input$IFR_monitoreo
    })
    
    output$indice_suf_nitrogeno <- renderUI({
      req(indice_suf_nitrogeno())  # Asegúrate de que indice_suf_nitrogeno() tenga un valor válido
      
      div(
        class = "value-box",
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #A3B18A; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
        div(
          style = "font-size: 20px; font-weight: bold; margin-bottom: 6px; text-align: center;",
          HTML("<strong>Índice de suficiencia de nitrógeno:</strong>")
        ),
        div(
          style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",
          div(
            style = "font-size: 30px; font-weight: bold;",
            round(indice_suf_nitrogeno(), 2)  
          ),
          div(
            class = "icon-container",
            style = "font-size: 40px;",
            icon("hourglass-3")
          )
        )
      )
    })
    
    dosis_monitoreo <- reactive({
      req(input$cultivo_monitoreo, indice_suf_nitrogeno())  
      
      dosis <- if (input$cultivo_monitoreo == "maiz") {
        324 - 329 * (indice_suf_nitrogeno()^3.12)
      } else if (input$cultivo_monitoreo == "trigo") {
        509 - 657 * (indice_suf_nitrogeno()^3.52)
      } else if (input$cultivo_monitoreo == "papa") {
        336 - 346 * (indice_suf_nitrogeno()^3.36)
      } else {
        NA
      }
      
      # Asegurar que la dosis no sea negativa
      max(0, dosis)
    })
    
    output$dosis_monitoreo <- renderUI({
      req(dosis_monitoreo())  # Asegúrate de que dosis_monitoreo() tenga un valor válido
      
      div(
        class = "value-box",
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #588157; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",
        div(
          style = "font-size: 20px; font-weight: bold; margin-bottom: 6px; text-align: center;",
          HTML("<strong>Dosis óptima económica<br>(kg N / ha):</strong>")
        ),
        div(
          style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",
          div(
            style = "font-size: 30px; font-weight: bold;",
            round(dosis_monitoreo(), 0)  # Llamada correcta a la función reactiva
          ),
          div(
            class = "icon-container",
            style = "font-size: 40px;",
            icon("droplet")
          )
        )
      )
    })
    
    output$grafico_monitoreo <- renderPlot({
      req(input$cultivo_monitoreo)  # Asegúrate de que el usuario seleccionó un cultivo
      
      # Define el rango de ISN
      ISN <- seq(0.5, 1, by = 0.01)
      
      # Calcula la dosis de nitrógeno según el cultivo
      dosis <- if (input$cultivo_monitoreo == "maiz") {
        324 - 329 * (ISN^3.12)
      } else if (input$cultivo_monitoreo == "trigo") {
        509 - 657 * (ISN^3.52)
      } else if (input$cultivo_monitoreo == "papa") {
        336 - 346 * (ISN^3.36)
      } else {
        rep(NA, length(ISN))  # Si no hay cultivo, devuelve NA
      }
      
      # Crear un data frame para el gráfico
      datos <- data.frame(ISN = ISN, Dosis = dosis)
      
      ISN_usuario <- indice_suf_nitrogeno()
      Dosis_usuario <- dosis_monitoreo()
      
      titulo <- if (input$cultivo_monitoreo == "maiz") {
        "Relación entre ISN y DOE para maíz en V10"
      } else if (input$cultivo_monitoreo == "trigo") {
        "Relación entre ISN y DOE para trigo en Z31, un nudo"
      } else if (input$cultivo_monitoreo == "papa") {
        "Relación entre ISN y DOE para papa en llenado de tubérculos"
      } else {
        paste("Relación entre ISN y DOE en", input$cultivo_monitoreo)  
      }

      ggplot(datos, aes(x = ISN, y = Dosis)) +
        geom_point(color = "#4C72B0", size = 3, alpha = 0.6) +  
        geom_line(color = "#DD8452", size = 1.0) +             
        geom_point(aes(x = ISN_usuario, y = Dosis_usuario), 
                   color = "#F28E2C", size = 6, shape = 21, 
                   stroke = 1.5) +
        geom_segment(aes(x = ISN_usuario, xend = ISN_usuario, 
                         y = 0, yend = Dosis_usuario),
                     linetype = "dashed", color = "#F28E2C", size = 0.8, alpha = 0.6) +
        geom_segment(aes(x = 0.5, xend = ISN_usuario, 
                         y = Dosis_usuario, yend = Dosis_usuario),
                     linetype = "dashed", color = "#F28E2C", size = 0.8, alpha = 0.6) +
        scale_x_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.05)) +
        scale_y_continuous(limits = c(0, 300),
                           labels = scales::comma,
                           breaks = seq(0, 300, by = 50)) +  
        labs(
          title = titulo,
          x = "Índice de Suficiencia de Nitrógeno (ISN)",
          y = "Dosis óptima económica (DOE, kg N / ha)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(
            face = "bold", size = 18, hjust = 0.5, color = "#333333"
          ),
          axis.title = element_text(face = "bold", size = 16, color = "#555555"),
          axis.text = element_text(face = "bold", size = 14, color = "#333333"),
          panel.grid.major = element_line(color = "#D9D9D9"),
          panel.grid.minor = element_blank()
        )
    })
          
    #### Múltiples lotes
    
    output$tabla_monitoreo <- downloadHandler(
      filename = function() {
        "data_monitoreo.xlsx"
      },
      content = function(file) {
        # Crear un dataframe modelo
        datos <- data.frame(
          Lote = c(1, 2), 
          Cultivo = c("maiz", "trigo"),
          Estadio = c(NA, NA), 
          Indice_vegetacion = c(NA, NA),
          Indice_franja_referencia = c(NA, NA)
        )
        
        
        
        # Renombrar las columnas con las unidades correspondientes
        column_units <- c(
          Lote = "Lote",
          Cultivo = "Cultivo",
          Estadio = "Estadio",
          Indice_vegetacion = "Índice de vegetación",
          Indice_franja_referencia = "Índice de franja de referencia"
        )
        
        # Aplicar los nuevos nombres al data.frame
        colnames(datos) <- column_units[colnames(datos)]
        
        
        
        library(openxlsx)
        
        wb <- createWorkbook()
        
        
        addWorksheet(wb, "Datos")
        
        writeData(wb, "Datos", datos)
        
        
        estilo_general1 <- createStyle(fgFill = "gray90",
                                       textDecoration = "bold",
                                       border = "Bottom",
                                       borderColour = "black",
                                       borderStyle = "thin",
                                       wrapText = TRUE )
        
        
        addStyle(wb, "Datos", style = estilo_general1, rows = 1, cols = c(1:5), gridExpand = TRUE)
        

        
        saveWorkbook(wb, file, overwrite = TRUE)
        
        
      }
    )
    
    
    data_monitoreo <- reactive({
      if (is.null(input$archivo_monitoreo)) {
        return(NULL)  
      }
      
      ext <- tools::file_ext(input$archivo_monitoreo$name)
      
      
      if (ext == "csv") {
        data <- read.csv(input$archivo_monitoreo$datapath)
      } else if (ext == "xlsx") {
        data <- readxl::read_xlsx(input$archivo_monitoreo$datapath, sheet = "Datos")
      } else {
        showNotification("Formato de archivo no soportado.", type = "error")
        return(NULL)
      }
      
      
      # Verificar si el archivo tiene las columnas requeridas
      required_columns <- c("Lote", "Cultivo", "Estadio", "Índice de vegetación", "Índice de franja de referencia")
      missing_columns <- setdiff(required_columns, colnames(data))
      
      if (length(missing_columns) > 0) {
        showNotification(
          paste("El archivo no tiene las columnas requeridas:", 
                paste(missing_columns, collapse = ", ")), 
          type = "error"
        )
        return(NULL) 
      }
      
      # Renombrar las columnas con las unidades correspondientes
      column_original <- c(
        Lote = "Lote",
        Cultivo = "Cultivo",
        `Estadio` = "Estadio" ,
        `Índice de vegetación` = "Indice_vegetación",
        `Índice de franja de referencia` = "Indice_franja_referencia"
      )
      
      # Aplicar los nuevos nombres al data.frame
      colnames(data) <- column_original[colnames(data)]
      
      colnames(data) <- tolower(colnames(data))
      data$cultivo <- tolower(data$cultivo)
      data$estadio <- tolower(data$estadio)
      
      # Confirmar al usuario que el archivo se ha procesado correctamente
      showNotification("Archivo subido correctamente.", type = "message")
      
      
      return(data)
    })

    
    
    resultados_monitoreo <- reactive({
      req(data_monitoreo())      
      datos <- data_monitoreo()    
      
      
      datos <- datos %>%
        mutate(
          indice_suf_nitrogeno = round(`indice_vegetación` / `indice_franja_referencia`, 2)
        ) %>%
        mutate(
          dosis_monitoreo = case_when(
            cultivo == "maiz" ~ round(324 - 329 * (indice_suf_nitrogeno^3.12), 0),
            cultivo == "trigo" ~ round(509 - 657 * (indice_suf_nitrogeno^3.52), 0),
            cultivo == "papa" ~ round(336 - 346 * (indice_suf_nitrogeno^3.36), 0),
            TRUE ~ NA_real_ # Manejar casos donde el cultivo no coincide
          )
        )
      
      datos_resultado <- datos %>%
        select(
          lote, cultivo, indice_suf_nitrogeno, dosis_monitoreo
        ) %>%
        rename(`Lote` = lote,
               `Cultivo` = cultivo,
               `Índice de suficiencia de nitrógeno` = indice_suf_nitrogeno,
               `Dosis óptima económica (kg N / ha)` = dosis_monitoreo
        )
      return(datos_resultado)
      
    })
    
    output$tabla_monitoreoN <- renderUI({
      data <- resultados_monitoreo()
      
      tabla_html <- paste0(
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<thead><tr>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Lote</th>",
        "<th style='background-color: #CCCCCC; padding: 5px;'>Cultivo</th>",
        "<th style='background-color: #A3B18A60; padding: 5px;'>Índice de suficiencia de nitrógeno</th>",
        "<th style='background-color: #58815760; padding: 5px;'>Dosis óptima económica <br>(kg N / ha)</th>",
        
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
    
    output$descarga_monitoreoN <- downloadHandler(
      filename = function() {
        paste("monitoreoN_", Sys.Date(), ".xlsx", sep = "")  
      },
      content = function(file) {
        write_xlsx(resultados_monitoreo(), file)  
      }
    )
    
    
    
}


# Run the app ----
shinyApp(ui, server )

# rsconnect::forgetDeployment("I:/TRABAJO/CERBAS/Proyectos/Web_fertilizar/fertilizar")
# rsconnect::deployApp()
# renv::init()
# renv::restore()
# renv::snapshot() #para capturar todas las dependencias 

# renv::status() #para ver si hay paquetes no instalados

 # rsconnect::setAccountInfo(name='intabalcarce',
#                            token='C0EB33DC639D60FE1930A4CA5CC8141F',
#                            secret='xQn4aq7hXde1aFaoEpJM5BzIoBEKHw247ACHktKH')

# rsconnect::deployApp(appDir = "C:/Users/lewczuk.nuria/OneDrive - Instituto Nacional de Tecnologia Agropecuaria/Escritorio/shiny_app/Nutrientes_suelo", appPrimaryDoc = "app.R",
#                     appName = "Nutrientes_suelo", account = 'intabalcarce', server = 'shinyapps.io')
  
  