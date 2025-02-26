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
               
               tabPanel("Múltiples lotes",
                        h3(strong("Cálculo de la dosis recomendada de nitrógeno para cada lote")),
                        br(),
                        fluidRow(  
                          column(4, uiOutput("zonas_maiz"))
                        ),
                        div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                            
                            uiOutput("tabla_nitrogeno"),
                            uiOutput("mensaje_advertencia"),
                            br(),
                            downloadButton("descarga_N", "Descargar resultados (.CSV)")
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
                                                             label = strong(HTML("Rendimiento objetivo del cultivo 2º(t/ha)")),
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
                            downloadButton("descarga_P", "Descargar resultados (.CSV)")
                        )
                        # ,
                        # br(),
                        # br(),
                        # fluidRow(
                        #   column(
                        #     10, offset = 1,
                        #     withSpinner(plotOutput("multi_lotes_P", height = "500px"),
                        #                 type = 5,
                        #                 color = "#0dc5c1",
                        #                 size = 0.5),
                        #     div(style = "text-align: right; margin-top: 10px;",
                        #         downloadButton("download_plot_P", "Descargar Gráfico", class = "btn btn-success")
                        #     )
                        #   )
                        # )
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
                                     h4(strong("xxxxxxxxxxxxxxx")),
                                     fluidRow(
                                       column(6, 
                                              numericInput("nitrato_20_S",  
                                                           label = strong(HTML("N-nitrato (ppm) (0-20cm)")),
                                                           value = 0),
                                              numericInput("nitrato_40_s",  
                                                           label = strong(HTML("N-nitrato (ppm) (20-40cm)")),
                                                           value = 0),
                                              numericInput("nitrato_60_s",  
                                                           label = strong(HTML("N-nitrato (ppm) (40-60cm)")),
                                                           value = 0),
                                              numericInput("dens_ap_s",  
                                                           label = strong(HTML("Densidad aparente (g / cm3)")), 
                                                           value = 1.2)
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
                          h3(strong("Cálculo de la dosis recomendada de azufre para cada lote")),
                          br(),
                          fluidRow(  
                            column(4, uiOutput("zona_multi_s"))
                          ),
                          
                          div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                              
                              uiOutput("tabla_azufre"),
                              br(),
                              downloadButton("descarga_S", "Descargar resultados (.CSV)")
                          )
                 )
               )
             ),
    
    tabPanel("Recomendaciones",
             br(),
             h2(HTML("<strong>Recomendaciones</strong>")),
             h4(HTML("xxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx")),
             br(),
             div(style = "background-color: #DDB89240; padding: 15px; border-radius: 10px;",
                 
                 uiOutput("tabla_total"),
                 br(),
                 downloadButton("descarga_total", "Descargar resultados (.CSV)")
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
      modelo <- data.frame(
        Lote = c(1, 1, 1, 2, 2, 2), 
        Cultivo = c("maiz", "maiz", "maiz", "trigo", "trigo", "trigo"),
        Rendimiento_objetivo = c(NA, NA, NA, NA, NA, NA), 
        Cultivo_antecesor = c("", "", "", "soja", "soja", "soja"),
        Rendimiento_objetivo_cultivo_antecesor = c(NA, NA, NA, NA, NA, NA), 
        Efecto_antecesor = c(NA, NA, NA, NA, NA, NA),
        Proteina_objetivo = c(NA, NA, NA, NA, NA, NA),
        Nan_20 = c(NA, NA, NA, NA, NA, NA),
        Densidad_aparente = c(1.2, NA, NA, 1.0, NA, NA),
        Estrato = c("0-20", "20-40", "40-60", "0-20", "20-40", "40-60"),
        N_nitrato = c(NA, NA, NA, NA, NA, NA),
        P_Bray_actual = c(NA, NA, NA, NA, NA, NA),
        nivelP_objetivo = c(NA, NA, NA, NA, NA, NA),
        factor_construccion = c(NA, NA, NA, NA, NA, NA),
        Nutriente_en_grano_P = c(NA, NA, NA, NA, NA, NA),
        Nutriente_en_grano_S = c(NA, NA, NA, NA, NA, NA)
      )
      
      # Crear la segunda hoja: unidades
      unidades <- data.frame(
        Variable = c("Lote", "Cultivo", "Rendimiento_objetivo", "Cultivo_antecesor", "Rendimiento_objetivo_cultivo_antecesor", "Efecto_antecesor", "Proteina_objetivo",
                     "Nan_20", "Densidad_aparente", "Estrato", "N_nitrato", "P_Bray_actual", "nivelP_objetivo", 
                     "factor_construccion", "Nutriente_en_grano_P", "Nutriente_en_grano_S"),
        Unidad = c("Número de lote", "Nombre de cultivo", "tn/ha", "Nombre de cultivo antecesor", "tn/ha", "kg/ha", "%", "mg/kg", "g/cm³", 
                   "Rango de profundidad", "mg/kg", "P Bray", "ppm", "kg P/ppm", "kg P/t", "kg S/t")
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
      
      library(openxlsx)
      
      wb <- createWorkbook()
      
      # Agregar hojas al archivo
      addWorksheet(wb, "Modelo")
      addWorksheet(wb, "Unidades")
      addWorksheet(wb, "Aclaraciones")
      
      # Escribir los datos en las hojas
      writeData(wb, "Modelo", modelo)
      writeData(wb, "Unidades", unidades)
      writeData(wb, "Aclaraciones", aclaraciones)

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
     
      addStyle(wb, "Modelo", style = estilo_general1, rows = 1, cols = c(1:16), gridExpand = TRUE)
      addStyle(wb, "Modelo", style = estilo_general2, rows = c(2, 5), cols = c(1:9, 10:16), gridExpand = TRUE)
      addStyle(wb, "Modelo", style = estilo_general5, rows = c(4, 7), cols = c(10,11), gridExpand = TRUE)
      addStyle(wb, "Modelo", style = estilo_general4, rows = c(3, 6), cols = c(10,11), gridExpand = TRUE)
      addStyle(wb, "Modelo", style = estilo_general3, rows = c(4, 7), cols = c(1:9, 12:16), gridExpand = TRUE)
      
      
      
      # Crear estilo para negrita (nombres de columnas)
      estilo_negrita <- createStyle(textDecoration = "bold")
      addStyle(wb, "Unidades", style = estilo_negrita, rows = 1, cols = 1:ncol(unidades), gridExpand = TRUE)
      addStyle(wb, "Aclaraciones", style = estilo_negrita, rows = 1, cols = 1:ncol(aclaraciones), gridExpand = TRUE)
      
      # Auto ajuste del tamaño de las columnas
      # setColWidths(wb, "Modelo", cols = 1:ncol(modelo), widths = "auto")
      setColWidths(wb, "Unidades", cols = 1:ncol(unidades), widths = "auto")
      setColWidths(wb, "Aclaraciones", cols = 1:ncol(aclaraciones), widths = "auto")
      
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
      data <- readxl::read_xlsx(input$archivo_usuario$datapath)
    } else {
      showNotification("Formato de archivo no soportado.", type = "error")
      return(NULL)
    }
    
    # Verificar si el archivo tiene las columnas requeridas
    required_columns <- c("Lote", "Cultivo", "Rendimiento_objetivo", "Cultivo_antecesor", "Rendimiento_objetivo_cultivo_antecesor", "Efecto_antecesor", "Proteina_objetivo",
                          "Nan_20", "Densidad_aparente", "Estrato", "N_nitrato", "P_Bray_actual", "nivelP_objetivo", 
                          "factor_construccion", "Nutriente_en_grano_P", "Nutriente_en_grano_S")
    missing_columns <- setdiff(required_columns, colnames(data))
    
    if (length(missing_columns) > 0) {
      showNotification(
        paste("El archivo no tiene las columnas requeridas:", 
              paste(missing_columns, collapse = ", ")), 
        type = "error"
      )
      return(NULL) 
    }
    
    
    colnames(data) <- tolower(colnames(data))
    data$cultivo <- tolower(data$cultivo)
    data$cultivo_antecesor <- tolower(data$cultivo_antecesor)
    
    
    
    data <- data %>%
      mutate(
        n_nitrato_20 = ifelse(estrato == "0-20", n_nitrato, 0),
        n_nitrato_40 = ifelse(estrato == "20-40", n_nitrato, 0),
        n_nitrato_60 = ifelse(estrato == "40-60", n_nitrato, 0)
      )
    
    
    data <- data %>%
      group_by(lote, cultivo) %>%
      summarise(
        rendimiento_objetivo = first(rendimiento_objetivo),
        cultivo_antecesor = first(cultivo_antecesor),
        rendimiento_objetivo_cultivo_antecesor = first(rendimiento_objetivo_cultivo_antecesor),
        efecto_antecesor = first(efecto_antecesor),
        proteina_objetivo = first(proteina_objetivo),
        nan_20 = first(nan_20),
        densidad_aparente = first(densidad_aparente),
        n_nitrato_20 = max(n_nitrato_20, na.rm = TRUE),
        n_nitrato_40 = max(n_nitrato_40, na.rm = TRUE),
        n_nitrato_60 = max(n_nitrato_60, na.rm = TRUE),
        p_bray_actual = first(p_bray_actual),
        nivelp_objetivo = first(nivelp_objetivo),
        factor_construccion = first(factor_construccion),
        nutriente_en_grano_p = first(nutriente_en_grano_p),
        nutriente_en_grano_s = first(nutriente_en_grano_s)
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
    
    # Reemplazar valores vacíos (NA) con 0 en todas las columnas
    data[is.na(data)] <- 0
    
    # Confirmar al usuario que el archivo se ha procesado correctamente
    showNotification("Archivo subido correctamente.", type = "message")
    
    
    return(data)
  })
  
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
      selectInput("zona_multi_maiz", 
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
  
  resultados_nitrogeno <- reactive({
    req(data_usuario())
    req(input$zona_multi_maiz)
    
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
    
    zona_maiz <- input$zona_multi_maiz
    
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
      select(lote, cultivo, rendimiento_objetivo, DemandaN, efecto_antecesor, Nan_total, N_disponible, OfertaN, DosisN) %>%
      rename(`Lote` = lote,
             `Cultivo` = cultivo,
             `Rendimiento (tn/ha)` = rendimiento_objetivo,
             `Demanda N (kg N / ha)` = DemandaN,
             `Efecto antecesor` = efecto_antecesor,
             `N mineralizable (kg N / ha)` = Nan_total,
             `Nitrogeno Disponible (kg N / ha)` = N_disponible,
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
      "<th style='background-color: #FF991460; padding: 5px;'>Nitrogeno Disponible <br>(kg N / ha)</th>",
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
    
    HTML(tabla_html)
  })
  
  output$descarga_N <- downloadHandler(
    filename = function() {
      paste("resultados_N_", Sys.Date(), ".csv", sep = "")  
    },
    content = function(file) {
      write.csv(resultados_nitrogeno(), file, row.names = FALSE)  
    }
  )
  
  
  grafico_nitrogeno <- function(datos) {
    req("Lote" %in% names(datos), "Cultivo" %in% names(datos))
    
    datos <- datos %>%
      mutate(Titulo = paste("Lote", Lote, "-", toupper(Cultivo)), 
             Lote_num = as.numeric(gsub("\\D", "", Lote)))
    
    datos <- datos %>%
      arrange(Lote_num)
    
    datos_dosis <- datos %>%
      select(Lote, Cultivo, `Dosis N (kg N / ha)`, Titulo) 
    
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
      scale_y_continuous(limits = c(0, max(datos_dosis$`Dosis N (kg N / ha)` + 20, na.rm = TRUE)))
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
          HTML("<strong>Dosis de suficiencia (kg P / ha):</strong>")
        ),
        div(
          style = "display: flex; justify-content: space-between; width: 40%; align-items: center;",
          div(
            style = "display: flex; flex-direction: column; align-items: flex-start;",
            
            div(
              style = "font-size: 25px; font-weight: bold; margin-top: 10px;",
              if (!is.na(dosis_vals$dosis)) {
                paste(round(dosis_vals$dosis, 2))
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
              "Mínima: ", round(dosis_vals$min, 2)
            ),
            div(
              style = "font-size: 25px; font-weight: bold;",
              "Máxima: ", round(dosis_vals$max, 2)
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
    HTML(paste("<strong>Construcción:</strong>", round(construir_P(), 2), "kg P / ha"))
  })
 
  
  mantener_P <- reactive({
    req(input$rendimiento_P, input$factor_mantenimiento)  

    input$rendimiento_P * input$factor_mantenimiento
  })
  
  output$mantener_P <- renderUI({
    HTML(paste("<strong>Mantenimiento:</strong>", round(mantener_P(), 2), "kg P / ha"))
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
    HTML(paste("<strong>Mantenimiento:</strong>", round(mantener_P_2(), 2), "kg P / ha"))
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
        HTML("<strong>Dosis de construcción y mantenimiento (kg P / ha):</strong>")
      ),
      div(
        style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",

        div(
          style = "font-size: 30px; font-weight: bold;",
          round(dosis_valor, 2)
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
        cultivo_antecesor = trimws(tolower(cultivo_antecesor)), 
        rendimiento_objetivo_cultivo_antecesor = as.numeric(rendimiento_objetivo_cultivo_antecesor),
        p_bray_actual = as.numeric(p_bray_actual),
        nivelp_objetivo = as.numeric(nivelp_objetivo),
        factor_construccion = as.numeric(factor_construccion),
        nutriente_en_grano_p = as.numeric(nutriente_en_grano_p)
      )
    
    factores_mantener <- c("soja" = 4.5, "trigo" = 3.2, "maiz" = 2.6, "girasol" = 6, "papa" = 0.45)
    factores_construir <- c("soja" = 3, "trigo" = 3, "maiz" = 3, "girasol" = 3, "papa" = 4)
    niveles_p <- c("soja" = 20, "trigo" = 20, "maiz" = 20, "girasol" = 20, "papa" = 30)
    
    datos <- datos %>%
      mutate(
        factor_mantener = ifelse(nutriente_en_grano_p == 0, factores_mantener[cultivo], nutriente_en_grano_p),
        factor_construir = ifelse(factor_construccion == 0, factores_construir[cultivo], factor_construccion),
        nivel_p = ifelse(nivelp_objetivo == 0, niveles_p[cultivo], nivelp_objetivo)
      )
    
    
    
    datos <- datos %>%
      mutate(
        filtro_rango = map2(cultivo, p_bray_actual, ~ {
          result <- dosis_data %>% filter(cultivoP == .x, .y >= P_min, .y < P_max)
          result
        }),
        dosis_suficiencia_min = map_dbl(filtro_rango, ~ {
          if (!is.null(.x) && nrow(.x) > 0) {
            min(.x$min_dosis, na.rm = TRUE)
          } else {
            print("Dosis mínima no encontrada")
            NA_real_
          }
        }),
        dosis_suficiencia_max = map_dbl(filtro_rango, ~ {
          if (!is.null(.x) && nrow(.x) > 0) {
            max(.x$max_dosis, na.rm = TRUE)
          } else {
            print("Dosis máxima no encontrada")
            NA_real_
          }
        })
      ,
        mantener_P = rendimiento_objetivo * factor_mantener,
        construir_P = pmax(0, (nivel_p - p_bray_actual) * factor_construir),
      
      mantener_P_antecesor = ifelse(
        !is.na(cultivo_antecesor) & cultivo_antecesor != "",
        rendimiento_objetivo_cultivo_antecesor * factores_mantener[tolower(cultivo_antecesor)],
        0
      ),
      
      # Suma de construir_P y construir_P_antecesor
      mantener_P_total = mantener_P + mantener_P_antecesor,
      dosisCyM = construir_P + mantener_P_total
      
      ) %>%
      ungroup()
    
    # Seleccionar columnas relevantes
    datos_resultado <- datos %>%
      select(
        lote, cultivo, cultivo_antecesor, rendimiento_objetivo, dosis_suficiencia_min, dosis_suficiencia_max, 
        construir_P, mantener_P, dosisCyM
      ) %>%
      rename(Lote = lote,
             Cultivo = cultivo,
             `Cultivo antecesor` = cultivo_antecesor,
             `Rendimiento (tn/ha)` = rendimiento_objetivo,
             `Dosis minima de suficiencia (kg P / ha)` = dosis_suficiencia_min,
             `Dosis maxima de suficiencia (kg P / ha)` = dosis_suficiencia_max,
             `Dosis de construccion (kg P / ha)` = construir_P,
             `Dosis de mantenimiento (kg P / ha)` = mantener_P,
             `Dosis de construccion y mantenimiento (kg P / ha)` = dosisCyM
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
        "<th style='background-color: #58815760; padding: 5px;'>Dosis minima de suficiencia<br>(kg P / ha)</th>",
        "<th style='background-color: #58815760; padding: 5px;'>Dosis maxima de suficiencia<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de construccion<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de mantenimiento<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de construccion y mantenimiento <br>(kg P / ha)</th>",
        
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
        paste("resultados_P_", Sys.Date(), ".csv", sep = "")  
      },
      content = function(file) {
        write.csv(resultados_fosforo(), file, row.names = FALSE)  
      }
    )

    
    # grafico_fosforo <- reactive({
    #   req(resultados_fosforo())
    #   
    #   datos <- resultados_fosforo()
    #   
    #   # Asegurarse de que la columna Titulo se cree correctamente
    #   datos <- datos %>%
    #     mutate(
    #       Titulo = paste("Lote", Lote, "-", toupper(Cultivo)),
    #       Lote_num = as.numeric(gsub("\\D", "", Lote))         
    #     ) %>%
    #     arrange(Lote_num)
    #   
    #   # Asegurarse de que los datos están en formato largo para el gráfico
    #   datos_largo <- datos %>%
    #     pivot_longer(
    #       cols = c(`Dosis minima de suficiencia (kg P / ha)`, 
    #                `Dosis maxima de suficiencia (kg P / ha)`,
    #                `Dosis de construccion (kg P / ha)`, 
    #                `Dosis de mantenimiento (kg P / ha)`),
    #       names_to = "Tipo",        
    #       values_to = "Valor"       
    #     ) %>%
    #     mutate(
    #       Tipo = factor(Tipo, 
    #                     levels = c("Dosis minima de suficiencia (kg P / ha)", 
    #                                "Dosis maxima de suficiencia (kg P / ha)",
    #                                "Dosis de construccion (kg P / ha)", 
    #                                "Dosis de mantenimiento (kg P / ha)"))
    #     )
    #   
    #   # Crear el gráfico de barras agrupadas
    #   ggplot(datos_largo, aes(x = Titulo, y = Valor, fill = Tipo)) +
    #     geom_bar(
    #       data = subset(datos_largo, Tipo %in% c("Dosis de construccion (kg P / ha)", "Dosis de mantenimiento (kg P / ha)")),
    #       stat = "identity", 
    #       position = "stack",  
    #       color = "#D4A373",
    #       width = 0.6
    #     ) +
    #     geom_bar(
    #       data = subset(datos_largo, Tipo %in% c("Dosis minima de suficiencia (kg P / ha)", "Dosis maxima de suficiencia (kg P / ha)")),
    #       stat = "identity", 
    #       position = position_dodge(width = 0.8), 
    #       color = "#CCD5AE",
    #       width = 0.6
    #     ) +
    #     geom_text(data = subset(datos_largo, Tipo %in% c("Dosis de construccion (kg P / ha)", "Dosis de mantenimiento (kg P / ha)")),
    #               aes(label = round(Valor, 0)), 
    #               position = position_stack(vjust = 1),  # Centro de las barras apiladas
    #               vjust = -0.5, size = 5, color = "black", 
    #               fontface = "bold") +
    #     # Etiquetas para las barras agrupadas
    #     geom_text(data = subset(datos_largo, Tipo %in% c("Dosis minima de suficiencia (kg P / ha)", "Dosis maxima de suficiencia (kg P / ha)")),
    #               aes(label = round(Valor, 0)), 
    #               position = position_dodge(width = 0.8),  
    #               vjust = -0.5, size = 5, color = "black", 
    #               fontface = "bold") +   
    #     scale_fill_manual(
    #       values = c("Dosis minima de suficiencia (kg P / ha)" = "#58815760", 
    #                  "Dosis maxima de suficiencia (kg P / ha)" = "#588157",
    #                  "Dosis de construccion (kg P / ha)" = "#BC6C25", 
    #                  "Dosis de mantenimiento (kg P / ha)" = "#BC6C2560")
    #     ) +
    #     labs(
    #       title = "",
    #       x = "",
    #       y = "kg P / ha",
    #       fill = "Componente"
    #     ) +
    #     theme_minimal() +
    #     theme(
    #       axis.text.y = element_text(face = "bold", size = 16),
    #       axis.text.x = element_text(face = "bold", size = 16, angle = 45, hjust = 1),
    #       axis.title.x = element_text(face = "bold", size = 16),
    #       axis.title.y = element_text(face = "bold", size = 16),
    #       legend.position = "right",  
    #       legend.title = element_blank(),  
    #       legend.text = element_text(face = "bold", size = 16),
    #       strip.text = element_text(face = "bold", size = 16),
    #       plot.title = element_text(hjust = 0.5, face = "bold"),
    #       plot.caption = element_text(size = 14, hjust = 1, face = "italic")
    #     )
    # })
    # 
    # 
    # 
    # # Renderizar el gráfico en el UI
    # output$multi_lotes_P <- renderPlot({
    #   
    #   grafico_fosforo()
    # })
    # 
    # 
    # output$download_plot_P <- downloadHandler(
    #   filename = function() {
    #     paste("dosisP_lotes", Sys.Date(), ".png", sep = "")
    #   },
    #   content = function(file) {
    #     # Guardar el gráfico como archivo
    #     png(file, width = 1000, height = 600, res = 60)
    #     print(
    #       grafico_fosforo() +
    #         labs(title = "Dosis recomendada de fósforo para cada lote",
    #              caption = Sys.Date()
    #         ) +  
    #         theme(plot.title = element_text(face = "bold", size = 14, hjust = 0, vjust = 1.1),
    #               plot.caption = element_text(size = 12, hjust = 1, face = "italic"),
    #               plot.title.position = "plot",
    #               legend.position = "bottom",
    #               legend.box.margin = margin(t = 10)
    #               )  
    #     )
    #     dev.off()
    #   }
    # )
    
    ############################ Azufre #########################################
  
    observeEvent(input$cultivoS, {  
      req(input$cultivoS)  
      
      if (input$cultivoS == "maiz") {
        updateNumericInput(session, "factor_s", value = 2.6)
        
      } else if (input$cultivoS == "soja") {
        updateNumericInput(session, "factor_s", value = 4.5)
        
      } else if (input$cultivoS == "trigo") {
        updateNumericInput(session, "factor_s", value = 3.2)
        
      } else if (input$cultivoS == "girasol") {
        updateNumericInput(session, "factor_s", value = 6)
        
      } else if (input$cultivoS == "papa") {
        updateNumericInput(session, "factor_s", value = 0.45)
      }
    })
    
    
    dosis_s <- reactive({
      req(input$cultivoS, input$rendimiento_s, input$factor_s)  
      
      input$rendimiento_s * input$factor_s
    })
    
    output$dosis_s <- renderUI({
      
      req(input$nitrato_20_S, input$nitrato_40_s, input$nitrato_60_s, input$nan_s, input$zona_s, input$dens_ap_s)
      
      # Condiciones
      suma_nitratos <- (input$nitrato_20_S + input$nitrato_40_s + input$nitrato_60_s) * input$dens_ap_s
      condicion1 <- (input$nitrato_20_S * input$dens_ap_s) < 10
      condicion2 <- suma_nitratos < 45
      condicion3 <- (input$zona_s == "Sudeste de Bs.As" && input$nan_s < 50) || 
        (input$zona_s == "Otras zonas" && input$nan_s < 30)
      
      if ((condicion1 || condicion2) && condicion3) {
        
      dosis_valor <- dosis_s()  
      
      div(
        class = "value-box",
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; background-color: #DE9E36; color: white; border-radius: 10px; height: 160px; width: 300px; padding: 10px;",

        div(
          style = "font-size: 20px; font-weight: bold; margin-bottom: 6px; text-align: center;",
          HTML("<strong>Dosis de azufre (kg S / ha):</strong>")
        ),
        div(
          style = "display: flex; justify-content: space-between; width: 60%; align-items: center;",
          
          div(
            style = "font-size: 30px; font-weight: bold;",
            round(dosis_valor, 2)
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
                    label = strong("Seleccione la zona geográfica"),
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
          n_nitrato_20 = as.numeric(n_nitrato_20), 
          n_nitrato_40 = as.numeric(n_nitrato_40),
          n_nitrato_60 = as.numeric(n_nitrato_60),
          nan_20 = as.numeric(nan_20),
          densidad_aparente = as.numeric(densidad_aparente)
        )
      
      factores_s <- c("soja" = 4.5, "trigo" = 3.2, "maiz" = 2.6, "girasol" = 6, "papa" = 0.45)
      
      
      datos <- datos %>%
        mutate(
          factor_s = ifelse(nutriente_en_grano_s == 0, factores_s[cultivo], nutriente_en_grano_s)
        )
      
      datos <- datos %>%
        mutate(
          suma_nitratos = ((n_nitrato_20 + n_nitrato_40 + n_nitrato_60) * densidad_aparente),
          
          condiciones_cumplidas = case_when(
            ((n_nitrato_20 * densidad_aparente) < 10 | (suma_nitratos * densidad_aparente) < 45) & 
              ((input$zona_s == "Sudeste de Bs.As." & nan_20 < 50) | 
                 (input$zona_s == "Otra" & nan_20 < 30)) ~ TRUE,
            TRUE ~ FALSE
          ),
          
          dosis_s = ifelse(
            condiciones_cumplidas, 
            rendimiento_objetivo * factor_s,
            "No se recomienda fertilizar con azufre"
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
               `Dosis de azufre (kg S / ha)` = dosis_s
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
        "<th style='background-color: #DEB84160; padding: 5px;'>Dosis de azufre <br>(kg S / ha)</th>",
        
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
        paste("resultados_S_", Sys.Date(), ".csv", sep = "")  
      },
      content = function(file) {
        write.csv(resultados_azufre(), file, row.names = FALSE)  
      }
    )
    ######################## RECOMENDACIONES ############################

    resultados_total <- reactive({

      nitrogeno <- resultados_nitrogeno()
      fosforo <- resultados_fosforo()
      azufre <- resultados_azufre()
      
      # Verifica si los datos existen
      if (is.null(nitrogeno) | is.null(fosforo) | is.null(azufre)) {
        return(NULL)  
      }
      
      nitrogeno_seleccionada <- nitrogeno[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis N (kg N / ha)")]
      fosforo_seleccionada <- fosforo[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis minima de suficiencia (kg P / ha)", "Dosis maxima de suficiencia (kg P / ha)", "Dosis de construccion y mantenimiento (kg P / ha)")]
      azufre_seleccionada <- azufre[, c("Lote", "Cultivo", "Rendimiento (tn/ha)", "Dosis de azufre (kg S / ha)")]

      
      fosforo_seleccionada <- fosforo_seleccionada[, !colnames(fosforo_seleccionada) %in% c("Cultivo", "Rendimiento (tn/ha)")]
      azufre_seleccionada <- azufre_seleccionada[, !colnames(azufre_seleccionada) %in% c("Cultivo", "Rendimiento (tn/ha)")]

      tabla_general <- nitrogeno_seleccionada %>%
        left_join(fosforo_seleccionada, by = "Lote") %>%
        left_join(azufre_seleccionada, by = "Lote")

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
        "<th style='background-color: #58815760; padding: 5px;'>Dosis de minima suficiencia<br>(kg P / ha)</th>",
        "<th style='background-color: #58815760; padding: 5px;'>Dosis de maxima suficiencia<br>(kg P / ha)</th>",
        "<th style='background-color: #BC6C2560; padding: 5px;'>Dosis de construccion y mantenimiento<br>(kg P / ha)</th>",
        "<th style='background-color: #DEB84160; padding: 5px;'>Dosis de azufre <br>(kg S / ha)</th>",

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
        paste("resultados_total_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(resultados_total(), file, row.names = FALSE)
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
  
  