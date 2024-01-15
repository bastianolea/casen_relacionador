library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(stringr)
library(fresh)
library(shinycssloaders)
library(glue)

options(scipen=999)

color_fondo = "#45171d"
color_secundario = "#ff847c"
color_detalle = "#ff847c"
color_texto = "#fecea8"
color_destacado = "#e84a5f"

color_fondo = "#3c1b1f"
color_secundario = "#db8d6c"
color_detalle = "#62393e"
color_texto = "#fecea8"
color_destacado = "#b21e4b"
color_enlaces = "#cc3865"

# diccionario de variables
source("variables.R")

#cargar datos ----
casen_comunas <- readr::read_csv2("casen_comunas.csv", 
                                  col_types = c(rep("c", 3), rep("n", 44))
)
variables_numericas <- casen_comunas |> select(where(is.numeric)) |> select(-cut_comuna) |> names()


# funciones ----
#se usa para transformar el nombre de la variable (desde los inputs) a la etiqueta
nombre_variable <- function(texto) {
  # texto = input$selector_y
  unlist(variables)[unlist(variables) == texto] |> names() |> str_remove(".*\\.")
}

estilo_cuadros <- glue("margin: -6px; margin-top: 10px; margin-bottom: 10px;
                           padding: 20px;
                           padding-top: 0;
                           border-radius: 8px;
           border: 3px solid {color_detalle} ;")

#—----
# ui ----
ui <- fluidPage(
  title = "Relacionador Casen", 
  lang = "es",
  
  use_googlefont("Urbanist"), #cargar fuente o tipo de letra
  use_googlefont("DM Serif Display"),
  
  use_theme(create_theme(
    theme = "default",
    bs_vars_input(bg = color_fondo),
    bs_vars_global(body_bg = color_fondo, 
                   text_color = color_texto, 
                   link_color = color_destacado),
    bs_vars_font(size_base = "19px", #aumentar globalmente tamaño de letra  
                 family_sans_serif = "Urbanist" #cargar fuente o tipo de letra
    ), 
    bs_vars_modal(content_bg = color_fondo, content_border_color = color_detalle, 
                  backdrop_bg = color_fondo, backdrop_opacity = "60%"),
    bs_vars_button(
      default_color = color_fondo,
      default_bg = color_destacado,
      default_border = color_fondo, 
      border_radius_base = "6px"
    )
  )),
  
  # css ----
  tags$style(paste0(
    "h1 {
    font-size: 250%;
    font-weight: bold;
    color: ", color_destacado, ";
    }")),
  
  tags$style(paste0("
    h1, h2, h3 {
    font-weight: bold;
    font-family: DM Serif Display;
    color: ", color_destacado, ";
    }")),
  
  #labels de inputs
  tags$style(paste0("
    h4 {
    font-style: italic;
    font-size: 110%;
    margin-bottom: 6px;
    font-family: DM Serif Display;
    color: ", color_secundario, ";
    }")),
  
  #enlaces
  tags$style(paste0("
    a {
    text-decoration: underline;
    color: ", color_destacado, ";
    }")),
  
  #texto de pickers
  tags$style(paste0("
                    .btn.dropdown-toggle {
                   font-size: 85%;
                   }")),
  
  # >a, .dropdown-menu>.active>a:hover, .dropdown-menu>.active>a:focus {
  tags$style(paste0("
         .dropdown-menu,  .divider {
          color: black !important;
         background: ", color_destacado, " !important;
         }
  
         .dropdown-header {
         color: black !important;
         font-family: DM Serif Display;
         font-weight: bold;
         font-size: 110%;
         }
         .text {
         color: black !important;
         }
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_destacado, " !important;
         }
.selected {
background-color: ", color_secundario, " !important;
color: ", color_fondo, " !important;
}")),

#botones, botones hover
tags$style(paste0("
    .action-button {
    opacity: 0.6; font-size: 80%; padding: 4px; padding-left: 8px; padding-right: 8px; color: black; 
    border: 3px solid", color_enlaces, ";
    }
    .action-button:hover, .action-button:active, .action-button:focus {
    opacity: 1;
    color: black; 
    border: 3px solid", color_destacado, ";
    }")),

#separador
tags$style(paste0("
                    hr {
  border-top: 3px solid ", color_detalle, ";
                    }")),

# tags$style(paste0("
# .selectize-input, .selectize-input.full {
# background: ", color_detalle, ";
# color: ", color_fondo, ";
# font-size: 80%;
# padding: 12px;
# border: 0;
# }
# 
# .selectize-control.single .selectize-input:after {
# border-color: ", color_fondo, " transparent transparent transparent;
# }")),
# 


# .selectize-dropdown, .shiny-input-select, .form-control, .selectize-dropdown-content {
# background: ", color_detalle, ";
# color: ", color_fondo, ";
# }")),



# header ----
fluidRow(  
  column(12,
         h1("Relacionador de datos Casen 2022"),
         p("Este visualizador le permite analizar la relación entre múltiples datos socioeconómicos de las comunas del país, en base a los datos de la",
           tags$a("Encuesta de caracterización socioeconómica nacional (Casen) 2022", target = "_blank", href = "https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022")),
         p("El gráfico expresa cómo se posicionan las comunas entre dos ejes que pueden representar ingresos, condiciones de vida, o situaciones de vulnerabilidad,
             expresando así la relación entre las desigualdades y condiciones de vida del país."),
         
  )
),

#selectores ----
fluidRow(
  column(12,
         hr(),
         h3("Seleccionar variables"),
         p("Seleccione variables socioeconómicas de su interés para relacionarlas unas con otras, o bien, elija variables al azar usando los botones para descubrir nuevas relaciones.")
  )
),
fluidRow(
  column(12,
         #eje x
         column(4, 
                div(style = estilo_cuadros,
                    pickerInput("selector_x",
                                label = h4("Variable para el eje horizontal"),
                                width = "100%",
                                choices = variables, multiple = FALSE,
                                options = list( `live-search` = TRUE)
                    ),
                    actionButton("azar_x", "Elegir eje X al azar")
                )
         ),
         #eje y
         column(4, 
                div(style = estilo_cuadros,
                    pickerInput("selector_y",
                                label = h4("Variable para el eje vertical"),
                                width = "100%",
                                choices = variables, multiple = FALSE,
                                selected = "ytotcorh",
                                options = list( `live-search` = TRUE)
                    ),
                    actionButton("azar_y", "Elegir eje Y al azar")
                )
         ),
         
         #tamaño
         column(4, div(
           style = estilo_cuadros,
           pickerInput("selector_size",
                       label = h4("Variable para el tamaño"),
                       width = "100%",
                       choices = variables, multiple = FALSE,
                       selected = "poblacion",
                       options = list( `live-search` = TRUE)
           ),
           actionButton("azar_size", "Elegir tamaño al azar")
         )
         )
  )
),

fluidRow(
  column(12, align = "center", 
         style = "margin-top: 14px; margin-bottom: 24px;",
         actionButton("azar", "Elegir todas las variables al azar", style = "padding-left: 24px; padding-right: 24px;")
  )
),

# territorios ----
fluidRow(
  column(12,
         hr(),
         h3("Seleccionar territorios"),
         p("Elija una o más regiones para posteriormente elegir una o varias comunas que serán incluidas en el gráfico.")
  )
),
fluidRow(
  column(12,
         column(6, 
                div(style = paste(estilo_cuadros, "padding-bottom: 55px;"),
                    pickerInput("selector_regiones",
                                label = h4("Regiones"),
                                width = "100%",
                                multiple = TRUE,
                                selected = "Región Metropolitana de Santiago", 
                                choices = NULL,
                                options = list( `live-search` = TRUE)
                    )
                    # em("Seleccione una o más regiones para filtrar el selector de comunas a continuación")
                )
         ),
         column(6,  
                div(style = estilo_cuadros,   
                    pickerInput("selector_comunas",
                                label = h4("Comunas que desea graficar"),
                                width = "100%",
                                multiple = TRUE,
                                choices = NULL,
                                selected = c("La Florida", "Puente Alto", "La Pintana", "Ñuñoa", "Vitacura", "Providencia", "Lo Barnechea"),
                                options = list( `live-search` = TRUE)
                    ),
                    
                    actionButton("azar_comunas", "Elegir comunas al azar")
                )
         )
  )
  
),


#grafico ----
fluidRow(
  column(12,
         hr(),
         h3("Visualizar"),
         
         # div(style = "line-height: 1em;",
         uiOutput("texto_etiqueta_x")|> div(style = "margin-bottom: -10px;"),
         uiOutput("texto_etiqueta_y") |> div(style = "margin-bottom: -10px;"),
         uiOutput("texto_etiqueta_size")|> div(style = "margin-bottom: -10px;")
         
  ),
  column(12, align = "center", style = "padding: 24px;",
         plotOutput("grafico", width = 600, height = 500)
  )
),

# firma ----
fluidRow(
  column(12,
         hr(),
         p("Diseñado y programado por",
           tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
         p(
           "Código de fuente de esta app y del procesamiento de los datos",
           tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/casen_relacionador")
         )
  )
)

)


#—----

server <- function(input, output, session) {
  
  
  
  #selectores ----
  
  updatePickerInput(session,
                    inputId = "selector_regiones",
                    choices = c("Todas las regiones", unique(casen_comunas$region)),
                    selected = "Región Metropolitana de Santiago"
  )
  
  #pone las comunas en el selector de comunas según la región elegida
  
  lista_comunas <- reactive({
    if ("Todas las regiones" %in% input$selector_regiones) {
      lista_comunas <- split(casen_comunas$comuna, casen_comunas$region)
    } else {
      casen_region <- casen_comunas |> filter(region == input$selector_regiones)
      lista_comunas <- split(casen_region$comuna, casen_region$region)
    }  
    return(lista_comunas)
  }) |> bindEvent(input$selector_regiones)
  
  
  observeEvent(input$selector_regiones, {
    req(length(input$selector_regiones) > 0)
    
    updatePickerInput(session,
                      inputId = "selector_comunas",
                      options = list( `live-search` = TRUE),
                      selected = c("La Florida", "La Pintana", "Ñuñoa", "Vitacura", "Providencia"),
                      choices = lista_comunas()
    )
  })
  
  observeEvent(input$azar_comunas, {
    azar_comunas <- sample(unlist(lista_comunas()), 5)
    updatePickerInput(session,
                      inputId = "selector_comunas",
                      options = list( `live-search` = TRUE),
                      selected = azar_comunas
    )
  })
  
  
  
  # para que al apretar algún botón "al azar" no salgan variables ya elegidas
  variables_en_uso <- reactive(c(input$selector_y, input$selector_x, input$selector_size))
  variables_numericas_sin_en_uso <- reactive(variables_numericas[!(variables_numericas %in% variables_en_uso())])
  
  ## variables al azar ----
  
  #todas al azar
  observeEvent(input$azar,{
    updateSelectizeInput(session, inputId = "selector_y",
                         selected = sample(variables_numericas, 1))
    updateSelectizeInput(session, inputId = "selector_x",
                         selected = sample(variables_numericas, 1))
    updateSelectizeInput(session, inputId = "selector_size",
                         selected = sample(variables_numericas, 1))
  })
  
  #azar y
  observeEvent(input$azar_y, {
    updateSelectizeInput(session, inputId = "selector_y",
                         selected = sample(variables_numericas_sin_en_uso(), 1))
  })
  
  #variable x aleatoria al iniciar
  updateSelectizeInput(session, inputId = "selector_x",
                       selected = sample(variables_numericas, 1))
  
  #azar x
  observeEvent(input$azar_x, {
    updateSelectizeInput(session, inputId = "selector_x",
                         selected = sample(variables_numericas_sin_en_uso(), 1))
  })
  
  #azar tamaño
  observeEvent(input$azar_size, {
    updateSelectizeInput(session, inputId = "selector_size",
                         selected = sample(variables_numericas, 1))
  })
  
  # etiquetas ----
  etiqueta_y <- reactive(nombre_variable(input$selector_y))
  etiqueta_x  <- reactive(nombre_variable(input$selector_x))
  etiqueta_size <- reactive(nombre_variable(input$selector_size))
  comunas <- reactive({
    req(length(input$selector_comunas) > 0)
    comunas <- input$selector_comunas |> paste(collapse = ", ")
    comunas
  })
  
  output$texto_etiqueta_y <- renderUI(p("Eje vertical:", 
                                        strong(etiqueta_y(), style = paste0("color: ", color_destacado, ";")), 
                                        #p(glue("({input$selector_y})"), style = paste0("color: ", color_detalle, ";"))
                                        ))
  output$texto_etiqueta_x <- renderUI(p("Eje horizontal:", 
                                        strong(etiqueta_x(), style = paste0("color: ", color_destacado, ";"))
                                        ))
  output$texto_etiqueta_size <- renderUI(p("Tamaño:", 
                                           strong(etiqueta_size(), style = paste0("color: ", color_destacado, ";"))
                                                             ))
  
  # calcular ----
  datos <- reactive({
    req(length(input$selector_comunas) > 0)
    
    message("comunas: ", comunas())
    message("variable x: ", input$selector_x)
    message("variable y: ", input$selector_y)
    message("variable size: ", input$selector_size)
    message(">")
    
    casen_comunas %>%
      #filtrar comunas
      filter(comuna %in% input$selector_comunas) |> 
      select(comuna, region,
             any_of(c(input$selector_x,
                      input$selector_y,
                      input$selector_size)))
  })
  
  
  
  
  #gráfico ----
  output$grafico <- renderPlot({
    
    p <- datos() |> 
      ggplot(aes(x = .data[[input$selector_x]],
                 y = .data[[input$selector_y]],
                 col = comuna,
                 size = .data[[input$selector_size]])
      ) +
      geom_point(col= "white") +
      geom_point(alpha=0.7) +
      ggrepel::geom_text_repel(aes(label = comuna),
                               size = 4, point.padding = 25,
                               min.segment.length = 2,
                               show.legend = FALSE) +
      ### escalas ----
    scale_size(range = c(5, 15),
               labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      coord_cartesian(clip = "off") +
      ### tema ----
    theme_light(base_size = 18) +
      theme(legend.position = "right") +
      theme(axis.line = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
            axis.ticks = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color = color_detalle),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(color = color_texto, size = 13),
            legend.text = element_text(color = color_texto, size = 13),
            legend.title = element_text(color = color_destacado, face = "bold", size = 17),
            axis.title = element_text(color = color_destacado, face = "bold", size = 17)
      ) +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      guides(size = guide_legend(override.aes = list(color = color_detalle)),
             col = guide_legend(override.aes = list(size = 6))) +
      # #etiquetas
      labs(y = etiqueta_y(),
           x = etiqueta_x(),
           col = "Comunas",
           size = etiqueta_size() |> str_wrap(25)
      )
    # browser()
    
    # nombre_variable(input$selector_y)
    
    return(p)
    
  })
  
  
  
  
}


shinyApp(ui = ui, server = server)
