library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(stringr)
library(fresh)
library(shinycssloaders)
library(glue)

options(scipen=999)

color_fondo = "#301519" 
# color_fondo = "#3c1b1f"
color_secundario = "#db8d6c"
color_detalle = "#62393e"
color_texto = "#fecea8"
color_destacado = "#b21e4b"
color_enlaces = "#cc3865"

# diccionario de variables
source("variables.R")

#cargar datos ----
# setwd("app/")
casen_comunas <- readr::read_csv2("casen_comunas.csv", 
                                  col_types = c(rep("c", 3), rep("n", 44))
) |> 
  #seleccionar solo columnas que van a usarse
  select(comuna, region,
         any_of(c("pco1", variables |> unlist() |> unname()))
  )

variables_numericas <- casen_comunas |> select(where(is.numeric)) |> names()


# funciones ----
#se usa para transformar el nombre de la variable (desde los inputs) a la etiqueta
nombre_variable <- function(texto) {
  # texto = input$selector_y
  # texto = "ingreso_ytrabajocor_menor_2medianas_p"
  # unlist(variables)[unlist(variables) == texto] |> names() |> str_remove("^.*\\.")
  unlist(variables)[unlist(variables) == texto] |> names() |> str_remove("^[^\\.]*\\.")
  
}

estilo_cuadros <- glue("margin: -6px; margin-top: 10px; margin-bottom: 14px;
                           padding: 16px;
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
  
  #colores pickers
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
         font-size: 80%;
         }
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .form-control:focus {
         border-color: ", color_secundario, ";
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_destacado, " !important;
         }
         .selected {
         background-color: ", color_secundario, " !important;
         color: ", color_fondo, " !important;
         }
         
         .bs-placeholder, .bs-placeholder:active, bs-placeholder:focus, .bs-placeholder:hover {
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
  
  
  
  # header ----
  fluidRow(  
    column(12,
           div(style = "margin-bottom: 16px;",
               h1("Relacionador de datos Casen 2022"),
               em("Bastián Olea Herrera")
           ),
           p("Este visualizador permite analizar la relación entre múltiples datos socioeconómicos de un conjunto personalizable de comunas del país, en base a los datos de la",
             tags$a("Encuesta de caracterización socioeconómica nacional (Casen) 2022", target = "_blank", href = "https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022")),
           
           p("El gráfico generado, ubicado en la parte inferior, visualiza el posicionamiento de las comunas elegidas entre dos ejes numéricos (horizontal y vertical), que pueden representar ingresos, condiciones de vida, o situaciones de vulnerabilidad,
             expresando así la relación entre las desigualdades y condiciones de vida del país."),
           
    )
  ),
  
  
  fluidRow(
    
    # territorios ----
    column(12,
           hr(),
           h3("Seleccionar territorios"),
           p("Elija una o más regiones para luego elegir una o varias comunas para incluir en el gráfico:"),
           
           fluidRow(
             column(6,
                    
                    div(style = paste(estilo_cuadros, "padding-bottom: 55px;"),
                        pickerInput("selector_regiones",
                                    label = h4("Regiones"),
                                    width = "100%",
                                    multiple = TRUE,
                                    selected = "Región Metropolitana de Santiago", 
                                    choices = NULL,
                                    # options = list(`live-search` = TRUE)
                                    options = list(noneSelectedText = "Sin selección",
                                                   width = FALSE)
                        )
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
                                    options = list(`live-search` = TRUE,
                                                   width = FALSE,
                                                   noneSelectedText = "Sin selección")
                        ),
                        
                        actionButton("azar_comunas", "Elegir comunas al azar")
                    )
             )
           ),
           hr()
    )
  ),
  fluidRow(
    column(4,
           
           #selectores ----
           fluidRow(
             column(12,
                    # hr(),
                    h3("Seleccionar variables", style = "margin-top: 0;"),
                    
                    #eje x
                    div(style = estilo_cuadros,
                        pickerInput("selector_x",
                                    label = h4("Variable para el eje horizontal"),
                                    choices = variables, 
                                    multiple = FALSE, width = "100%", 
                                    options = list(`live-search` = TRUE,
                                                   width = FALSE,
                                                   options = list(noneSelectedText = "Sin selección"))
                        ),
                        actionButton("azar_x", "Elegir eje X al azar")
                    ),
                    
                    #eje y
                    div(style = estilo_cuadros,
                        pickerInput("selector_y",
                                    label = h4("Variable para el eje vertical"),
                                    choices = variables, selected = "ytotcorh",
                                    multiple = FALSE, width = "100%", 
                                    options = list(`live-search` = TRUE,
                                                   width = FALSE,
                                                   options = list(noneSelectedText = "Sin selección"))
                        ),
                        actionButton("azar_y", "Elegir eje Y al azar")
                    ),
                    #tamaño
                    div(style = estilo_cuadros,
                        pickerInput("selector_size",
                                    label = h4("Variable para el tamaño"),
                                    choices = variables, selected = "poblacion",
                                    multiple = FALSE, width = "100%", 
                                    options = list(`live-search` = TRUE,
                                                   width = FALSE,
                                                   options = list(noneSelectedText = "Sin selección"))
                        ),
                        actionButton("azar_size", "Elegir tamaño al azar")
                    ),
                    p("Seleccione variables socioeconómicas de su interés para relacionarlas unas con otras, o bien, elija variables al azar usando los botones para descubrir nuevas relaciones.")
             )
           )
           
    ),
    column(8,
           #grafico de dispersión ----
           fluidRow(
             column(12,
                    # hr(),
                    h3("Visualizar", style = "margin-top: 0;"),
                    
                    p("Gráfico de dispersión donde la posición de cada comuna (cada círculo) representa su situación en relación a las variables horizontales y verticales. 
                      La posición alta o baja del círculo indica que en esa comuna existe un mayor o menor valor de la variable horizontal, 
                      y su ubicación entre izquierda y derecha significa menor o mayor valor de la variable horizontal, respectivamente."),
                    
                    # # div(style = "line-height: 1em;",
                    # uiOutput("texto_etiqueta_x")|> div(style = "margin-bottom: -10px;"),
                    # uiOutput("texto_etiqueta_y") |> div(style = "margin-bottom: -10px;"),
                    # uiOutput("texto_etiqueta_size")|> div(style = "margin-bottom: -10px;")
                    # 
             ),
             column(12, align = "center", style = "padding: 0px;",
                    plotOutput("grafico", width = "100%", height = 720) |> 
                      withSpinner(color = color_destacado, type = 8)
             )
           ),
           fluidRow(
             column(12, align = "right", 
                    style = "margin-top: 24px;",
                    actionButton("azar", "Elegir todas las variables al azar", style = "padding-left: 24px; padding-right: 24px;")
             )
           ),
           
           #contexto ----
           fluidRow(
             column(12, style = "margin-top: 24px !important;", 
             h3("Contexto de la variable horizontal"),
             h4(textOutput("titulo_variable_contexto"), style = "margin-top: -8px;"),
             p("Resultados de la variable sleeccionada, para las comunas seleccionadas, puestas en contexto de los resultados de", strong("todas", style = "text-decoration: underline;"), "las demás comunas del país, para entender cómo se ubican entre las posibles realidades nacionales."),
             ),
             column(12, align = "center", style = "padding: 0px;",
                    
                    plotOutput("grafico_dispersion_contexto", width = "100%", height = 300) |> 
                      withSpinner(color = color_destacado, type = 8)
             )
           ),
           
           
           #correlación ----
           fluidRow(
             column(12, style = "margin-top: 24px !important;", 
                    h3("Correlación entre variables"),
                    p("Gráfico que indica numéricamente qué tan correlacionadas están las variables, 
                      donde una correlación positiva significa que los valores de ambas variables aumentan o descienden juntos (por ejemplo, mientras más camino, más me canso), 
                      y una correlación negativa significa que las variables se mueven en direcciones opuestas (por ejemplo, tengo menos sed si tomo más agua)."),
                    ),
             column(12, align = "center", style = "padding: 0px;",
                    
                    plotOutput("grafico_correlacion", height = 460) |> 
                      withSpinner(color = color_destacado, type = 8)
             )
           )
    )
  ),
  
  # firma ----
  fluidRow(
    column(12,
           hr(),
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           
           p("Puedes explorar mis otras",
             tags$a("aplicaciones interactivas sobre datos sociales aquí.",
                    href = "https://bastianolea.github.io/shiny_apps/", target = "_blank")
           ),
           
           p("Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/casen_relacionador")
           ),
           div(style = "height: 40px")
           
    )
  )
  
)


#—----

server <- function(input, output, session) {
  
  
  
  #selectores ----
  
  updatePickerInput(session,
                    inputId = "selector_regiones",
                    choices = c("Todas las regiones", unique(casen_comunas$region)),
                    selected = "Región Metropolitana de Santiago",
                    options = list(width = FALSE,
                                   noneSelectedText = "Sin selección")
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
  
  #aplicar comunas al selector de comunas
  observeEvent(input$selector_regiones, {
    req(length(input$selector_regiones) > 0)
    
    updatePickerInput(session,
                      inputId = "selector_comunas",
                      selected = c("La Florida", "La Pintana", "Ñuñoa", "Vitacura", "Providencia"),
                      choices = lista_comunas(),
                      options = list(`live-search` = TRUE,
                                     width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  
  # para que al apretar algún botón "al azar" no salgan variables ya elegidas
  variables_en_uso <- reactive(c(input$selector_y, input$selector_x, input$selector_size))
  variables_numericas_sin_en_uso <- reactive(variables_numericas[!(variables_numericas %in% variables_en_uso())])
  
  ## variables al azar ----
  
  #todas al azar
  observeEvent(input$azar,{
    updateSelectizeInput(session, inputId = "selector_y",
                         selected = sample(variables_numericas, 1),
                         options = list(width = FALSE))
    updateSelectizeInput(session, inputId = "selector_x",
                         selected = sample(variables_numericas, 1),
                         options = list(width = FALSE))
    updateSelectizeInput(session, inputId = "selector_size",
                         selected = sample(variables_numericas, 1),
                         options = list(width = FALSE))
  })
  
  #azar y
  observeEvent(input$azar_y, {
    updateSelectizeInput(session, inputId = "selector_y",
                         selected = sample(variables_numericas_sin_en_uso(), 1),
                         options = list(width = FALSE))
  })
  
  #variable x aleatoria al iniciar
  updateSelectizeInput(session, inputId = "selector_x",
                       selected = sample(variables_numericas, 1),
                       options = list(width = FALSE))
  
  #azar x
  observeEvent(input$azar_x, {
    updateSelectizeInput(session, inputId = "selector_x",
                         selected = sample(variables_numericas_sin_en_uso(), 1),
                         options = list(width = FALSE))
  })
  
  #azar tamaño
  observeEvent(input$azar_size, {
    updateSelectizeInput(session, inputId = "selector_size",
                         selected = sample(variables_numericas, 1),
                         options = list(width = FALSE))
  })
  
  #azar comunas
  observeEvent(input$azar_comunas, {
    azar_comunas <- sample(unlist(lista_comunas()), 5)
    
    updatePickerInput(session,
                      inputId = "selector_comunas",
                      selected = azar_comunas,
                      options = list(`live-search` = TRUE,
                                     width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  #azar comunas al cambiar región
  observeEvent(input$selector_regiones, {
    azar_comunas <- sample(unlist(lista_comunas()), 5)
    updatePickerInput(session,
                      inputId = "selector_comunas",
                      selected = azar_comunas,
                      options = list(`live-search` = TRUE,
                                     width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  
  # etiquetas ----
  etiqueta_y <- reactive({
    nombre_variable(input$selector_y)
  })
  etiqueta_x  <- reactive({
    nombre <- nombre_variable(input$selector_x)
    if (str_detect(input$selector_x, "_p$")) {
      nombre <- paste(nombre, "(porcentaje)")
    } else if (input$selector_x %in% variables[["Variables de personas"]] |
               input$selector_x %in% variables[["Variables de hogares"]]) {
      nombre <- paste(nombre, "(promedio)")
    } 
  })
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
  
  # datos ----
  datos <- reactive({
    
    req(length(input$selector_comunas) > 0)
    
    
    cat(input$selector_comunas)
    
    message("comunas: ", comunas())
    message("variable x: ", input$selector_x)
    message("variable y: ", input$selector_y)
    message("variable size: ", input$selector_size)
    message(">")
    
    datos <- casen_comunas %>%
      #filtrar comunas
      filter(comuna %in% input$selector_comunas) |> 
      select(comuna, region,
             any_of(c(input$selector_x,
                      input$selector_y,
                      input$selector_size)))
    datos
  })
  
  
  datos_dispersion <- reactive({
    req(length(input$selector_comunas) > 0)
    # browser()
    
    # #filtrar por hogares si la variable lo requiere
    # if (input$selector_x %in% unname(variables_numericas_hogar)) {
    #   dato1 <- casen_comunas |> 
    #     filter(pco1 == "1. Jefatura de Hogar")
    # } else {
    #   dato1 <- casen_comunas
    # }
    
    dato2 <- casen_comunas |> 
      #crear variable con los datos elegidos
      mutate(variable = !!sym(input$selector_x)) |> 
      select(comuna, variable)
    
    dato3 <- dato2 |> 
      # filter(comuna %in% .comunas) |> 
      mutate(comuna_seleccionada = ifelse(comuna %in% input$selector_comunas, TRUE, FALSE)) |>
      # mutate(variable = !!sym(.variable)) |>
      group_by(comuna, comuna_seleccionada) |> 
      summarize(variable = mean(variable, na.rm = TRUE), .groups = "drop")
    return(dato3)
  })
  
  
  
  #gráfico dispersión ----
  output$grafico <- renderPlot({
    # browser()
    # dev.new()
    
    grafico <- datos() |> 
      ggplot(aes(x = .data[[input$selector_x]],
                 y = .data[[input$selector_y]],
                 col = comuna,
                 size = .data[[input$selector_size]])) +
      # geom_smooth(col = color_secundario, linewidth = 2, method = "lm", alpha = 0, show.legend = F) +
      stat_smooth(method = "lm",
                  linewidth = 1.5, col = color_destacado, linetype = "dashed",
                  se = FALSE, fullrange = T, show.legend = F) +
      geom_point(alpha = 0.7) +
      ggrepel::geom_text_repel(aes(label = comuna),
                               size = 4.5, alpha = 0.7,
                               point.padding = 25, min.segment.length = 2, show.legend = FALSE) +
      ### escalas ----
    scale_size_continuous(range = c(6, 16), breaks = scales::extended_breaks(n = 3),
                          labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_color_brewer(palette = "Set2") +
      coord_cartesian(clip = "off") +
      ### tema ----
    theme_light(base_size = 18) +
      theme(legend.position = "bottom",
            legend.direction = "vertical") +
      theme(axis.line = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
            axis.ticks = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color = color_detalle),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(color = color_texto, size = 13),
            axis.text.y = element_text(margin = margin(l = 6)),
            legend.text = element_text(color = color_texto, size = 13, margin = margin(t= 4, b = 4)),
            legend.title = element_text(color = color_destacado, face = "bold", size = 17),
            legend.margin = margin(t = 10),
            axis.title = element_text(color = color_destacado, face = "bold", size = 17),
            axis.title.x = element_text(margin = margin(t = 8))
      ) +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      guides(size = guide_legend(override.aes = list(color = color_detalle), 
                                 direction = "vertical"),
             col = guide_legend(override.aes = list(size = 6), ncol = 1)) +
      # #etiquetas
      labs(y = etiqueta_y(),
           x = etiqueta_x(),
           col = "Comunas",
           size = etiqueta_size() |> str_wrap(25)
      )
    
    #escalas de porcentajes
    if (str_detect(input$selector_x, "_p$")) {
      grafico <- grafico +
        scale_x_continuous(labels = ~scales::percent(.x, accuracy = 1))
    }
    if (str_detect(input$selector_y, "_p$")) {
      grafico <- grafico +
        scale_y_continuous(labels = ~scales::percent(.x, accuracy = 1))
    }
    if (str_detect(input$selector_size, "_p$")) {
      grafico <- grafico +
        scale_size_continuous(range = c(5, 17), 
                              breaks = scales::extended_breaks(n = 3),
                              labels = ~scales::percent(.x, accuracy = 1))
    }
    
    # browser()
    return(grafico)
  })
  
  
  
  #gráfico dispersión contexto ----
  
  output$titulo_variable_contexto <- renderText(nombre_variable(input$selector_x))
  
  output$grafico_dispersion_contexto <- renderPlot({
    req(datos_dispersion())
    # browser()
    # datos_dispersion() |> count(comuna_seleccionada)
    
    grafico <- datos_dispersion() |> 
      ggplot(aes(x = variable, y = 1,
                 # fill = comuna_seleccionada, color = comuna_seleccionada, 
                 size = comuna_seleccionada, alpha = comuna_seleccionada)) +
      geom_jitter(data = datos_dispersion() |> filter(!comuna_seleccionada), 
                  color = color_secundario, width = 0, height = 1) +
      #línea vertical de promedio
      # geom_vline(xintercept = mean(datos_dispersion()$variable), 
      #            linewidth = 1.2, linetype = "solid", 
      #            color = color_fondo) +
      #sombra de puntos de comuna
      geom_point(data = datos_dispersion() |> filter(comuna_seleccionada), 
                 aes(color = comuna),
                 size = 11, color = color_fondo, alpha = 0.6) +
      #puntos de comuna
      geom_point(data = datos_dispersion() |> filter(comuna_seleccionada), 
                 aes(color = comuna), alpha = 0.7) +
      #tamaño de puntos destacados
      scale_size_manual(values = c(5, 12), guide = "none") +
      #transparencia de puntos destacados
      scale_alpha_manual(values = c("TRUE" = 0.7, "FALSE" = 0.2), guide = "none") +
      scale_y_continuous(limits = c(0, 2)) +
      scale_color_brewer(palette = "Set2") +
      # scale_x_continuous(label = ~scales::percent(.x),
      #                    expand = expansion(0.08)) +
      scale_x_continuous(labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      #otros
      theme(#axis.line.x = element_line(linewidth = 2, color = color_detalle, lineend = "round"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.5, color = color_detalle),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(color = color_texto, size = 13),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5)),
        legend.text = element_text(color = color_texto, size = 13, margin = margin(t= 4, b = 4, r = 14)),
        # legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank()
      ) +
      theme(legend.position = "none")
    
    #escalas de porcentajes
    if (str_detect(input$selector_x, "_p$")) {
      grafico <- grafico +
        scale_x_continuous(labels = ~scales::percent(.x, accuracy = 1))
    }
    plot(grafico)
  })
  
  
  #gráfico correlación ----
  output$grafico_correlacion <- renderPlot({
    req(datos())
    # browser()
    # dev.new()
    
    datos_correlacion <- datos() |>
      select(where(is.numeric)) |>
      cor() |> 
      as.data.frame() |>
      tibble::rownames_to_column(var = "fila") |> 
      tidyr::pivot_longer(cols = where(is.numeric), names_to = "columna", values_to = "correlacion") |> 
      mutate(diagonal = ifelse(fila == columna, TRUE, FALSE))
    
    datos_correlacion_2 <- datos_correlacion |> 
      rowwise() |> 
      mutate(fila = nombre_variable(fila),
             columna = nombre_variable(columna)) |> 
      mutate(fila = str_wrap(fila, 15),
             columna = str_wrap(columna, 15))
    
    datos_correlacion_2 |> 
      ggplot(aes(x = columna, y = fila, fill = correlacion, alpha = diagonal)) +
      geom_tile(linewidth = 1, color = color_fondo) +
      geom_text(aes(label = scales::percent(correlacion, accuracy = 1.1) |> 
                      str_remove("\\.0") |> 
                      str_replace("100\\.1%", "100%")), 
                color = color_texto, size = 3, fontface = "bold") +
      scale_fill_gradient2(low = color_destacado, mid = color_fondo, high = color_destacado, 
                           limits = c(-1, 1), labels = ~scales::percent(.x)) +
      scale_alpha_manual(values = c("TRUE" = 0, "FALSE" = 1)) +
      theme(legend.key.height = unit(1.5, "cm"), legend.key.width = unit(3, "mm")) +
      coord_fixed(ratio = 1/1, expand = FALSE) +
      theme(text = element_text(color = color_texto),
            axis.text = element_text(color = color_texto),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = .5),
            axis.title = element_blank(), legend.title = element_blank()) +
      #fondo
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.key = element_rect(fill = color_fondo)) +
      guides(alpha = "none") +
      guides(fill = guide_colourbar(ticks = FALSE))
    
    # browser()
  }, res = 100, bg = color_fondo)
}


shinyApp(ui = ui, server = server)