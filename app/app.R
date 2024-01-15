library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(stringr)

options(scipen=999)


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

#—----
#interfaz ----
ui <- fluidPage(
  title = "Relacionador Casen", 
  lang = "es",
  
  # header ----
  fluidRow(  
    column(12,
           h1("Relacionador de datos Casen 2022"),
           p("Este visualizador le permite analizar la relación entre múltiples datos socioeconómicos de las comunas del país, en base a los datos de la",
             tags$a("Encuesta de caracterización socioeconómica nacional (Casen) 2022", target = "_blank", href = "https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022"),
             "El gráfico expresa cómo se posicionan las comunas entre dos ejes que pueden representar ingresos, condiciones de vida, o situaciones de vulnerabilidad,
             expresando así la relación entre las desigualdades y condiciones de vida del país."),
           
    )
  ),
  
  #selectores ----
  fluidRow(
    column(12,
           h3("Seleccionar variables"),
           p("Seleccione variables socioeconómicas de su interés para relacionarlas unas con otras, o bien, elija variables al azar usando los botones para descubrir nuevas relaciones.")
    ),
    #eje x
    column(4, style = "margin-top: 24px;",
           selectizeInput("selector_x",
                          label = "Variable para el eje horizontal (X)",
                          width = "100%",
                          choices = variables,
                          options = list( `live-search` = TRUE)
           ),
           actionButton("azar_x", "Elegir eje X al azar")
    ),
    #eje y
    column(4, style = "margin-top: 24px;",
           selectizeInput("selector_y",
                          label = "Variable para el eje vertical (Y)",
                          width = "100%",
                          choices = variables,
                          selected = "ytotcorh",
                          options = list( `live-search` = TRUE)
           ),
           actionButton("azar_y", "Elegir eje Y al azar")
    ),
    
    #tamaño
    column(4, style = "margin-top: 24px;",
           selectizeInput("selector_size",
                          label = "Variable para el tamaño",
                          width = "100%",
                          choices = variables,
                          selected = "poblacion",
                          options = list( `live-search` = TRUE)
           ),
           actionButton("azar_size", "Elegir tamaño al azar")
    )
  ),
  
  fluidRow(
    column(12, align = "center", 
           style = "margin-top: 24px; margin-bottom: 24px;",
           actionButton("azar", "Elegir todas las variables al azar")
    )
  ),
  
  #comunas
  fluidRow(
    column(12,
           h3("Seleccionar territorios"),
           p("Elija una o más regiones para posteriormente elegir una o varias comunas que serán incluidas en el gráfico.")
    ),
    column(6,
           pickerInput("selector_regiones",
                       label = "Regiones",
                       width = "100%",
                       multiple = TRUE,
                       selected = "Región Metropolitana de Santiago", 
                       choices = NULL,
                       options = list( `live-search` = TRUE)
           )
           # em("Seleccione una o más regiones para filtrar el selector de comunas a continuación")
    ),
    column(6,     
           pickerInput("selector_comunas",
                       label = "Comunas que desea graficar",
                       width = "100%",
                       multiple = TRUE,
                       choices = NULL,
                       selected = c("La Florida", "Puente Alto", "La Pintana", "Ñuñoa", "Vitacura", "Providencia", "Lo Barnechea"),
                       options = list( `live-search` = TRUE)
           ),
           
           actionButton("azar_comunas", "Elegir comunas al azar")
    )
    
  ),
  
  
  #grafico ----
  fluidRow(
    column(12,
           h3("Visualizar")
    ),
    column(12, align = "center", style = "padding: 24px;",
           plotOutput("grafico", width = 600, height = 500)
    )
  ),
  fluidRow(
    column(12,
           hr(),
           p("Diseñado y programado por",
             tags$a("Bastián Olea.", target = "_blank", href = "https://bastian.olea.biz"),
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
  
  
  # calcular ----
  
  datos <- reactive({
    req(length(input$selector_comunas) > 0)
    
    message("comunas: ", input$selector_comunas |> paste(collapse = ", "))
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
      theme(axis.line = element_line(linewidth = 1, color = "gray40", lineend = "round"),
            axis.ticks = element_blank(),
            panel.grid.major = element_line(linewidth = 0.5, color="gray90"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(color= "gray60", size=13),
            legend.text = element_text(color= "gray60", size = 13),
            legend.title = element_text(color= "gray60", face = "bold", size = 14),
            axis.title = element_text(color= "gray60", face = "bold", size = 14)) +
      guides(size = guide_legend(override.aes = list(color = "gray60")),
             col = guide_legend(override.aes = list(size = 6))) +
      # #etiquetas
      labs(y = nombre_variable(input$selector_y),
           x = nombre_variable(input$selector_x),
           col = "Comunas",
           size = nombre_variable(input$selector_size) |> str_wrap(25)
      )
    # browser()
    
    # nombre_variable(input$selector_y)
    
    return(p)
    
  })
  
  
  
  
}


shinyApp(ui = ui, server = server)
