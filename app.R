library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
options(scipen=999)


# diccionario de variables ----
source("variables.R")


ui <- fluidPage(
  
  #selectores ----
  fluidRow(
    #eje x
    column(12,
                  selectizeInput("selector_x",
                                 label = "Variable para el eje horizontal",
                                 width = "100%",
                                 choices = variables
                  ),
                  actionButton("azar_x", "Elegir al azar")
    ),
    #eje y
    column(12,
                  selectizeInput("selector_y",
                                 label = "Variable para el eje vertical",
                                 width = "100%",
                                 choices = variables
                  ),
                  actionButton("azar_y", "Elegir al azar")
    ),
    
    #tamaño
    column(12,
                  selectizeInput("selector_size",
                                 label = "Variable para el tamaño",
                                 width = "100%",
                                 choices = variables,
                                 selected = "poblacion"
                  ),
                  actionButton("azar_size", "Elegir al azar")
    )
  ),
    
  fluidRow(
    column(12,
           actionButton("azar", "Elegir todas al azar")
    )
  ),
    
    #comunas
  fluidRow(
    column(12,
                  pickerInput("selector_comunas",
                              label = "Comunas que desea graficar",
                              width = "100%",
                              multiple = TRUE,
                              choices = NULL
                  )
           )
           
  ),
  
  
  #grafico ----
  fluidRow(
    column(12,
           plotOutput("grafico", width = 600, height = 500)
    )
  )
)


server <- function(input, output, session) {
  
  #cargar datos ----
  casen_comunas <- readr::read_csv2("casen_comunas.csv")
  
  variables_numericas <- casen_comunas |> select(where(is.numeric)) |> select(-cut_comuna) |> names()
  
  #selectores ----
  
  #pone las comunas en el selector de comunas
  updatePickerInput(session,
                    inputId = "selector_comunas",
                    choices = casen_comunas$comuna,
                    selected = c("La Florida", "Puente Alto", "La Pintana", "Cerrillos", "Ñuñoa", "Vitacura", "Providencia", "Lo Barnechea")
  )
  
  observeEvent(input$azar,
               {
                 updateSelectizeInput(session, inputId = "selector_y",
                                      selected = sample(variables_numericas, 1))
                 updateSelectizeInput(session, inputId = "selector_x",
                                      selected = sample(variables_numericas, 1))
                 updateSelectizeInput(session, inputId = "selector_size",
                                      selected = sample(variables_numericas, 1))
               })
  
  observeEvent(input$azar_y, {
                 updateSelectizeInput(session, inputId = "selector_y",
                                      selected = sample(variables_numericas, 1))
               })
  
  observeEvent(input$azar_x, {
                 updateSelectizeInput(session, inputId = "selector_x",
                                      selected = sample(variables_numericas, 1))
               })
  
  observeEvent(input$azar_size, {
    updateSelectizeInput(session, inputId = "selector_size",
                         selected = sample(variables_numericas, 1))
  })
  
  # updateSelectizeInput(session,
  #                      inputId = "selector_y",
  #                      #choices = variables,
  #                      choices = variables_numericas,
  #                      selected = sample(variables_numericas, 1)
  # )
  # 
  # updateSelectizeInput(session,
  #                      inputId = "selector_x",
  #                      choices = variables_numericas,
  #                      selected = sample(variables_numericas, 1)
  # )
  # 
  # updateSelectizeInput(session,
  #                      inputId = "selector_size",
  #                      choices = variables_numericas,
  #                      selected = sample(variables_numericas, 1)
  # )
  
  
  
  
  
  #gráfico ----
  output$grafico <- renderPlot({
    
    #estética ----
    aes_mapping <- aes_string(
      x = input$selector_x,
      y = input$selector_y,
      col = "comuna",
      size = input$selector_size
    )
    
    #ggplot ----
    p <- casen_comunas %>%
      #filtrar comunas
      filter(comuna %in% input$selector_comunas) %>%
      #graficar
      ggplot(mapping = aes_mapping) +
      geom_point(col= "white") +
      geom_point(alpha=0.7) +
      ggrepel::geom_text_repel(aes(label = comuna),
                               size = 5, point.padding = 1,
                               min.segment.length = 2,
                               show.legend = FALSE) +
      #escalas
      scale_size(range = c(6, 20),
                 labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      coord_cartesian(clip = "off") +
      #tema
      theme_light(base_size = 18) +
      theme(legend.position = "right") +
      theme(axis.line = element_line(size = 1, color = "gray40", lineend = "round"),
            axis.ticks = element_blank(),
            panel.grid.major = element_line(size=0.5, color="gray80"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(color= "gray60", size=13),
            legend.text = element_text(color= "gray60", size = 13),
            legend.title = element_text(color= "gray60", face = "bold", size = 14),
            axis.title = element_text(color= "gray60", face = "bold", size = 14)) +
      guides(size = guide_legend(override.aes = list(color = "gray60")),
             col = guide_legend(override.aes = list(size = 6)))
      # #etiquetas
      # labs(y = names(variables)[variables == input$selector_y], #obtener del vector nombrado
      #      x = names(variables)[variables == input$selector_x],
      #      col = "Comunas",
      #      size = names(variables)[variables == input$selector_size]
      # )
    
    return(p)
    
  })
  
  
  
  
}


shinyApp(ui = ui, server = server)
