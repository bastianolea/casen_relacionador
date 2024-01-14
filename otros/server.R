shinyServer(function(input, output, session) {
  
  #texto de fechas ----
  output$fecha_maxima_nuevos <- renderText({
    paste(format(casos_comuna_nuevos() %>%
                   filter(!is.na(inicio_semana_epidemiologica)) %>%
                   filter(inicio_semana_epidemiologica != max(inicio_semana_epidemiologica)) %>%
                   filter(inicio_semana_epidemiologica == max(inicio_semana_epidemiologica)) %>%
                   select(fin_semana_epidemiologica) %>%
                   distinct() %>% pull(), "%d/%m/%Y"))
  })
  
  output$fecha_maxima_activos <- renderText({
    paste(format(max(casos_comuna_activos()$fecha), "%d/%m/%Y"))
  })
  
  output$fecha_maxima_incremental <- renderText({
    paste(format(max(casos_comuna_incremental()$fecha), "%d/%m/%Y"))
  })
  
  
  datos <- reactive({
    
    casen_datos_covid <- casen_datos %>%
      mutate(comuna_match = iconv(tolower(comuna), from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
      select(-c(comuna_match, fecha))
    
    # glimpse(casen_datos_covid)
    
    return(casen_datos_covid)
  })
  
  output$selector_y <- renderText({ input$selector_y })
  output$selector_x <- renderText({ input$selector_x })
  
  #actualizar selectores ----
  #pone la lista de variables en cada selector
  
  updateSelectizeInput(session,
                       inputId = "selector_y",
                       #choices = variables,
                       choices = variables,
                       selected = sample(variables, 1)
  )
  
  updateSelectizeInput(session,
                       inputId = "selector_x",
                       choices = variables,
                       selected = sample(variables, 1)
  )
  
  updateSelectizeInput(session,
                       inputId = "selector_size",
                       choices = variables,
                       selected = sample(variables, 1)
  )
  
  #pone las comunas en el selector de comunas
  updatePickerInput(session,
                    inputId = "selector_comunas",
                    choices = casen_datos %>% select(comuna) %>% distinct() %>% pull() %>% as.character(),
                    selected = c("La Florida", "Puente Alto", "La Cisterna", "Cerrillos", "Ñuñoa", "Vitacura", "Providencia")
  )
  
  #actualiza los selectores de tipo para poner "no aplica" en las variables que no tienen mediana (las de conteo)
  observeEvent(input$selector_x, {
    if(input$selector_x %in% variables_conteo) {
      updateSelectInput(session,
                        inputId = "tipo_elegido_x",
                        choices = c("No aplica" = "promedio"),
                        selected = c("No aplica" = "promedio"))
    }
  })
  
  observeEvent(input$selector_y, {
    if(input$selector_y %in% variables_conteo) {
      updateSelectInput(session,
                        inputId = "tipo_elegido_y",
                        choices = c("No aplica" = "promedio"),
                        selected = c("No aplica" = "promedio"))
    }
  })
  
  observeEvent(input$selector_size, {
    if(input$selector_size %in% variables_conteo) {
      updateSelectInput(session,
                        inputId = "tipo_elegido_size",
                        choices = c("No aplica" = "promedio"),
                        selected = c("No aplica" = "promedio"))
    }
  })
  
  #gráfico ----
  output$grafico <- renderPlot({
    
    req(datos())
    req(input$selector_y)
    
    #variables del gráfico ----
    variable_y = input$selector_y #"ytotcorh"
    variable_x = input$selector_x #"numper"
    variable_col = "comuna"
    variable_size = input$selector_size #"s4"
    
    comunas_elegidas <- input$selector_comunas#c("La Florida", "Puente Alto", "Vitacura")
    
    tipo_elegido_x = input$tipo_elegido_x #"promedio"
    tipo_elegido_y = input$tipo_elegido_y #"promedio"
    tipo_elegido_size = input$tipo_elegido_size #"mediana"
    
    #estética ----
    aes_mapping <- aes_string(
      x = variable_x,
      y = variable_y,
      col = variable_col,
      size = variable_size
    )
    
    #ggplot ----
    p <- datos() %>%
      #crear columnas de promedio/mediana
      tidyr::pivot_wider(names_from = tipo, values_from = 2:25) %>%
      #seleccionar variables y tipos elegidos
      select(comuna, 
             paste(variable_x, tipo_elegido_x, sep="_"), 
             paste(variable_y, tipo_elegido_y, sep="_"),
             #all_of(variable_col),
             paste(variable_size, tipo_elegido_size, sep="_")) %>%
      rename_all(list(~ stringr::str_remove(., "_promedio"))) %>%
      rename_all(list(~ stringr::str_remove(., "_mediana"))) %>%
      #filtrar comunas
      filter(comuna %in% comunas_elegidas) %>%
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
      #viridis::scale_color_viridis(discrete = TRUE, option = "magma") +
      #fishualize::scale_color_fish(option = "Oncorhynchus_tshawytscha", discrete = TRUE) +
      rcartocolor::scale_color_carto_d(type = "qualitative",
                                       #palette = "Sunset"
                                       #palette = "Vivid"
                                       palette = "Pastel"
      ) +
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
             col = guide_legend(override.aes = list(size = 6))) +
      #etiquetas
      labs(y = names(variables)[variables == variable_y], #obtener del vector nombrado
           x = names(variables)[variables == variable_x],
           col = "Comunas",
           size = names(variables)[variables == variable_size]
      )
    
    return(p)
    
  })
  
  
  # #tabla ----
  # 
  # output$tabla <- renderFormattable({
  #   req(input$selector_y)
  #   
  #   #variables del gráfico ----
  #   variable_y = input$selector_y #"ytotcorh"
  #   variable_x = input$selector_x #"numper"
  #   variable_col = "comuna"
  #   variable_size = input$selector_size #"s4"
  #   
  #   comunas_elegidas <- input$selector_comunas#c("La Florida", "Puente Alto", "Vitacura")
  #   
  #   tipo_elegido_x = input$tipo_elegido_x #"promedio"
  #   tipo_elegido_y = input$tipo_elegido_y #"promedio"
  #   tipo_elegido_size = input$tipo_elegido_size #"mediana"
  #   
  #   
  #   t <- casen_datos_covid() %>%
  #     #crear columnas de promedio/mediana
  #     tidyr::pivot_wider(names_from = tipo, values_from = 2:25) %>%
  #     #seleccionar variables y tipos elegidos
  #     select(comuna, 
  #            paste(variable_x, tipo_elegido_x, sep="_"), 
  #            paste(variable_y, tipo_elegido_y, sep="_"),
  #            all_of(variable_col),
  #            paste(variable_size, tipo_elegido_size, sep="_")) %>%
  #     rename_all(list(~ stringr::str_remove(., "_promedio"))) %>%
  #     rename_all(list(~ stringr::str_remove(., "_mediana"))) %>% 
  #     formattable()
  # 
  #   t
  # })
  
  
  # output$alerta_repetidas <- renderText({
  #     if(input$selector_x == input$selector_y) {
  #         alerta <- "Las variables seleccionadas no pueden repetirse"
  #     } else if (input$selector_x == input$selector_size) {
  #         alerta <- "Las variables seleccionadas no pueden repetirse"
  #     } else if (input$selector_y == input$selector_size) {
  #         alerta <- "Las variables seleccionadas no pueden repetirse"
  #     }  else {
  #         alerta <- ""
  #     }
  #     return(alerta)
  # })
  
  
}) #fin