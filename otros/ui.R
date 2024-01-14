shinyUI(fluidPage(
    
    titlePanel(title = NULL,
               windowTitle = "Relacionador Casen"),
    
    #tipografías
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Open Sans');")), # Importar Open Sans
    
    #título ----
    fluidRow(
        column(12,
               h1("Relacionador Casen COVID-19"),
               p("Esta herramienta permite relacionar datos socioeconómicos cuantitativos de la encuesta Casen 2017 con los datos más actualizados del COVID-19."),
               p("Utilice los botones presentados a continuación para seleccionar las variables que le interesa graficar."),
               
               HTML("<p>Los datos de COVID-19 son obtenidos desde la 
               <a href='https://github.com/datauc/api-covid19-datauc' 
                       style='color: #999999'>
                    API COVID-19</a> de DataUC. 
               Para más visualizaciones sobre el Coronavirus en Chile, visite el 
                    <a href='https://coronavirus.mat.uc.cl/' 
                       style='color: #999999'>
                    Visualizador COVID-19 Chile</a></p>"),
               
        #Por ejemplo, seleccionar 'Mujeres' y 'Pobreza extrema' resultará en la cantidad de población que corresponde a dicho grupo social, junto a otros datos tales como sus ingresos."),
               hr(),
        )
    ),
    
    #selectores ----
    fluidRow(
        #eje x
        column(12,
               column(8,
                      selectizeInput("selector_x",
                                     label = "Variable para el eje horizontal",
                                     width = "100%",
                                     choices = NULL
                                     #choices = c("primera variable" = "1",
                                     #               "segunda variable" = "2")
                      ),
               ),
               column(4,
                      selectizeInput("tipo_elegido_x",
                                     label = "Medida del dato del eje horizontal",
                                     width = "100%",
                                     choices = c("Promedios" = "promedio",
                                                 "Medianas" = "mediana")
                      )
               )
               
        ),
        #eje y
        column(12,
               column(8,
               selectizeInput("selector_y",
                              label = "Variable para el eje vertical",
                              width = "100%",
                              choices = NULL
                              #choices = c("primera variable" = "1",
                              #               "segunda variable" = "2")
               ),
               ),
               column(4,
               selectizeInput("tipo_elegido_y",
                              label = "Medida del dato del eje horizontal",
                              width = "100%",
                              choices = c("Promedios" = "promedio",
                                          "Medianas" = "mediana")
               )
               )
        ),
        
        #tamaño
        column(12,
               column(8,
               selectizeInput("selector_size",
                              label = "Variable para el tamaño",
                              width = "100%",
                              choices = NULL
                              #choices = c("primera variable" = "1",
                              #               "segunda variable" = "2")
               ),
               ),
               column(4,
               
               selectizeInput("tipo_elegido_size",
                              label = "Medida del tamaño",
                              width = "100%",
                              choices = c("Promedios" = "promedio",
                                          "Medianas" = "mediana")
               )
               )
               
        ),
        
        #comunas
        column(12,
               column(12,
               pickerInput("selector_comunas",
                           label = "Comunas que desea graficar",
                           width = "100%",
                           multiple = TRUE,
                           choices = NULL
               )
               )
               
        )
    ),

    #gráfico ----
    fluidRow(    
        column(12,
               br(),
               hr(),
               plotOutput("grafico", height = "640px") %>% shinycssloaders::withSpinner()
        ),
        column(12,
               hr(),
               div(style = "display: inline-block;", em("Casos nuevos actualizados al: ")),
               div(style = "display: inline-block;", textOutput("fecha_maxima_nuevos")),
               br(),
               
               div(style = "display: inline-block;", em("Casos activos actualizados al: ")),
               div(style = "display: inline-block;", textOutput("fecha_maxima_activos")),
               br(),
               
               div(style = "display: inline-block;", em("Casos incrementales actualizados al: ")),
               div(style = "display: inline-block;", textOutput("fecha_maxima_incremental"))
        )
               
    ),
    
    
    # #tabla ----
    # fluidRow(    
    #     column(12,
    #            br(),
    #            hr(),
    #            formattableOutput("tabla", height = "640px") %>% shinycssloaders::withSpinner()
    #     )
    # ),
    
    #footer ----
    fluidRow(
        column(12, align = "center",
               
               hr(),
               p("Plataforma desarrollada por el equipo DATA UC, usando R Shiny"),

               HTML("<p>Diseño y desarrollo: 
                    <a href='http://bastian.olea.biz' 
                       style='color: #999999'>
                    Bastián Olea Herrera.</a></p>"),
               
               p("Facultad de Matemáticas"),
               p("Pontificia Universidad Católica de Chile"),
               tags$a(img(
                   src = "logodatauc.png",
                   width = 200, style = "padding: 10px"
                   ),
               #href = "http://www.mat.uc.cl"
               hreg = "http://datascience.uc.cl"
               )
               
               
        )) #end footer
               
    
))
