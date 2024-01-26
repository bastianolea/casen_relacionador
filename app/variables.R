

variables <- list(
  "Viviendas" = c(
    #"Hogares con jefatura femenina" = "hogar_jefatura_femenina_p",
    "Hogares en situación de hacinamiento (4 personas o más)" = "hacinamiento_p",
    "Hogares con personas menores de 18 años" = "men18c_p",
    "Hogares con personas mayores de 60 años" = "may60c_p",
    "Viviendas propias" = "vivienda_propia_p",
    "Viviendas arrendadas" = "vivienda_arrendada_p",
    "Metros cuadrados aproximados de la vivienda" = "v12mt", #ojo con esta que es numérica
    "Viviendas pequeñas (30m cuadrados o menos)" = "vivienda_pequeña_p",
    "Hogares en sectores en mal estado" = "sector_malo_p",
    "Hogares en sectores con mucho daño deliberado a la propiedad" = "sector_dañado_p",
    "Hogares en zonas rurales" = "rural_p",
    "Viviendas en mal estado" = "carencia_estado_vivienda_p",
    "Viviendas carentes de servicios básicos" = "carencia_servicios_basicos_p",
    "Hogares con ingresos percapita menores a la mediana ($450.000)" = "ingreso_percapita_hogar_menor_mediana_p",
    "Hogares con ingresos totales menores a la mediana ($1.110.000)" = "ingreso_total_hogar_menor_mediana_p",
    "Hogares con ingresos producto del trabajo menores a la mediana ($700.000)" = "ingreso_trabajo_hogar_menor_mediana_p"
  ),
  
  #numéricas
  "Ingresos de las personas" = c(
    "Ingresos promedio" = "ytotcor",
    "Ingreso ocupación principal" = "yoprcor",
    "Años de escolaridad promedio" = "esc",
    "Edad promedio" = "edad",
    "Ingreso del trabajo" = "ytrabajocor",
    "Jubilación o pensión de vejez" = "y2803",
    "Decil autónomo regional" = "dautr",
    "Quintil autónomo regional" = "qautr"
  ),
  
  "Ingresos de la población" = c(
    "Ingresos personales por ocupación principal menores a la mediana ($500.000)" = "ingreso_ocup_princ_menor_mediana_p",
    "Ingresos personales por ocupación principal menores a $1.000.000" = "ingreso_ocup_princ_menor_2medianas_p",
    "Ingresos personales producto del trabajo menores a la mediana ($500.000)" = "ingreso_ytrabajocor_menor_mediana_p",
    "Ingresos personales producto del trabajo menores a $1.000.000" = "ingreso_ytrabajocor_menor_2medianas_p"
  ),
  
  #numéricas
  "Ingresos de los hogares" = c(
    "Ingresos promedio de los hogares" = "ytotcorh",
    "Ingreso del trabajo del hogar" = "ytrabajocorh",
    "Ingreso autónomo per cápita" = "ypchautcor",
    "Numero de personas en el hogar" = "numper",
    "Ingreso total per cápita del hogar" = "ypc"
  ),
  
  "Trabajo" = c(
    "Trabajadores independientes" = "independientes_p",
    "Personas en situación laboral inactiva" = "inactivos_p",
    "Personas en situación laboral desocupada" = "desocupados_p",
    "Trabaja como familiar no remunerado" = "trabajo_familiar_no_remunerado_p",
    "Trabaja en servicio doméstico puertas adentro o afuera" = "trabajo_servicio_domestico_p",
    "Trabaja como empleador" = "trabajo_empleador_p",
    "Trabajadores del hogar o servicio doméstico" = "trabajo_domestico_p"
  ),
  
  "Pensiones" = c(
    "Jubilación o pensión menor o igual a la mediana ($230.000)" = "pension_menor_mediana_p",
    "Jubilación o pensión menor o igual al salario mínimo ($500.000)" = "pension_menor_salario_minimo_p"
  ),
  
  "Educación" = c(
    "Personas con estudios superiores (técnico o profesional)" = "estudios_superiores_p",
    "Nivel educacional máximo: básica o menor" = "estudios_basica_o_menos_p",
    "Nivel educacional máximo: media incompleta o menor" = "estudios_sin_media_o_menos_p",
    "Nivel educacional máximo: estudios superiores incompletos" = "estudios_superiores_incompletos_p"
  ),
  
  "Pobreza" = c(
    "Personas en situación de pobreza" = "pobreza_p",
    "Personas en situación de pobreza multidimensional" = "pobreza_multi_p"
  ),
  
  "Demografía" = c(
    "Personas pertenecientes a pueblos originarios" = "originario_p",
    "Personas de origen extranjero" = "extranjero_p"
  ),
  
  "Salud" = c(
    "Afiliados a previsión de salud Fonasa" = "fonasa_p",
    "Afiliados a previsión de salud Isapre" = "isapre_p"
  )
)




# 
# # seleccionar variables casen ----
# 
# # variables que serán filtradas de la base casen para su procesamiento
# variables_casen <- c(
#   "comuna",
#   "cut_comuna",
#   "region",
#   "area",
#   "expc",                    #factor de expansión comunal
#   "expr",                    #factor de expansión regional
#   "sexo",                    #género
#   "esc",                     #años de escolaridad
#   "edad",                    #edad
#   # "pco1",
#   
#   #económicas y de salario
#   "ytotcorh",                #Ingreso total del hogar corregido
#   "ytotcor",                 #Ingreso total corregido
#   "yoprcor",                 #Ingreso ocupación principal
#   "ypc",                     #Ingreso total per cápita del hogar corregido
#   "ytrabajocor",             #ingreso del trabajo
#   "ytrabajocorh",            #ingreso del trabajo del hogar
#   "ypchautcor",              #ingreso autónomo per cápita 
#   # "y26_2c",                  #jubilación o pensión
#   # "y28_2c",                  #y28_2c. Monto Jubilación o Pensión de Vejez
#   # "y28_2b1",   #y28_2b1. Monto Jubilación o Pensión de Vejez sin APS
#   "y2803",                  #"Jubilación o pensión de vejez"
#   "y0101",                   #Asalariados principal - Sueldos y salarios monetario
#   "yosa",                    #Ingresos de la ocupación secundaria - Asalariados
#   "ytot",                    #Ingreso total
#   "dau",                     #Decil autónomo nacional
#   "qaut",                    #Quintil autónomo nacional
#   "dautr",                   #Decil autónomo regional
#   "qautr",                   #Quintil autónomo regional
#   
#   #social
#   "pobreza",                 #pobreza
#   "pobreza_multi_5d",        #pobreza multidimensional
#   "r1a",                     #nacionalidad
#   "r3",                      #pertenencia a pueblos originarios
#   
#   #laboral
#   "activ",                   #actividad
#   
#   #vivienda
#   "numper",                  #numero de personas en el hogar
#   # "s4",                      #hijos vivos
#   "pco1",                    #jefe de hogar
#   "v12",                     #metros cuadrados de la casa
#   "indmat",                  #índice de materialidad de la vivienda
#   "v12mt",
#   
#   
#   "p2",                      #Indique estado de edificios y casas del sector
#   "p3",                      #Presencia de basura en el sector
#   "p4",                      #Vandalismo, grafiti o daño deliberado a la propiedad en el sector
#   "p9",                      #p9. Incluyéndose a Ud., ¿cuántas personas viven habitualmente en la vivienda?
#   
#   
#   "contrato",                #Tiene contrato de trabajo
#   "v13",                     #v13. Su hogar, ¿bajo qué situación ocupa la vivienda?
#   "men18c",                  #Presencia de menores de 18 años en el hogar (excluye SDPA)
#   "may60c",                  #Presencia de mayores de 60 años en el hogar (excluye SDPA)
#   "educ",                    #Nivel de escolaridad
#   "s13",                     #s13. ¿A qué sistema previsional de salud pertenece?
#   "v1",                      #v1. ¿Cuál es el tipo de vivienda que ocupa la persona entrevistada?
#   "qaut",                    #Quintil autónomo nacional
#   "dautr",                   #Decil autónomo regional
#   "qautr"                    #Quintil autónomo regional
# )
# 
# 
# # seleccionar variables visualizador ----
# # variables con su etiqueta para usarse en la app de visualización
# 
# variables_numericas_personas <- c(
#   # "Ingreso total (promedio)" = "ytotcor",
#   "Ingresos promedio" = "ytotcor",
#   "Ingreso ocupación principal" = "yoprcor",
#   "Años de escolaridad promedio" = "esc",
#   "Edad promedio" = "edad",
#   "Ingreso del trabajo" = "ytrabajocor",
#   # "Jubilación o pensión de vejez (sin APS)" = "y28_2b1",
#   "Jubilación o pensión de vejez" = "y2803",
#   # "Jubilación o pensión de vejez (promedio)" = "y28_2c",
#   # "Número de hijos vivos" = "s4",
#   "Decil autónomo regional" = "dautr",
#   "Quintil autónomo regional" = "qautr"
# )
# 
# variables_numericas_hogar <- c(
#   "Ingresos promedio de los hogares" = "ytotcorh", # "Ingreso total del hogar (promedio)" = "ytotcorh",
#   "Ingreso del trabajo del hogar" = "ytrabajocorh",
#   "Ingreso autónomo per cápita " = "ypchautcor",
#   "Numero de personas en el hogar" = "numper",
#   "Ingreso total per cápita del hogar" = "ypc",
#   "Metros cuadrados aproximados de la vivienda" = "v12mt")
# 
# variables <- list(
#   "Variables de personas" = variables_numericas_personas,
#   
#   "Variables de hogares" = c(variables_numericas_hogar,
#                              "Cantidad de hogares" = "hogares",
#                              # "Hogares en zonas rurales" = "rural",
#                              # "Hogares con menores de 18 años" = "men18c",
#                              # "Hogares con mayores de 60 años" = "may60c",
#                              # "Cantidad de viviendas propias" = "vivienda_propia",
#                              # "Cantidad de viviendas pequeñas (30m cuadrados o menos)" = "vivienda_pequeña",
#                              # "Hogares con jefatura femenina" = "hogar_jefatura_femenina",
#                              # "Hogares hacinados (4 personas o más)" = "hacinamiento",  
#                              "Hogares con menores de 18 años" = "men18c_p",
#                              "Hogares con mayores de 60 años" = "may60c_p",
#                              "Hogares con jefatura femenina" = "hogar_jefatura_femenina_p",
#                              "Hogares en situación de hacinamiento (4 personas o más)" = "hacinamiento_p",
#                              "Viviendas propias" = "vivienda_propia_p",
#                              "Viviendas arrendadas" = "vivienda_arrendada_p",
#                              "Viviendas pequeñas (30m cuadrados o menos)" = "vivienda_pequeña_p",
#                              "Hogares en sectores en mal estado" = "sector_malo_p",
#                              "Hogares en sectores con mucho daño deliberado a la propiedad" = "sector_dañado_p",
#                              "Hogares en zonas rurales" = "rural_p",
#                              "Viviendas en mal estado" = "carencia_estado_vivienda_p",
#                              "Viviendas carentes de servicios básicos" = "carencia_servicios_basicos_p",
#                              #
#                              "Ingresos del hogar percapita menores a la mediana ($450.000)" = "ingreso_percapita_hogar_menor_mediana_p",
#                              "Ingresos del hogar totales menores a la mediana ($1.110.000)" = "ingreso_total_hogar_menor_mediana_p",
#                              "Ingresos del hogar producto del trabajo menores a la mediana ($700.000)" = "ingreso_trabajo_hogar_menor_mediana_p"
#   ),
#   
#   #conteos
#   "Variables poblacionales" = c(
#     "Población" = "poblacion",
#     # "Personas en situación de pobreza" = "pobreza",
#     # "Personas en situación de pobreza multidimensional" = "pobreza_multi",
#     # "Personas pertenecientes a pueblos originarios" = "originario",
#     # "Personas de nacionalidad extranjera" = "extranjero",
#     # "Personas en situación de inactividad laboral" = "inactivos",
#     # "Personas en situación de desocupación laboral" = "desocupados",
#     # "Afiliados a previsión de salud Fonasa" = "fonasa",
#     # "Afiliados a previsión de salud Isapre" = "isapre",
#     # "Personas con estudios superiores (técnico o profesional)" = "estudios_superiores",
#     #
#     "Ingresos personales por ocupación principal menores a la mediana ($500.000)" = "ingreso_ocup_princ_menor_mediana_p",
#     "Ingresos personales por ocupación principal menores a $1.000.000" = "ingreso_ocup_princ_menor_2medianas_p",
#     "Ingresos personales producto del trabajo menores a la mediana ($500.000)" = "ingreso_ytrabajocor_menor_mediana_p",
#     "Ingresos personales producto del trabajo menores a $1.000.000" = "ingreso_ytrabajocor_menor_2medianas_p",
#     #
#     "Personas en situación de pobreza" = "pobreza_p",
#     "Personas en situación de pobreza multidimensional" = "pobreza_multi_p",
#     "Personas pertenecientes a pueblos originarios" = "originario_p",
#     "Personas de origen extranjero" = "extranjero_p",
#     "Personas en situación laboral inactiva" = "inactivos_p",
#     "Personas en situación laboral desocupada" = "desocupados_p",
#     "Afiliados a previsión de salud Fonasa" = "fonasa_p",
#     "Afiliados a previsión de salud Isapre" = "isapre_p",
#     "Personas con estudios superiores (técnico o profesional)" = "estudios_superiores_p"
#   )
# )
# 
# # tabla_variables <- variables |> tibble::enframe(name = "grupo", value = "variable") |> tidyr::unnest(variable, )
# # 
# # variables |> purrr::list_flatten()
