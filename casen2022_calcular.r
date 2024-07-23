#en este script se cargan los datos de la casen preprocesados en casen2022_procesar.r, 
#y se seleccionan las variables a utilizar en el visualizador, calculando las medidas 
#necesarias para optimizar el rendimiento en vivo de la app.

library(dplyr)
library(purrr)
library(stringr)

#cargar datos de casen2022_procesar.r
casen2022_2 <- arrow::read_parquet("datos/casen2022.parquet")

#ejecutar script con listas de variables relevantes
source("app/variables.R")

# lista de variables de la casen a considerar
variables_casen <- c(
  "comuna",
  "cut_comuna",
  "region",
  "area",
  "expc",                    #factor de expansión comunal
  "expr",                    #factor de expansión regional
  
  "pco1",                    #jefe de hogar
  
  "sexo",                    #género
  "esc",                     #años de escolaridad
  "edad",                    #edad
  "educ",                    #Nivel de escolaridad
  "s13",                     #s13. ¿A qué sistema previsional de salud pertenece?
  
  #económicas y de salario
  "ytotcorh",                #Ingreso total del hogar corregido
  "ytotcor",                 #Ingreso total corregido
  "yoprcor",                 #Ingreso ocupación principal
  "ypc",                     #Ingreso total per cápita del hogar corregido
  "ytrabajocor",             #ingreso del trabajo
  "ytrabajocorh",            #ingreso del trabajo del hogar
  "y2803",                   #Jubilación o pensión de vejez
  "y0101",                   #Asalariados principal - Sueldos y salarios monetario
  "ytot",                    #Ingreso total
  "dau",                     #Decil autónomo nacional
  "qaut",                    #Quintil autónomo nacional
  "dautr",                   #Decil autónomo regional
  "qautr",                   #Quintil autónomo regional
  
  #social
  "pobreza",                 #pobreza
  "pobreza_multi_5d",        #pobreza multidimensional
  "r1a",                     #nacionalidad
  "r3",                      #pertenencia a pueblos originarios
  
  #laboral
  "activ",                   #actividad
  "contrato",                #Tiene contrato de trabajo
  
  #vivienda
  "numper",                  #numero de personas en el hogar
  "v12mt",                   #metros cuadrados
  "v12",                     #metros cuadrados
  "p9",                      #p9. Incluyéndose a Ud., ¿cuántas personas viven habitualmente en la vivienda?
  # "indmat", (no está)                  #índice de materialidad de la vivienda
  "p2",                      #Indique estado de edificios y casas del sector
  "p3",                      #Presencia de basura en el sector
  "p4",                      #Vandalismo, grafiti o daño deliberado a la propiedad en el sector
  "men18c",                  #Presencia de menores de 18 años en el hogar (excluye SDPA)
  "may60c",                  #Presencia de mayores de 60 años en el hogar (excluye SDPA)
  "v1",                      #v1. ¿Cuál es el tipo de vivienda que ocupa la persona entrevistada?
  "v13",                     #v13. Su hogar, ¿bajo qué situación ocupa la vivienda?
  "hh_d_estado",             #	Hogar carente en estado de la vivienda
  "hh_d_servbas",            #	Hogar carente en servicios básicos
  "o15",                     #	o15. En su trabajo o negocio principal, ¿trabaja como?
  "rama1",
  "rama4"
)

variables_ingresos_personas <- c(
  "ytotcor",                 #Ingreso total corregido
  "yoprcor",                 #Ingreso ocupación principal
  "ytrabajocor",             #ingreso del trabajo
  "y2803",                   #Jubilación o pensión de vejez
  "y0101",                   #Asalariados principal - Sueldos y salarios monetario
  "ytot"                    #Ingreso total
)

variables_numericas_personas <- c(
  "esc",                     #años de escolaridad
  "edad",                    #edad
  "dau",                     #Decil autónomo nacional
  "qaut",                    #Quintil autónomo nacional
  "dautr",                   #Decil autónomo regional
  "qautr"                    #Quintil autónomo regional
)

variables_ingresos_hogar <- c(
  "ytotcorh",                #Ingreso total del hogar corregido
  "ytrabajocorh",            #ingreso del trabajo del hogar
  "ypchautcor",              #Ingreso autónomo per cápita
  "ypc"                     #Ingreso total per cápita del hogar corregido
)

variables_numericas_hogar <- c(
  "numper",                  #numero de personas en el hogar
  "v12mt",                   #metros cuadrados
  "p9"                       #p9. Incluyéndose a Ud., ¿cuántas personas viven habitualmente en la vivienda?
)

# filtrar variables y aplicar factor de expansión ----
casen2022_comunas <- casen2022_2 |> 
  # select(comuna, region, expc,
  #        any_of(variables |> unlist() |> unname())) |> 
  select(any_of(c(variables_casen, 
                  variables_ingresos_personas, variables_ingresos_hogar,
                  variables_numericas_personas, variables_numericas_hogar))) |> 
  tidyr::uncount(weights = expc) |> #factor de expansión
  mutate(nivel = "comuna")


# modificar variables ----
casen2022_comunas_2 <- casen2022_comunas |> 
  mutate(dautr = as.integer(dautr)) |> 
  mutate(qautr = as.integer(qautr)) |> 
  mutate(dau = as.integer(dau)) |> 
  mutate(qaut = as.integer(qaut)) |> 
  mutate(v12mt = readr::parse_integer(as.character(v12mt)))

# probar variables ----
# casen2022_comunas |> count(v12mt) |> print(n=Inf)
# casen2022_comunas |> 
#   filter(region == "Región Metropolitana de Santiago") |>
#   filter(comuna == "Providencia") |> 
#   select(comuna, area, v12mt) |> 
#   count(v12mt)
# casen2022_comunas_2 |> count(v12mt)
# casen2022_comunas_2 |> count(area)


# calcular numéricas ----

## numéricas de personas ----
casen2022_numericos_personas <- casen2022_comunas_2 |> 
  group_by(comuna, cut_comuna, region) |> 
  summarize(
    # promedio
    across(any_of(variables_numericas_personas),
           ~mean(.x, na.rm = TRUE)),
    # mediana
    across(any_of(variables_ingresos_personas),
           ~median(.x, na.rm = TRUE)), 
    .groups = "drop")

## numéricas de hogar ----
casen2022_numericos_hogar <- casen2022_comunas_2 |> 
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(comuna, cut_comuna, region) |> 
  summarize(
    # promedio
    across(any_of(variables_numericas_hogar),
           ~mean(.x, na.rm = TRUE)), 
    # mediana
    across(any_of(variables_ingresos_hogar),
           ~median(.x, na.rm = TRUE)), 
    .groups = "drop")


# calcular conteo ----

#para toda la población, por comunas, se evalúan expresiones, por ejemplo, si la persona pertenece a pueblos originarios, 
#y se obtiene el conteo de personas en la comuna que cumple esa característica,
#de modo que, en el siquiente paso, se transforme esa cifra de personas en un porcentaje comunal

## jefatura femenina ----
jefatura_femenina <- casen2022_comunas |> 
  group_by(comuna, cut_comuna, region, sexo) |> 
  count(pco1) |> 
  ungroup() |> 
  filter(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer") |> 
  select(comuna, cut_comuna, region, hogar_jefatura_femenina = n)


## variables por persona ----
casen2022_comunas_personas <- casen2022_comunas_2 %>%
  group_by(comuna, cut_comuna, region) %>%
  summarize(poblacion = n(),
            #demografía
            originario = sum(r3 != "11. No pertenece a ninguno de estos pueblos indígenas", na.rm = TRUE),
            extranjero = sum(r1a == "3. Otro país (extranjeros)", na.rm=TRUE),
            #pobreza
            pobreza = sum(pobreza == "Pobreza extrema" | pobreza == "Pobreza no extrema", na.rm = TRUE),
            pobreza_multi = sum(pobreza_multi_5d == "Pobreza", na.rm=TRUE),
            #trabajo
            inactivos = sum(activ == "Inactivos", na.rm=TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            independientes = sum(o15 == "2. Trabajador(a) por cuenta propia", na.rm=T),
            trabajo_familiar_no_remunerado = sum(o15 == "9. Familiar no remunerado", na.rm=T),
            trabajo_servicio_domestico = sum(o15 == "6. Servicio doméstico puertas adentro" | o15 == "7. Servicio doméstico puertas afuera", na.rm=T),
            trabajo_empleador = sum(o15 == "1. Patrón(a) o empleador(a)", na.rm=T),
            trabajo_domestico = sum(rama4 == "Actividades de los hogares como empleadores de personal doméstico", na.rm =T),
            #salud
            fonasa = sum(str_detect(s13, "FONASA"), na.rm=TRUE),
            isapre = sum(str_detect(s13, "Isapre"), na.rm=TRUE),
            #educación
            estudios_basica_o_menos = sum(edad >= 18 & educ %in% c("Básica completa", "Básica incompleta", "Sin educación formal", "No sabe"), na.rm=T), 
            estudios_sin_media_o_menos = sum(edad >= 18 & educ %in% c("Básica completa", "Básica incompleta", "Sin educación formal", "No sabe", "Media humanista incompleta", "Media técnica profesional incompleta"), na.rm=T), 
            estudios_superiores = sum(educ %in% c("Técnico nivel superior completo", "Profesional completo", "Posgrado incompleto", "Posgrado completo"), na.rm=T),
            estudios_superiores_incompletos = sum(educ %in% c("Técnico nivel superior incompleta", "Profesional incompleto", "Posgrado incompleto"), na.rm=T),
            #ingresos
            ingreso_ocup_princ_menor_mediana = sum(yoprcor <= 500000, na.rm=T), #ingresos totales
            ingreso_ocup_princ_menor_2medianas = sum(yoprcor <= 1000000, na.rm=T), #ingresos totales
            ingreso_ytrabajocor_menor_mediana = sum(ytrabajocor <= 500000, na.rm=T), #ingresos del trabajo
            ingreso_ytrabajocor_menor_2medianas = sum(ytrabajocor <= 1000000, na.rm=T), #ingresos del trabajo
            #pensiones
            pension_menor_mediana = sum(y2803 <= 230000, na.rm = T), #jubilación o pensión de vejez
            pension_menor_salario_minimo = sum(y2803 <= 500000, na.rm = T), #jubilación o pensión de vejez
            .groups = "drop"
  )

## variables por hogar ----
casen2022_comunas_hogares <- casen2022_comunas_2 %>%
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(comuna, cut_comuna, region) %>%
  summarize(hogares = n(),
            hacinamiento =  sum(p9 >= 4, na.rm=TRUE),
            men18c = sum(men18c == "Sí", na.rm=TRUE),
            rural = sum(area == "Rural", na.rm=TRUE),
            may60c = sum(may60c == "Sí", na.rm=TRUE),
            vivienda_propia = sum(v13 == "1. Propia", na.rm=TRUE),
            vivienda_arrendada = sum(v13 == "2. Arrendada", na.rm=TRUE),
            vivienda_pequeña = sum(v12 == "1. Menos de 30 m2", na.rm=TRUE),
            vivienda_mediana = sum(v12 %in% c("2. De 30 a 40 m2", "3. De 41 a 60 m2", "4. De 61 a 100 m2"), na.rm = TRUE),
            vivienda_grande = sum(v12 == "5. De 101 a 150 m2" | v12 == "6. Más de 150 m2", na.rm=TRUE),
            sector_malo = sum(p2 == "4. Malo" | p2 == "5. Muy malo", na.rm = T),
            sector_dañado = sum(p4 == "1. Mucho (observa 5 o más áreas con grafitis o daño deliberado)" | p4 == "2. Más o menos (observa 3 o 4 áreas con grafitis o daño deliberado)", na.rm=T),
            ingreso_percapita_hogar_menor_mediana = sum(ypc <= 450000, na.rm = T),
            ingreso_total_hogar_menor_mediana = sum(ytotcorh <= 1100000, na.rm = T),
            ingreso_trabajo_hogar_menor_mediana = sum(ytrabajocorh <= 700000, na.rm = T),
            carencia_estado_vivienda = sum(hh_d_estado == "Carente", na.rm = T),
            carencia_servicios_basicos = sum(hh_d_servbas == "Carente", na.rm = T),
            .groups = "drop"
  ) 


#consolidar base ----
#agregar variables hechas por separado
casen2022_comunas_4 <- casen2022_comunas_personas |> 
  left_join(casen2022_numericos_hogar) |> 
  left_join(casen2022_numericos_personas) |> 
  left_join(casen2022_comunas_hogares, join_by(comuna, cut_comuna, region)) |> 
  left_join(jefatura_femenina)


# porcentajes ----
# se obtienen los conteos de personas por comuna, y se transforman en porcentajes, para poder comparar comunas

variables_personas <- casen2022_comunas_personas |> select(where(is.numeric)) |> names() |> str_subset("poblacion", negate = TRUE)
variables_hogares <- casen2022_comunas_hogares |> select(where(is.numeric)) |> names() |> str_subset("hogares", negate = TRUE)

casen2022_comunas_5 <- casen2022_comunas_4 |> 
  group_by(comuna, cut_comuna, region) |>
  #porcentaje en relación a población
  mutate(across(any_of(variables_personas),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  #porcentaje en relación a viviendas
  mutate(across(any_of(c("hogar_jefatura_femenina", variables_hogares)),
                ~.x/hogares, .names = "{.col}_p"))


#filtrar columnas necesarias ----
#solo variables numéricas y porcentuales, no conteos crudos

casen2022_comunas_6 <- casen2022_comunas_5 |> 
  ungroup() |> 
  select(comuna, region, poblacion, hogares,
         any_of(variables_numericas_hogar),
         any_of(variables_numericas_personas),
         any_of(variables_ingresos_hogar),
         any_of(variables_ingresos_personas),
         ends_with("_p")
  )


#revisar variables ----
# casen2022_comunas_5 |> View()
# casen2022_comunas_5 |> names() |> cat(sep = "\n")

# casen2022_comunas |> 
#   count(sexo)
# casen2022_comunas |> 
#   count(v12mt)
# casen2022_comunas |> count(v12mt)

glimpse(casen2022_comunas_5)

# variables
# "Jubilación o pensión de vejez (promedio)" = "y28_2c",
# "Número de hijos vivos" = "s4",

# casen2022_comunas_5 |> count(v12mt)
# casen2022_comunas_5 |> count(area)
# casen2022_comunas_5 |> count(hacinamiento)
# casen2022_comunas_5 |> count(y2803)
# casen2022_comunas_5 |> count(rural_p)
# casen2022_comunas_5 |> count(fonasa)
# casen2022_comunas_5 |> count(isapre_p)

#probar todas las variables de la lista que se usa en la app, para ver si están presentes en los datos
walk(unlist(variables), ~{
  message("probando ", .x)
  .variable <- unname(.x)
  
  conteo <- casen2022_comunas_6 |> count(!!sym(.variable))
  message(nrow(conteo))
})

# nombre_variable("hacinamiento")

# volver a ponerle cut_comunas
casen2022_comunas_7 <- casen2022_comunas_6 |> 
  left_join(casen2022_2 |> 
              select(comuna, cut_comuna) |> 
              distinct(),
            by = "comuna") |> 
  relocate(cut_comuna, .after = comuna)

# #guardar datos preparados para su uso en la app
readr::write_csv2(casen2022_comunas_7, "app/casen_comunas.csv")
