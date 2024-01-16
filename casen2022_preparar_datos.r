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


# filtrar variables y aplicar factor de expansión ----
casen2022_comunas <- casen2022_2 |> 
  select(any_of(variables_casen)) |> 
  tidyr::uncount(weights = expc) |> #factor de expansión
  mutate(nivel = "comuna")


# modificar variables ----
casen2022_comunas_2 <- casen2022_comunas |> 
  mutate(dautr = as.integer(dautr)) |> 
  mutate(qautr = as.integer(qautr)) |> 
  mutate(v12mt = readr::parse_integer(as.character(v12mt)))

# probar variables ----
casen2022_comunas_2 |> count(v12mt)
casen2022_comunas_2 |> count(area)


# calcular variables numéricas ----
casen2022_numericos_hogar <- casen2022_comunas_2 |> 
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(comuna, cut_comuna, region) |> 
  # summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
  summarize(across(any_of(variables_numericas_hogar |> unname()),
                   ~mean(.x, na.rm = TRUE)), 
            .groups = "drop")


casen2022_numericos_personas <- casen2022_comunas_2 |> 
  group_by(comuna, cut_comuna, region) |> 
  # summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), 
  summarize(across(any_of(variables_numericas_personas |> unname()),
                   ~mean(.x, na.rm = TRUE)), 
            .groups = "drop")



# calcular variables de conteo ----
jefatura_femenina <- casen2022_comunas |> 
  group_by(comuna, cut_comuna, region, sexo) |> 
  count(pco1) |> 
  ungroup() |> 
  filter(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer") |> 
  select(comuna, cut_comuna, region, hogar_jefatura_femenina = n)


#variables por persona
casen2022_comunas_personas <- casen2022_comunas_2 %>%
  group_by(comuna, cut_comuna, region) %>%
  summarize(poblacion = n(),
            pobreza = sum(pobreza == "Pobreza extrema" | pobreza == "Pobreza no extrema", na.rm = TRUE),
            originario = sum(r3 != "11. No pertenece a ninguno de estos pueblos indígenas", na.rm = TRUE),
            extranjero = sum(r1a == "3. Otro país (extranjeros)", na.rm=TRUE),
            inactivos = sum(activ == "Inactivos", na.rm=TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            pobreza_multi = sum(pobreza_multi_5d == "Pobreza", na.rm=TRUE),
            fonasa = sum(str_detect(s13, "FONASA"), na.rm=TRUE),
            isapre = sum(str_detect(s13, "Isapre"), na.rm=TRUE),
            estudios_superiores = sum(educ %in% c("Técnico nivel superior completo", "Profesional completo", "Posgrado incompleto", "Posgrado completo"), na.rm=TRUE),
  ) |> 
  ungroup()

#variables por hogar
casen2022_comunas_hogares <- casen2022_comunas_2 %>%
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(comuna, cut_comuna, region) %>%
  summarize(hogares = n(),
            hacinamiento =  sum(p9 >= 4, na.rm=TRUE),
            men18c = sum(men18c == "Sí", na.rm=TRUE),
            rural = sum(area == "Rural", na.rm=TRUE),
            may60c = sum(may60c == "Sí", na.rm=TRUE),
            vivienda_propia = sum(v13 == "1. Propia", na.rm=TRUE),
            vivienda_pequeña = sum(v12 == "1. Menos de 30 m2", na.rm=TRUE)
  ) |> 
  ungroup()


#agregar variables hechas por separado
casen2022_comunas_4 <- casen2022_comunas_personas |> 
  left_join(casen2022_numericos_hogar) |> 
  left_join(casen2022_numericos_personas) |> 
  left_join(casen2022_comunas_hogares, join_by(comuna, cut_comuna, region)) |> 
  left_join(jefatura_femenina)


# porcentajes ----
casen2022_comunas_5 <- casen2022_comunas_4 |> 
  group_by(comuna, cut_comuna, region) |>
  #porcentaje en relación a población
  mutate(across(c(pobreza, originario, extranjero, inactivos, desocupados, pobreza_multi, fonasa, isapre,
                  estudios_superiores),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  #porcentaje en relación a viviendas
  mutate(across(c(hogar_jefatura_femenina, hacinamiento, men18c, rural, may60c, vivienda_propia, vivienda_pequeña),
                ~.x/hogares, .names = "{.col}_p"))

# casen2022_comunas_5 |> View()
# casen2022_comunas_5 |> names() |> cat(sep = "\n")

# casen2022_comunas |> 
#   count(sexo)
# casen2022_comunas |> 
#   count(v12mt)
# casen2022_comunas |> count(v12mt)

glimpse(casen2022_comunas_5)

#revisar variables ----
variables
# "Jubilación o pensión de vejez (promedio)" = "y28_2c",
# "Número de hijos vivos" = "s4",

casen2022_comunas_5 |> count(v12mt)
casen2022_comunas_5 |> count(area)
casen2022_comunas_5 |> count(hacinamiento)
casen2022_comunas_5 |> count(y2803)
casen2022_comunas_5 |> count(rural_p)
casen2022_comunas_5 |> count(fonasa)
casen2022_comunas_5 |> count(isapre_p)

#probar todas las variables
walk(unlist(variables), ~{
  message("probando ", .x)
  .variable <- unname(.x)
  
  conteo <- casen2022_comunas_5 |> count(!!sym(.variable))
  message(nrow(conteo))
})

# nombre_variable("hacinamiento")

#guardar datos preparados para su uso en la app
readr::write_csv2(casen2022_comunas_5, "app/casen_comunas.csv")
