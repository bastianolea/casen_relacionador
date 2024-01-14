library(dplyr)
library(purrr)
library(stringr)

#preprocesar ----
message("cargando casen 2022...")

  
#obtener codigos unicos territoriales desde libro de codigos
cut_comunas <- readxl::read_excel("manuales/Libro_de_codigos_Base_de_datos_provincia_y_comuna_Casen_2022.xlsx") |> 
  janitor::row_to_names(2) |> 
  janitor::clean_names() |> 
  tidyr::fill(nombre_variable) |> 
  filter(nombre_variable == "comuna") |> 
  select(cut_comuna = valores, comuna = etiquetas_de_valores) |> 
  filter(!is.na(cut_comuna), !is.na(comuna))

#cargar columans de comunas
casen2022comunas <- readstata13::read.dta13("datos/Base de datos provincia y comuna Casen 2022 STATA.dta", generate.factors = T) |> 
  as_tibble() |> 
  left_join(cut_comunas, join_by(comuna))

#cargar base
casen2022 <- readstata13::read.dta13("datos/Base de datos Casen 2022 STATA.dta" , generate.factors = T) |> 
  as_tibble()

#unir base con comunas
casen2022_2 <- casen2022 |> 
  left_join(casen2022comunas, join_by(folio, id_persona)) |> 
  select(names(casen2022comunas), everything()) |> 
  select(-id_vivienda, -folio, -id_persona, -cod_upm, -estrato, -varstrat, -varunit, -fecha_entrev)

#guardar
# arrow::write_parquet(casen2022_2, "datos/casen2022.parquet")

# # eliminar base de datos
# file.remove("datos/Base de datos Casen 2022 STATA.dta")


source("variables.R")



# filtrar variables y aplicar factor de expansión ----
casen2022_comunas <- casen2022_2 |> 
  select(any_of(variables_casen)) |> 
  tidyr::uncount(weights = expc) |> 
  mutate(nivel = "comuna")


# modificar variables ----
casen2022_comunas_2 <- casen2022_comunas |> 
  mutate(dautr = as.integer(dautr)) |> 
  mutate(qautr = as.integer(qautr)) |> 
  mutate(v12mt = readr::parse_integer(as.character(v12mt)))


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
            fonasa = sum(str_detect(s13, "FONASA"), na.rm=TRUE)
  ) |> 
  ungroup()

#variables por hogar
casen2022_comunas_hogares <- casen2022_comunas_2 %>%
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(comuna, cut_comuna, region) %>%
  summarize(hogares = n(),
            hacinamiento =  sum(p9 >= 4, na.rm=TRUE),
            men18c = sum(men18c == "Sí", na.rm=TRUE),
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
  #porcentaje en relación a población
  mutate(across(c(pobreza, originario, extranjero, inactivos, desocupados, pobreza_multi, fonasa),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  #porcentaje en relación a viviendas
  mutate(across(c(men18c, may60c, vivienda_propia, vivienda_pequeña, hogar_jefatura_femenina, hacinamiento),
                ~.x/hogares, .names = "{.col}_p"))

# casen2022_comunas_5 |> View()
# casen2022_comunas_5 |> names() |> cat(sep = "\n")

# casen2022_comunas |> 
#   count(sexo)
# casen2022_comunas |> 
#   count(v12mt)


readr::write_csv2(casen2022_comunas_5, "casen_comunas.csv")
