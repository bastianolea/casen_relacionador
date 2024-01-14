library(dplyr)
library(purrr)


#preprocesar ----
message("cargando casen 2022...")
ruta_casen2022 = "datos/Base de datos Casen 2022 STATA.dta" 

  
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
casen2022 <- readstata13::read.dta13(ruta_casen2022, generate.factors = T) |> 
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



# seleccionar variables ----
#seleccionar variables
variables_casen <- c(
         "comuna",
         "cut_comuna",
         "region",
         "area",
         "expc",                    #factor de expansión comunal
         "sexo",                    #género
         "esc",                     #años de escolaridad
         "edad",                    #edad
         
         #económicas y de salario
         "ytotcorh",                #Ingreso total del hogar corregido
         "ytotcor",                 #Ingreso total corregido
         "yoprcor",                 #Ingreso ocupación principal
         "ypc",                     #Ingreso total per cápita del hogar corregido
         "ytrabajocor",             #ingreso del trabajo
         "ytrabajocorh",            #ingreso del trabajo del hogar
         "ypchautcor",              #ingreso autónomo per cápita 
         "y26_2c",                  #jubilación o pensión
         "y0101",                   #Asalariados principal - Sueldos y salarios monetario
         "yosa",                    #Ingresos de la ocupación secundaria - Asalariados
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
         
         #vivienda
         "numper",                  #numero de personas en el hogar
         "s4",                      #hijos vivos
         "pco1",                    #jefe de hogar
         "v12",                     #metros cuadrados de la casa
         "indmat",                  #índice de materialidad de la vivienda
         
         
         "p2",                      #Indique estado de edificios y casas del sector
         "p3",                      #Presencia de basura en el sector
         "p4",                      #Vandalismo, grafiti o daño deliberado a la propiedad en el sector
         "p9",                      #p9. Incluyéndose a Ud., ¿cuántas personas viven habitualmente en la vivienda?
         
         
         "contrato",                #Tiene contrato de trabajo
         "v13",                     #v13. Su hogar, ¿bajo qué situación ocupa la vivienda?
         "men18c",                  #Presencia de menores de 18 años en el hogar (excluye SDPA)
         "may60c",                  #Presencia de mayores de 60 años en el hogar (excluye SDPA)
         "educ",                    #Nivel de escolaridad
         "s13",                     #s13. ¿A qué sistema previsional de salud pertenece?
         "v1",                      #v1. ¿Cuál es el tipo de vivienda que ocupa la persona entrevistada?
         "qaut",                    #Quintil autónomo nacional
         "dautr",                   #Decil autónomo regional
         "qautr"                    #Quintil autónomo regional
          )

casen2022_3 <- casen2022_2 |> 
  select(any_of(variables_casen))


# aplicar factor de expansión ----
casen2022_4 <- casen2022_3 |> 
  tidyr::uncount(weights = expc)

casen_numericas_comunas <- casen2022_4 |> 
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(comuna, cut_comuna, region) |> 
  summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")

casen_numericas_regiones <- casen2022_4 |> 
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(region) |> 
  summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")


# crear variables ----
casen2022_5 <- casen2022_4 %>%
  group_by(comuna) %>%
  summarize(#hacinamiento =  sum(hacinamiento == "Hacinamiento crítico (5 y más)" | hacinamiento == "Hacinamiento medio alto (3,5 a 4,9)" | hacinamiento == "Hacinamiento medio bajo (2,5 a 3,49)", na.rm = TRUE),
            pobreza = sum(pobreza == "Pobreza extrema" | pobreza == "Pobreza no extrema", na.rm = TRUE),
            originario = sum(r3 != "11. No pertenece a ninguno de estos pueblos indígenas", na.rm = TRUE),
            extranjero = sum(r1a == "3. Otro país (extranjeros)", na.rm=TRUE),
            inactivos = sum(activ == "Inactivos", na.rm=TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            pobreza_multi = sum(pobreza_multi_5d == "Pobreza", na.rm=TRUE)
            )

readr::write_csv2(casen_numericas_comunas, "casen_comunas.csv")