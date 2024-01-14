library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
#library(formattable)

#desactivar notación científica
options(scipen=9999)

#cargar datos
casen_datos <- readRDS("casen_datos.rds")

variables_conteo <- c("hacinamiento",  
                      "pobreza",
                      "originario",
                      "extranjero",
                      "inactivos",
                      "desocupados",
                      "pobreza_multi",
                      "casos_comuna_nuevos",
                      "casos_comuna_incremental",
                      "casos_comuna_activos")


#vector nombrado que tiene los nombres de las variables
variables <- c("Años de escolaridad" = "esc",
               "Edad" = "edad",
               "Ingreso total del hogar corregido" = "ytotcorh",
               "Ingreso total corregido" = "ytotcor",
               "Ingreso ocupación principal" = "yoprcor",
               "Ingreso total per cápita del hogar corregido" = "ypc",
               "Ingreso del trabajo" = "ytrabajocor",
               "Ingreso del trabajo del hogar" = "ytrabajocorh",
               "Ingreso autónomo per cápita " = "ypchautcor",
               "Jubilación o pensión" = "y26_2c",
               "Numero de personas en el hogar" = "numper",
               "Número de hijos vivos" = "s4",
               #nuevas
               "Metros cuadrados aproximados de la vivienda" = "metros",
               #conteos
               "Personas en situación de hacinamiento" = "hacinamiento",  
               "Personas en situación de pobreza" = "pobreza",
               "Personas en situación de pobreza multidimensional" = "pobreza_multi",
               "Personas pertenecientes a pueblos originarios" = "originario",
               "Personas de nacionalidad extranjera" = "extranjero",
               "Personas en situación de inactividad económica" = "inactivos",
               "Personas en situación de desocupación económica" = "desocupados",
               #covid
               "Casos nuevos de COVID-19" = "casos_comuna_nuevos",
               "Casos totales de COVID-19" = "casos_comuna_incremental",
               "Casos activos de COVID-19" = "casos_comuna_activos")
