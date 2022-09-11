#Código de limpieza de datos


#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd()
#Samuel
setwd()

#Se establece semilla
set.seed(1000)
#Importar paquetes y cargar librerías
require(pacman)
p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe, 
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here)

#Importar archivos
train_hogares <- readRDS("train_hogares.Rds")
train_personas <- readRDS("train_personas.Rds")
test_hogares <- readRDS("test_hogares.Rds")
test_personas <- readRDS("test_personas.Rds")


#Variables que pueden sonarnos:
#Clase (clasifica si es rural o urbano)
#Dominio (en cuál ciudad está)
#P5090 (Indica si la vivienda es propia, arrendada u otra)
#p5130 y p5140 nos indican pagos de arriengos 
#nper y npersug


#Que variables se comparten?
names(test_hogares)
names(train_hogares)

#Construir pobrezaMonetaria porque "pobre" hace referencia a pobreza extrema
train_hogares <- train_hogares%>%mutate(prueba_pobreza = ifelse(Ingpcug < Li, 1, 0))
train_hogares <- train_hogares%>%mutate(test1 = ifelse(Pobre == prueba_pobreza, TRUE, FALSE))
train_hogares%>%count(test1)
train_hogares <- train_hogares%>%mutate(prueba_pobreza2 = ifelse(Ingpcug < Lp, 1, 0))
train_hogares <- train_hogares%>%mutate(tets = ifelse(Pobre == prueba_pobreza2, TRUE, FALSE))
train_hogares%>%count(tets)
#Con lo anterior comprobamos que Pobre es 1 si el ingreso per capita del hogar es menor a la linea de pobreza y 0 dlc


#Base para clasificación hogares
train_h <- train_hogares[names(test_hogares)]
train_h$Pobre <- train_hogares$Pobre
train_p <- train_personas[names(test_personas)]

#Creación de variables de nivel hogar en base personas
#Número de mujeres por hogar
train_p <- train_p%>%mutate(mujer = P6020 - 1)
train_p_num_muj <- train_p%>%group_by(id)%>%summarise(num_mujeres = sum(mujer))

#Jefe de hogar es mujer?
train_p <- train_p%>%mutate(jefe_y_mujer = ifelse(mujer == 1 & P6050 == 1, 1, 0))
train_p_jef_muj <- train_p%>%group_by(id)%>%summarise(jefe_mujer = sum(jefe_y_mujer))


#Merge de personas y hogares
