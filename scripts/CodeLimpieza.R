#Código de limpieza de datos

#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2")

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


#Importar archivos Diego
train_hogares <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/train_hogares.Rds")
test_hogares <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/test_hogares.Rds")

train_personas <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/train_personas.Rds")
test_personas <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/test_personas.Rds")


#Importar archivos Samuel

train_hogares <- readRDS("data/train_hogares.Rds")
test_hogares <- readRDS("data/test_hogares.Rds")

train_personas <- readRDS("data/train_personas.Rds")
test_personas <- readRDS("data/test_personas.Rds")


#Variables indicadores/significativas:

#Clase (clasifica si es rural o urbano)
#Dominio (en cuál ciudad está)
#P5090 (Indica si la vivienda es propia, arrendada u otra)
#p5130 y p5140 nos indican pagos de arriengos 
#nper y npersug


#Que variables se comparten?
names(test_hogares)
names(train_hogares)

#Que variables se comparten?
names(test_personas)
names(train_personas)


#Construir pobrezaMonetaria porque "pobre" hace referencia a pobreza extrema
####Esto de aca son solo pruebas, no hay que volver a correrlo
train_hogares <- train_hogares%>%mutate(prueba_pobreza = ifelse(Ingpcug < Li, 1, 0))
train_hogares <- train_hogares%>%mutate(test1 = ifelse(Pobre == prueba_pobreza, TRUE, FALSE))
train_hogares%>%count(test1)
train_hogares <- train_hogares%>%mutate(prueba_pobreza2 = ifelse(Ingpcug < Lp, 1, 0))
train_hogares <- train_hogares%>%mutate(tets = ifelse(Pobre == prueba_pobreza2, TRUE, FALSE))
train_hogares%>%count(tets)
#Con lo anterior comprobamos que Pobre es 1 si el ingreso per capita del hogar es menor a la linea de pobreza y 0 dlc


#Base para clasificación a nivel hogares
train_h <- train_hogares[names(test_hogares)]
#crear la variable de prediccion Pobre
train_h$Pobre <- train_hogares$Pobre



#Base a nivel personas 
train_p <- train_personas[names(test_personas)]

#Creación de variables a nivel hogar de la base personas

train_p <- train_p%>%mutate(mujer = P6020 - 1)

#Número de mujeres por hogar
train_p_num_muj <- train_p%>%group_by(id)%>%summarise(num_mujeres = sum(mujer))

#Jefe de hogar es mujer (0-1)
train_p <- train_p%>%mutate(jefe_y_mujer = ifelse(mujer == 1 & P6050 == 1, 1, 0))
train_p_jef_muj <- train_p%>%group_by(id)%>%summarise(jefe_mujer = sum(jefe_y_mujer))

#Edad jefe del hogar
train_p_edadjefe <- train_p%>%subset(P6050==1)%>%select(id, P6040)

#Jefe del hogar cotiza a seguridad social
train_p_jefecotiza <- train_p%>%subset(P6050==1)%>%select(id, P6090)%>%mutate(jefe_cotiza=ifelse(is.na(P6090),9,P6090))

#relab: relación laboral:P6430 del jefe del hogar
train_p_jef_ocu <- train_p%>%subset(P6050==1)%>%mutate(relab_jefe= ifelse(is.na(Oc), ifelse(is.na(Des), 11, 10) , P6430) )%>%select(id, relab_jefe)  #ifelse(Des==1,10,11)     

#Subsidio familiar
train_p%>%subset(P6050==1)%>%count(P6585s3)

#Subsidio educativo
train_p%>%subset(P6050==1)%>%count(P6585s4)
train_p%>%subset(P6050==1)%>%count(Oc, P6585s3)


#oficio del jefe del hogar (No se está usando)
#NA 47584 
train_p_jefeofi <- train_p%>%subset(P6050==1)%>%select(id, Oficio)


#Horas totales trabajadas en el hogar
train_p_horas <- train_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%group_by(id)%>%summarise(Horas_Hogar = sum(Horas_reales))

#Horas trabajadas por el jefe
train_p_horas_jefe <- train_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%subset(P6050==1)%>%select(id, Horas_reales)

union_horas <- full_join(train_p_horas, train_p_horas_jefe)

View(train_p_horas%>%select(id, Oc, P6800, Horas_reales))



#Maximo nivel educactivo en el hogar
train_p_adulto_maxlev_edu <-train_p%>%subset(is.na(P6210)==FALSE)%>%group_by(id)%>%summarise(max_edu_lev_h = max(as.numeric(P6210)) ) 


#Numero de empleados 

train_p_num_empl <- train_p%>%mutate(num_empleados= ifelse(is.na(P6870), 0, P6870))%>%group_by(id)%>%summarise(max_empl= max(as.numeric(num_empleados)))


#Merge: unir bases a nivel hogar y bases de personas a nivel del hogar

train_completa <- train_h
train_completa <- full_join(train_completa, train_p_num_muj)
train_completa <- full_join(train_completa, train_p_jef_muj)
train_completa <- full_join(train_completa, train_p_edadjefe)
train_completa <- full_join(train_completa, train_p_jefecotiza)
train_completa <- full_join(train_completa, train_p_jef_ocu)
train_completa <- full_join(train_completa, union_horas)


# escribir la base en archivo .csv
write.csv(train_completa, "train_hogares.csv")
# leer la base del archivo .csv
train_hogares <- read.csv("train_hogares.csv")





# crear la variable "Pobre" test_h
#Construir pobrezaMonetaria porque "pobre" hace referencia a pobreza extrema
####Esto de aca son solo pruebas, no hay que volver a correrlo
test_hogares <- test_hogares%>%mutate(prueba_pobreza = ifelse(Ingpcug < Li, 1, 0))
# 
# train_hogares <- train_hogares%>%mutate(test1 = ifelse(Pobre == prueba_pobreza, TRUE, FALSE))
# train_hogares%>%count(test1)
# train_hogares <- train_hogares%>%mutate(prueba_pobreza2 = ifelse(Ingpcug < Lp, 1, 0))
# train_hogares <- train_hogares%>%mutate(tets = ifelse(Pobre == prueba_pobreza2, TRUE, FALSE))
# train_hogares%>%count(tets)
# #Con lo anterior comprobamos que Pobre es 1 si el ingreso per capita del hogar es menor a la linea de pobreza y 0 dlc


# construir la base test_set hogares
# Base para clasificación hogares
test_h <- test_hogares[names(test_hogares)]
test_h$Pobre <- test_hogares$Pobre


# agregar las variables construidas a partir de la base train_hogares

#Base para clasificación hogares
test_h <- train_hogares[names(test_hogares)]

test_p <- train_personas[names(test_personas)]


#Creación de variables de nivel hogar a partir en base personas en test_p
#Base de 
test_p <- test_p%>%mutate(mujer = P6020 - 1)

#Número de mujeres por hogar
test_p_num_muj <- test_p%>%group_by(id)%>%summarise(num_mujeres = sum(mujer))

#Jefe de hogar es mujer?
test_p <- test_p%>%mutate(jefe_y_mujer = ifelse(mujer == 1 & P6050 == 1, 1, 0))
test_p_jef_muj <- test_p%>%group_by(id)%>%summarise(jefe_mujer = sum(jefe_y_mujer))

#Edad jefe del hogar
test_p_edadjefe <- test_p%>%subset(P6050==1)%>%select(id, P6040)

#Jefe del hogar cotiza a seguridad social
test_p_jefecotiza <- test_p%>%subset(P6050==1)%>%select(id, P6090)%>%mutate(jefe_cotiza=ifelse(is.na(P6090),9,P6090))

#relación laboral:P6430 del jefe del hogar
# 
test_p_jef_ocu <- test_p%>%subset(P6050==1)%>%mutate(relab_jefe= ifelse(is.na(Oc), ifelse(is.na(Des), 11, 10) , P6430) )%>%select(id, relab_jefe)  #ifelse(Des==1,10,11)     


#Subsidio familiar
test_p%>%subset(P6050==1)%>%count(P6585s3)

#Subsidio educativo
test_p%>%subset(P6050==1)%>%count(P6585s4)
test_p%>%subset(P6050==1)%>%count(Oc, P6585s3)


#oficio del jefe del hogar (No se está usando)
test_p_jefeofi <- test_p%>%subset(P6050==1)%>%select(id, Oficio)


#Horas totales trabajadas en el hogar
test_p_horas <- test_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%group_by(id)%>%summarise(Horas_Hogar = sum(Horas_reales))

#Horas trabajadas por el jefe
test_p_horas_jefe <- test_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%subset(P6050==1)%>%select(id, Horas_reales)

test_p_union_horas <- full_join(test_p_horas, test_p_horas_jefe)

#View(train_p_horas%>%select(id, Oc, P6800, Horas_reales))



#Maximo nivel educactivo en el hogar
test_p_adulto_maxlev_edu <-test_p%>%subset(is.na(P6210)==FALSE)%>%group_by(id)%>%summarise(max_edu_lev_h = max(as.numeric(P6210)) ) 


#Numero de empleados 

test_p_num_empl <- test_p%>%mutate(num_empleados= ifelse(is.na(P6870), 0, P6870))%>%group_by(id)%>%summarise(max_empl= max(as.numeric(num_empleados)))



#Merge de personas y hogares: unir las variables de la base de hogares y personas a nivel de hogares

test_completa <- test_h
test_completa <- full_join(test_completa, test_p_num_muj)
test_completa <- full_join(test_completa, test_p_jef_muj)
test_completa <- full_join(test_completa, test_p_edadjefe)
test_completa <- full_join(test_completa, test_p_jefecotiza)
test_completa <- full_join(test_completa, test_p_jef_ocu)
test_completa <- full_join(test_completa, test_p_union_horas)


# escribir la base en archivo .csv
write.csv(test_completa, "test_hogares.csv")
# leer la base del archivo .csv
train_hogares <- read.csv("test_hogares.csv")








