#Código de limpieza de datos

#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2")
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


#Importar archivos Diego
train_hogares <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/train_hogares.Rds")
test_hogares <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/test_hogares.Rds")

train_personas <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/train_personas.Rds")
test_personas <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/test_personas.Rds")




#Variables que pueden sonarnos:
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

#Edad jefe del hogar
train_p_edadjefe <- train_p%>%subset(P6050==1)%>%select(id, P6040)

#Jefe del hogar cotiza a seguridad social
train_p_jefecotiza <- train_p%>%subset(P6050==1)%>%select(id, P6090)%>%mutate(jefe_cotiza=ifelse(is.na(P6090),9,P6090))

#relación laboral:P6430
# 
train_p_jef_ocu <- train_p%>%subset(P6050==1)%>%mutate(relab_jefe= ifelse(is.na(Oc), ifelse(is.na(Des), 11, 10) , P6430) )%>%select(id, relab_jefe)  #ifelse(Des==1,10,11)     


#Subsidio familiar
train_p%>%subset(P6050==1)%>%count(P6585s3)

#Subsidio educativo
train_p%>%subset(P6050==1)%>%count(P6585s4)
train_p%>%subset(P6050==1)%>%count(Oc, P6585s3)


#oficio del jefe del hogar (No se está usando)
#NA 47584 
train_p_jefeofi <- train_p%>%subset(P6050==1)%>%select(id, Oficio)


#Horas totales trabajadas
train_p_horas <- train_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%group_by(id)%>%summarise(Horas_Hogar = sum(Horas_reales))

train_p_horas_jefe <- train_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%subset(P6050==1)%>%select(id, Horas_reales)

union_horas <- full_join(train_p_horas, train_p_horas_jefe)
View(union_horas)


is.na(train_p_horas$P6800)%>%table()

View(train_p_horas%>%select(id, Oc, P6800, Horas_reales))

#***Maximo nivel educactivo en el hogar

train_p_adulto_maxlev_edu <-train_p%>%subset(is.na(P6210)==FALSE)%>%group_by(id)%>%summarise(max_edu_lev_h = max(P6210))





#Merge de personas y hogares

train_completa <- train_h
train_completa <- full_join(train_completa, train_p_num_muj)
train_completa <- full_join(train_completa, train_p_jef_muj)
train_completa <- full_join(train_completa, train_p_edadjefe)
train_completa <- full_join(train_completa, train_p_jefecotiza)
train_completa <- full_join(train_completa, train_p_jef_ocu)
train_completa <- full_join(train_completa, union_horas)

write.csv(train_completa, "train_completa.csv")

