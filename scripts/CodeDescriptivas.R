#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2")

#Se establece semilla
set.seed(1000)
#Importar paquetes y cargar librerías
require(pacman)
p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe, 
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here, hablar)

##########################################################################
##########################################################################

##Creo que se puede quitar

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
#Base de 
train_p <- train_p%>%mutate(mujer = P6020 - 1)

#Número de mujeres por hogar
train_p_num_muj <- train_p%>%group_by(id)%>%summarise(num_mujeres = sum(mujer))

#Jefe de hogar es mujer?
train_p <- train_p%>%mutate(jefe_y_mujer = ifelse(mujer == 1 & P6050 == 1, 1, 0))
train_p_jef_muj <- train_p%>%group_by(id)%>%summarise(jefe_mujer = sum(jefe_y_mujer))

#Edad jefe del hogar
train_p_edadjefe <- train_p%>%subset(P6050==1)%>%select(id, P6040)

#Jefe del hogar cotiza a seguridad social
train_p_jefecotiza <- train_p%>%subset(P6050==1)%>%select(id, P6090)%>%mutate(jefe_cotiza=ifelse(is.na(P6090),9,P6090))

#relación laboral:P6430 del jefe del hogar
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


#Horas totales trabajadas en el hogar
train_p_horas <- train_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%group_by(id)%>%summarise(Horas_Hogar = sum(Horas_reales))

#Horas trabajadas por el jefe
train_p_horas_jefe <- train_p%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%subset(P6050==1)%>%select(id, Horas_reales)

union_horas <- full_join(train_p_horas, train_p_horas_jefe)

View(train_p_horas%>%select(id, Oc, P6800, Horas_reales))

#Maximo nivel educactivo en el hogar
train_p_adulto_maxlev_edu <-train_p%>%subset(is.na(P6210)==FALSE)%>%group_by(id)%>%summarise(max_edu_lev_h = max(as.numeric(P6210)) ) 

table(train_p_adulto_maxlev_edu$max_edu_lev_h)

# Tabla de estadísticas descriptivas con stargazer

dftablaeduc <- train_p_adulto_maxlev_edu %>% 
  group_by(max_edu_lev_h) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

stargazer(dftablaeduc$max_edu_lev_h, type = "html", title = "Estadísticas Descriptivas", out = "estdec.html")


#Numero de empleados 

train_p_num_empl <- train_p%>%mutate(num_empleados= ifelse(is.na(P6870), 0, P6870))%>%group_by(id)%>%summarise(max_empl= max(as.numeric(num_empleados)))


##########################################################################
##########################################################################

# Importar base de datos

train_completa <- read.csv("train_completa.csv")

train_completa_definitiva <- read.csv("data/train_completa_definitiva.csv")

# Corrección educación

train_completa_definitiva <- train_completa_definitiva %>%
  mutate(años_educ1 = case_when(Educ1 == 1 ~ 0,
                                Educ1 == 2 ~ 2,
                                Educ1 == 3 ~ 7,
                                Educ1 == 4 ~ 10,
                                Educ1 == 5 ~ 13,
                                Educ1 == 6 ~ 18))

train_completa_definitiva <- train_completa_definitiva %>%
  mutate(años_educ2 = case_when(Educ2 == 1 ~ 0,
                                Educ2 == 2 ~ 2,
                                Educ2 == 3 ~ 7,
                                Educ2 == 4 ~ 10,
                                Educ2 == 5 ~ 13,
                                Educ2 == 6 ~ 18))

train_completa_definitiva <- train_completa_definitiva %>%
  mutate(años_educ3 = case_when(Educ3 == 1 ~ 0,
                                Educ3 == 2 ~ 2,
                                Educ3 == 3 ~ 7,
                                Educ3 == 4 ~ 10,
                                Educ3 == 5 ~ 13,
                                Educ3 == 6 ~ 18))

train_completa_definitiva$años_educ_promedio <- rowMeans(subset(train_completa_definitiva, select= c("años_educ1", "años_educ2", "años_educ3")),na.rm=TRUE)

summary(train_completa_definitiva$años_educ_promedio)

# Cuartos per capita

train_completa_definitiva <- train_completa_definitiva %>% mutate(cuartospc = P5010/Nper)

summary(train_completa_definitiva$cuartospc)

# Gráficos

summary(train_completa_definitiva$Ingpcug)



