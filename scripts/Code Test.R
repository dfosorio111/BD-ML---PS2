#Código de limpieza de datos

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
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here)

#Importar archivos
train_hogares <- readRDS("train_hogares.Rds")
train_personas <- readRDS("train_personas.Rds")
test_hogares <- readRDS("test_hogares.Rds")
test_personas <- readRDS("test_personas.Rds")

names(test_personas)


#Importar archivos Diego
train_hogares <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/train_hogares.Rds")
test_hogares <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/test_hogares.Rds")

train_personas <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/train_personas.Rds")
test_personas <- readRDS("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data/test_personas.Rds")


#Importar archivos Samuel

train_hogares <- readRDS("data/train_hogares.Rds")
train_personas <- readRDS("data/train_personas.Rds")

test_hogares <- readRDS("data/test_hogares.Rds")
test_personas <- readRDS("data/test_personas.Rds")


#Variables que pueden sonarnos:
#Clase (clasifica si es rural o urbano)
#Dominio (en cuál ciudad está)
#P5090 (Indica si la vivienda es propia, arrendada u otra)
#p5130 y p5140 nos indican pagos de arriengos 
#nper y npersug

#Creación de variables de nivel hogar en base personas
#Base de 
test_personas <- test_personas%>%mutate(mujer = P6020 - 1)

#Número de mujeres por hogar
test_p_num_muj <- test_personas%>%group_by(id)%>%summarise(num_mujeres = sum(mujer))

#Jefe de hogar es mujer?
test_personas <- test_personas%>%mutate(jefe_y_mujer = ifelse(mujer == 1 & P6050 == 1, 1, 0))
test_p_jef_muj <- test_personas%>%group_by(id)%>%summarise(jefe_mujer = sum(jefe_y_mujer))

#Edad jefe del hogar
test_p_edadjefe <- test_personas%>%subset(P6050==1)%>%select(id, P6040)

#Jefe del hogar cotiza a seguridad social
test_p_jefecotiza <- test_personas%>%subset(P6050==1)%>%select(id, P6090)%>%mutate(jefe_cotiza=ifelse(is.na(P6090),9,P6090))

#relación laboral:P6430 del jefe del hogar
#10 es desocupado y 11 es inactivo
test_p_jef_ocu <- test_personas%>%subset(P6050==1)%>%mutate(relab_jefe= ifelse(is.na(Oc), ifelse(is.na(Des), 11, 10) , P6430) )%>%select(id, relab_jefe)  #ifelse(Des==1,10,11)     

#Subsidio familiar
test_personas%>%subset(P6050==1)%>%count(P6585s3)

#Subsidio educativo
test_personas%>%subset(P6050==1)%>%count(P6585s4)
test_personas%>%subset(P6050==1)%>%count(Oc, P6585s3)
#Las de subsidios tienen muchos NA


#oficio del jefe del hogar (No se está usando)
#NA 47584 
test_p_jefeofi <- test_personas%>%subset(P6050==1)%>%select(id, Oficio)


#Horas totales trabajadas en el hogar
test_p_horas <- test_personas%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%group_by(id)%>%summarise(Horas_Hogar = sum(Horas_reales))

#Horas trabajadas por el jefe
test_p_horas_jefe <- test_personas%>%mutate(Horas_reales = ifelse(is.na(Oc), 0, P6800))%>%subset(P6050==1)%>%select(id, Horas_reales)

union_horas_test <- full_join(test_p_horas, test_p_horas_jefe)

View(test_p_horas%>%select(id, Oc, P6800, Horas_reales))


#Maximo nivel educactivo en el hogar
test_p_adulto_maxlev_edu <-test_personas%>%subset(is.na(P6210)==FALSE)%>%mutate(Nivel_Educ = ifelse(as.numeric(P6210) == 9,0,as.numeric(P6210)))%>%group_by(id)%>%summarise(max_edu_lev_h = max(as.numeric(Nivel_Educ)) ) 


#Máximo tamaño de empresa del hogar
test_p_num_empl <- test_personas%>%mutate(num_empleados= ifelse(is.na(P6870), 0, P6870))%>%group_by(id)%>%summarise(max_empl= max(as.numeric(num_empleados)))


#Sacar el número de ocupados dentro del hogar (Para posteriormente sacar la proporción de ocupados)
Ocupados_Hogar_test <- test_personas%>%mutate(Ocupados = ifelse(is.na(Oc),0,Oc))%>%group_by(id)%>%summarise(Num_ocu_hogar = sum(Ocupados))
#Número de desocupados
Desocupados_Hogar_test <- test_personas%>%mutate(Desocupados = ifelse(is.na(Des),0,Des))%>%group_by(id)%>%summarise(Num_des_hogar = sum(Desocupados))
#Cantidad de personas en edad de trabajar
Pet_Hogar_test <- test_personas%>%mutate(PobEdTrab = ifelse(is.na(Pet),0,Pet))%>%group_by(id)%>%summarise(Num_pet_hogar = sum(PobEdTrab))

#############################
####### no funciona #########
#############################

#Se busca hacer un "pivot" de aquellas variables que pueden resultar útil tener para más de un individuo del hogar

if_else(is.na(P6430),if_else(is.na(Des), 11, 12),as.double(P6430))

#############################
#############################
#############################

#Inicialmente con la relación laboral
#10 hace referencia a individuos estudiando, 11 a inactivos no estudiando, 12 a desocupados
test_relab <- test_personas%>%mutate(relacion_lab = ifelse(as.numeric(P6240) == 3, 10, if_else(is.na(P6430),if_else(is.na(Des), 11, 12),as.double(P6430))))%>%select(id, Orden, relacion_lab, Oc, Des, Ina, P6430, P6240, P6040, P6050)
#Los niños no clasifican a nada, se les dará valor de 13
test_relab$relacion_lab <- ifelse(is.na(test_relab$relacion_lab), 13, test_relab$relacion_lab)
prueba <- test_relab%>%subset(relacion_lab == 13)
max(prueba$P6040)
test_relab_def <- test_relab%>%select(id, Orden, relacion_lab)
reshape_relab <- pivot_wider(test_relab_def, names_from = Orden, values_from = relacion_lab)

reshape_relab <- reshape_relab%>%select("id", `1`, `2`, `3`)
#Los NA son gente que no existe, esos no existe se les dará valor de 0
reshape_relab$`2` <- ifelse(is.na(reshape_relab$`2`),0,reshape_relab$`2`)
reshape_relab$`3` <- ifelse(is.na(reshape_relab$`3`),0,reshape_relab$`3`)

colnames(reshape_relab) <- c("id", "Relab1", "Relab2", "Relab3")

#Nivel educativo
#Los NA del nivel educativo se ponen 0 porque todos hacen referencia a personas menores de edad no ocupados se les dará un valor de 10 (da igual porque es categórica.

test_personas%>%subset(P6040 >= 18)%>%count(P6210)
test_personas%>%subset(Oc == 1)%>%count(P6210)


test_educ <- test_personas%>%mutate(educacion = if_else(is.na(P6210),10,as.double(P6210)))%>%select(id, Orden, educacion)
is.na(test_educ$educacion)%>%table()
reshape_educ <- pivot_wider(test_educ, names_from = Orden, values_from = educacion)

#También arbitrariamente se escogen los 3 primeros
reshape_educ <- reshape_educ%>%select("id", `1`, `2`, `3`)

#Los NA son gente que no existe, esos no existe se les dará valor de 0
reshape_educ$`2` <- ifelse(is.na(reshape_educ$`2`),0,reshape_educ$`2`)
reshape_educ$`3` <- ifelse(is.na(reshape_educ$`3`),0,reshape_educ$`3`)

colnames(reshape_educ) <- c("id", "Educ1", "Educ2", "Educ3")

###############################
#aqui iria la variable de edad#
###############################

#Variable de pensión
test_personas%>%subset(P6040 >= 18)%>%count(P7500s2, P6920)

test_pension <- test_personas%>%mutate(cotiza_recibe = ifelse(P6920 == 1 | P6920 == 3 | P7500s2 == 1, 1,0))%>%select(id, Orden, cotiza_recibe)
test_pension$cotiza_recibe <- ifelse(is.na(test_pension$cotiza_recibe ), 0, test_pension$cotiza_recibe )
test_pension%>%count(cotiza_recibe)
reshape_pension <- pivot_wider(test_pension, names_from = Orden, values_from = cotiza_recibe)
reshape_pension$Cant_cotiza_recibe <- apply(reshape_pension[2:ncol(reshape_pension)],1,sum, na.rm = TRUE) 

reshape_pension <- reshape_pension%>%select("id", "Cant_cotiza_recibe")


test_personas%>%subset(Des == 1)%>%count(P7310)


#En el paper de Samu utilizan el estatus marital de la personas jefe de hogar
#Acá eso no se puede, pero podemos saber si la persona jefe de hogar vive con su pareja, hijos o nietos
test_personas%>%count(P6050)

test_miembros <- test_personas%>%select(id, Orden, P6050) 
reshape_miembros <- pivot_wider(test_miembros, names_from = Orden, values_from = P6050)

reshape_miembros$hijos <- 0
reshape_miembros$pareja <- 0
reshape_miembros$nietos <- 0
reshape_miembros$otros_parientes <- 0
reshape_miembros$no_parientes <- 0
reshape_miembros$emp_pen <- 0

nombres <- c("2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22")
for (i in 1:nrow(reshape_miembros)) {
  for (j in nombres) {
    if (is.na(reshape_miembros[i,j]) == FALSE) {
      if (reshape_miembros[i,j] == 2) {
        reshape_miembros$pareja[i] <- 1
      }else if (reshape_miembros[i,j] == 3) {
        reshape_miembros$hijos[i] <- 1
      }else if (reshape_miembros[i,j] == 4) {
        reshape_miembros$nietos[i] <- 1
      }else if (reshape_miembros[i,j] == 5) {
        reshape_miembros$otros_parientes[i] <- 1
      }else if (reshape_miembros[i,j] == 9) {
        reshape_miembros$no_parientes[i] <- 1
      }else if (reshape_miembros[i,j] == 6 | reshape_miembros[i,j] == 7 | reshape_miembros[i,j] == 8) {
        reshape_miembros$emp_pen[i] <- 1
      }
    }
    
  }
}

reshape_miembros_test <- reshape_miembros%>%select("id", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen")

write.csv(reshape_miembros_test, "reshapemiembrostest.csv")
reshape_miembros_test <- read.csv("reshapemiembrostest.csv")

#Seadiciona los miembros que viven con esas personas
test_completa <- full_join(test_completa, reshape_miembros_test)

#Merge de personas y hogares

test_completa <- test_hogares
test_completa <- full_join(test_completa, test_p_num_muj)
test_completa <- full_join(test_completa, test_p_jef_muj)
test_completa <- full_join(test_completa, test_p_edadjefe)
test_completa <- full_join(test_completa, test_p_jefecotiza)
test_completa <- full_join(test_completa, test_p_jef_ocu)
test_completa <- full_join(test_completa, union_horas_test)
test_completa <- full_join(test_completa, test_p_adulto_maxlev_edu)

#Hay solo 3 en No sabe, no responde, arbitrariamente los clasificaré en "Ninguno"
test_completa$max_edu_lev_h <- ifelse(test_completa$max_edu_lev_h == 0, 1, test_completa$max_edu_lev_h)

test_completa <- full_join(test_completa, test_p_num_empl)
test_completa <- full_join(test_completa, Ocupados_Hogar_test)
test_completa <- full_join(test_completa, Desocupados_Hogar_test)
test_completa <- full_join(test_completa, Pet_Hogar_test)

#Relación laboral de los primeros 3 miembros del hogar ¿Por qué 3? realmente porque ajá
#10 estudiando, 11 inactivo no estudiando, 12 desocupado, 13 niños (no aplican)
test_completa <- full_join(test_completa, reshape_relab)

#Nivel educativo (se respetan las categorías originales, 10 es para NA y 0 para quienes no existen)
test_completa <- full_join(test_completa, reshape_educ)

#Se añade la cantidad de personas que cotiza o recibe pensión en el hogar
test_completa <- full_join(test_completa, reshape_pension)

#La P6090 tiene unos NA pero son niños, entonces se les pondrá que no están en seguridad social en salud
test_completa%>%count(P6090)
test_completa$P6090 <- ifelse(is.na(test_completa$P6090), 2, test_completa$P6090)

#Variables de proporciones. (Más que la cantidad nos interesa la proporción)
test_completa$prop_ocupados_total <- test_completa$Num_ocu_hogar/test_completa$Nper
test_completa$prop_ocupados_pet <- test_completa$Num_ocu_hogar/test_completa$Num_pet_hogar

test_completa$prop_Desocupados_total <- test_completa$Num_des_hogar/test_completa$Nper
test_completa$prop_Desocupados_pet <- test_completa$Num_des_hogar/test_completa$Num_pet_hogar

test_completa%>%count(prop_Desocupados_pet)

#Mujeres
test_completa$prop_mujeres_total <- test_completa$num_mujeres/test_completa$Nper
test_completa$prop_mujeres_pet <- test_completa$num_mujeres/test_completa$Num_pet_hogar

# Corrección educación

test_completa <- test_completa %>%
  mutate(años_educ1 = case_when(Educ1 == 1 ~ 0,
                                Educ1 == 2 ~ 2,
                                Educ1 == 3 ~ 7,
                                Educ1 == 4 ~ 10,
                                Educ1 == 5 ~ 13,
                                Educ1 == 6 ~ 18,
                                Educ1 == 9 ~ 0))

test_completa<- test_completa %>%
  mutate(años_educ2 = case_when(Educ2 == 1 ~ 0,
                                Educ2 == 2 ~ 2,
                                Educ2 == 3 ~ 7,
                                Educ2 == 4 ~ 10,
                                Educ2 == 5 ~ 13,
                                Educ2 == 6 ~ 18,
                                Educ2 == 9 ~ 0))

test_completa <- test_completa %>%
  mutate(años_educ3 = case_when(Educ3 == 1 ~ 0,
                                Educ3 == 2 ~ 2,
                                Educ3 == 3 ~ 7,
                                Educ3 == 4 ~ 10,
                                Educ3 == 5 ~ 13,
                                Educ3 == 6 ~ 18,
                                Educ3 == 9 ~ 0))

test_completa$años_educ_promedio <- rowMeans(subset(test_completa, select= c("años_educ1", "años_educ2", "años_educ3")),na.rm=TRUE)

#Alguien recibe arriendo
test_personas$P7495 <- 2 - test_personas$P7495

test_arriendo <- test_personas%>%subset(P6040>=10)%>%group_by(id)%>%summarise(suma = sum(P7495, na.rm = TRUE))%>%mutate(recibe_arriendos = ifelse(suma > 0, 1, 0))

test_completa <- full_join(test_completa, test_arriendo)

test_completa$Valor_Arriendo <- ifelse(is.na(test_completa$P5130),test_completa$P5140, test_completa$P5130)

#Edad del jefe del hogar al cuadrado
test_completa$age2 <- test_completa$P6040^2
#data$age_mujer <- data$P6040*data$jefe_mujer
#data$age2_mujer <- data$age2*data$jefe_mujer


test_completa$prop_cotiza <- test_completa$Cant_cotiza_recibe/test_completa$Num_pet_hogar
test_completa$ppc <- test_completa$Nper/test_completa$P5010

#Guardar base
write.csv(test_completa, "test_completa.csv")

#Abrir base

test_completa <- read.csv("test_completa.csv")
