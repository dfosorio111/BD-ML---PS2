#Código de limpieza de datos

#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2/data")

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
test_hogares <- readRDS("data/test_hogares.Rds")

train_personas <- readRDS("data/train_personas.Rds")
test_personas <- readRDS("data/test_personas.Rds")


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
#10 es desocupado y 11 es inactivo
train_p_jef_ocu <- train_p%>%subset(P6050==1)%>%mutate(relab_jefe= ifelse(is.na(Oc), ifelse(is.na(Des), 11, 10) , P6430) )%>%select(id, relab_jefe)  #ifelse(Des==1,10,11)     




#Subsidio familiar
train_p%>%subset(P6050==1)%>%count(P6585s3)

#Subsidio educativo
train_p%>%subset(P6050==1)%>%count(P6585s4)
train_p%>%subset(P6050==1)%>%count(Oc, P6585s3)
#Las de subsidios tienen muchos NA


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
train_p_adulto_maxlev_edu <-train_p%>%subset(is.na(P6210)==FALSE)%>%mutate(Nivel_Educ = ifelse(as.numeric(P6210) == 9,0,as.numeric(P6210)))%>%group_by(id)%>%summarise(max_edu_lev_h = max(as.numeric(Nivel_Educ)) ) 


#Máximo tamaño de empresa del hogar
train_p_num_empl <- train_p%>%mutate(num_empleados= ifelse(is.na(P6870), 0, P6870))%>%group_by(id)%>%summarise(max_empl= max(as.numeric(num_empleados)))


#Sacar el número de ocupados dentro del hogar (Para posteriormente sacar la proporción de ocupados)
Ocupados_Hogar <- train_p%>%mutate(Ocupados = ifelse(is.na(Oc),0,Oc))%>%group_by(id)%>%summarise(Num_ocu_hogar = sum(Ocupados))
#Número de desocupados
Desocupados_Hogar <- train_p%>%mutate(Desocupados = ifelse(is.na(Des),0,Des))%>%group_by(id)%>%summarise(Num_des_hogar = sum(Desocupados))
#Cantidad de personas en edad de trabajar
Pet_Hogar <- train_p%>%mutate(PobEdTrab = ifelse(is.na(Pet),0,Pet))%>%group_by(id)%>%summarise(Num_pet_hogar = sum(PobEdTrab))



#Se busca hacer un "pivot" de aquellas variables que pueden resultar útil tener para más de un individuo del hogar

if_else(is.na(P6430),if_else(is.na(Des), 11, 12),as.double(P6430))



#Inicialmente con la relación laboral
#10 hace referencia a individuos estudiando, 11 a inactivos no estudiando, 12 a desocupados
train_relab <- train_p%>%mutate(relacion_lab = ifelse(as.numeric(P6240) == 3, 10, if_else(is.na(P6430),if_else(is.na(Des), 11, 12),as.double(P6430))))%>%select(id, Orden, relacion_lab, Oc, Des, Ina, P6430, P6240, P6040, P6050)
#Los niños no clasifican a nada, se les dará valor de 13
train_relab$relacion_lab <- ifelse(is.na(train_relab$relacion_lab), 13, train_relab$relacion_lab)
prueba <- train_relab%>%subset(relacion_lab == 13)
max(prueba$P6040)
train_relab_def <- train_relab%>%select(id, Orden, relacion_lab)
reshape_relab <- pivot_wider(train_relab_def, names_from = Orden, values_from = relacion_lab)


reshape_relab_ninis <- data.frame()
  
for(i in 1:nrow(reshape_relab)){
  
  reshape_relab_ninis[i,] <- sum(reshape_relab[i,] == 11, na.rm = TRUE) 

}

reshape_relab_ninis$id <- reshape_relab$id




reshape_relab <- reshape_relab%>%select("id", `1`, `2`, `3`)
#Los NA son gente que no existe, esos no existe se les dará valor de 0
reshape_relab$`2` <- ifelse(is.na(reshape_relab$`2`),0,reshape_relab$`2`)
reshape_relab$`3` <- ifelse(is.na(reshape_relab$`3`),0,reshape_relab$`3`)

colnames(reshape_relab) <- c("id", "Relab1", "Relab2", "Relab3")

#Nivel educativo
#Los NA del nivel educativo se ponen porque todos hacen referencia a personas menores de edad no ocupados se les dará un valor de 10 (da igual porque es categórica.

train_p%>%subset(P6040 >= 18)%>%count(P6210)
train_p%>%subset(Oc == 1)%>%count(P6210)


train_educ <- train_p%>%mutate(educacion = if_else(is.na(P6210),10,as.double(P6210)))%>%select(id, Orden, educacion)
is.na(train_educ$educacion)%>%table()
reshape_educ <- pivot_wider(train_educ, names_from = Orden, values_from = educacion)

#También arbitrariamente se escogen los 3 primeros
reshape_educ <- reshape_educ%>%select("id", `1`, `2`, `3`)

#Los NA son gente que no existe, esos no existe se les dará valor de 0
reshape_educ$`2` <- ifelse(is.na(reshape_educ$`2`),0,reshape_educ$`2`)
reshape_educ$`3` <- ifelse(is.na(reshape_educ$`3`),0,reshape_educ$`3`)

colnames(reshape_educ) <- c("id", "Educ1", "Educ2", "Educ3")



#Variable de pensión
train_p%>%subset(P6040 >= 18)%>%count(P7500s2, P6920)

train_pension <- train_p%>%mutate(cotiza_recibe = ifelse(P6920 == 1 | P6920 == 3 | P7500s2 == 1, 1,0))%>%select(id, Orden, cotiza_recibe)
train_pension$cotiza_recibe <- ifelse(is.na(train_pension$cotiza_recibe ), 0, train_pension$cotiza_recibe )
train_pension%>%count(cotiza_recibe)
reshape_pension <- pivot_wider(train_pension, names_from = Orden, values_from = cotiza_recibe)
reshape_pension$Cant_cotiza_recibe <- apply(reshape_pension[2:ncol(reshape_pension)],1,sum, na.rm = TRUE) 

reshape_pension <- reshape_pension%>%select("id", "Cant_cotiza_recibe")


train_p%>%subset(Des == 1)%>%count(P7310)


#En el paper de Samu utilizan el estatus marital de la personas jefe de hogar
#Acá eso no se puede, pero podemos saber si la persona jefe de hogar vive con su pareja, hijos o nietos
train_p%>%count(P6050)

train_miembros <- train_p%>%select(id, Orden, P6050) 
reshape_miembros <- pivot_wider(train_miembros, names_from = Orden, values_from = P6050)

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



reshape_miembros <- reshape_miembros%>%select("id", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen")

#Merge de personas y hogares

train_completa <- train_h
train_completa <- full_join(train_completa, train_p_num_muj)
train_completa <- full_join(train_completa, train_p_jef_muj)
train_completa <- full_join(train_completa, train_p_edadjefe)
train_completa <- full_join(train_completa, train_p_jefecotiza)
train_completa <- full_join(train_completa, train_p_jef_ocu)
train_completa <- full_join(train_completa, union_horas)
train_completa <- full_join(train_completa, train_p_adulto_maxlev_edu)




table(train_completa$Pobre, train_completa$max_edu_lev_h)

#Hay solo 3 en No sabe, no responde, arbitrariamente los clasificaré en "Ninguno"
train_completa$max_edu_lev_h <- ifelse(train_completa$max_edu_lev_h == 0, 1, train_completa$max_edu_lev_h)

train_completa <- full_join(train_completa, train_p_num_empl)
train_completa <- full_join(train_completa, Ocupados_Hogar)
train_completa <- full_join(train_completa, Desocupados_Hogar)
train_completa <- full_join(train_completa, Pet_Hogar)

#Relación laboral de los primeros 3 miembros del hogar ¿Por qué 3? realmente porque ajá
#10 estudiando, 11 inactivo no estudiando, 12 desocupado, 13 niños (no aplican)
train_completa <- full_join(train_completa, reshape_relab)

#Nivel educativo (se respetan las categorías originales, 10 es para NA y 0 para quienes no existen)
train_completa <- full_join(train_completa, reshape_educ)

#Se añade la cantidad de personas que cotiza o recibe pensión en el hogar
train_completa <- full_join(train_completa, reshape_pension)

#Seadiciona los miembros que viven con esas personas
train_completa <- full_join(train_completa, reshape_miembros)

# Se adiciona el numero de NINIS por hogar
train_completa <- full_join(train_completa, reshape_relab_ninis)


#La P6090 tiene unos NA pero son niños, entonces se les pondrá que no están en seguridad social en salud
train_completa%>%count(P6090)
train_completa$P6090 <- ifelse(is.na(train_completa$P6090), 2, train_completa$P6090)

#Variables de proporciones. (Más que la cantidad nos interesa la proporción)
train_completa$prop_ocupados_total <- train_completa$Num_ocu_hogar/train_completa$Nper
train_completa$prop_ocupados_pet <- train_completa$Num_ocu_hogar/train_completa$Num_pet_hogar

ggplot(data = train_completa, aes(x = factor(Pobre), y = prop_ocupados_pet))+
  geom_boxplot()


train_completa$prop_Desocupados_total <- train_completa$Num_des_hogar/train_completa$Nper
train_completa$prop_Desocupados_pet <- train_completa$Num_des_hogar/train_completa$Num_pet_hogar

ggplot(data = train_completa, aes(x = factor(Pobre), y = prop_Desocupados_pet))+
  geom_boxplot()

train_completa%>%count(prop_Desocupados_pet)

#Mujeres
train_completa$prop_mujeres_total <- train_completa$num_mujeres/train_completa$Nper
train_completa$prop_mujeres_pet <- train_completa$num_mujeres/train_completa$Num_pet_hogar

ggplot(data = train_completa, aes(x = factor(Pobre), y = prop_mujeres_pet))+
  geom_boxplot()


ggplot(data = train_completa, aes(x = factor(Pobre), y = P6040))+
  geom_boxplot()


train_completa <- read.csv("train_completa.csv")

train_completa$Ingpcug <- train_hogares$Ingpcug
train_completa$Ingtotugarr <- train_hogares$Ingtotugarr
write.csv(train_completa, "train_completa.csv")

write.csv(reshape_miembros, "reshapemiembros.csv")
reshape_miembros <- read.csv("reshapemiembros.csv")

#Se anexan aquellas variables que faltaba anexar (Luego pasar esto arriba)

setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning")



#Adicionales

#Alguien recibe arriendo
train_p$P7495 <- 2 - train_p$P7495

train_arriendo <- train_p%>%subset(P6040>=10)%>%group_by(id)%>%summarise(suma = sum(P7495, na.rm = TRUE))%>%mutate(recibe_arriendos = ifelse(suma > 0, 1, 0))


data <- read.csv("train_completa.csv")

train_completa <- data

data <- full_join(data, train_arriendo)


names(data)
data$Valor_Arriendo <- ifelse(is.na(data$P5130),data$P5140, data$P5130)
data <- data[-c(1:5,12:14,17,19,21)]

#Las variables que son factores ponerlas como factores
names(data)
#Edad del jefe del hogar al cuadrado
data$age2 <- data$P6040^2
#data$age_mujer <- data$P6040*data$jefe_mujer
#data$age2_mujer <- data$age2*data$jefe_mujer

categoricas <- c("Pobre", "Clase", "Dominio", "P5090", "Depto", "jefe_mujer", "P6090", "jefe_cotiza", "relab_jefe",
                 "P6090", "jefe_cotiza", "relab_jefe", "max_edu_lev_h", "max_empl", "Relab1", "Relab2", "Relab3",
                 "Educ1", "Educ2", "Educ3", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen",
                 "recibe_arriendos")

data$Cant_cotiza_recibe
data$Num_pet_hogar

View(data%>%select("id","Cant_cotiza_recibe", "Num_pet_hogar", "prop_cotiza"))

for (var in categoricas) {
  data[,var] <- as.factor(data[,var, drop = TRUE])
}


data$prop_cotiza <- data$Cant_cotiza_recibe/data$Num_pet_hogar
data$ppc <- data$Nper/data$P5010


sum(is.na(train_completa$Educ3))

train_completa <- train_completa %>%
  mutate(años_educ1 = case_when(Educ1 == 1 ~ 0,
                                Educ1 == 2 ~ 2,
                                Educ1 == 3 ~ 7,
                                Educ1 == 4 ~ 10,
                                Educ1 == 5 ~ 13,
                                Educ1 == 6 ~ 18,
                                Educ1 == 9 ~ 0))

train_completa<- train_completa %>%
  mutate(años_educ2 = case_when(Educ2 == 1 ~ 0,
                                Educ2 == 2 ~ 2,
                                Educ2 == 3 ~ 7,
                                Educ2 == 4 ~ 10,
                                Educ2 == 5 ~ 13,
                                Educ2 == 6 ~ 18,
                                Educ2 == 9 ~ 0))

train_completa <- train_completa %>%
  mutate(años_educ3 = case_when(Educ3 == 1 ~ 0,
                                Educ3 == 2 ~ 2,
                                Educ3 == 3 ~ 7,
                                Educ3 == 4 ~ 10,
                                Educ3 == 5 ~ 13,
                                Educ3 == 6 ~ 18,
                                Educ3 == 9 ~ 0))


train_completa$años_educ_promedio <- rowMeans(subset(train_completa, select= c("años_educ1", "años_educ2", "años_educ3")),na.rm=TRUE)



write.csv(data, "train_completa.csv")
write.csv(train_completa, "train_completa.csv")
#Revisar si vale la pena agregar si reciben ingresos por arriendos


