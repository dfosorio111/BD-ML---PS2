#Código de modelos de ML

#Limpiar el ambiente
rm(list=ls())

# importar librerias
#Importar paquetes y cargar librerías
require(pacman)
p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe, 
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here, AER, tidymodels, themis)



#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2/data")

# cargar bases para prediccion

# leer la base del archivo .csv
train_hogares <- read.csv("train_hogares.csv")


train_hogares <- data.frame(train_hogares)

# Check los NAs de la base
sapply(train_hogares, function(x) sum(is.na(x)))


# ver distribucion de la variable a predecir: Pobre
prop.table(table(train_hogares$Pobre))


# ver variables de la base

names(train_hogares)
# [cont] X: indice 
# [cont] id: id de la observacion
# [disc] Clase: urbano-rural
# [disc] Dominio: ciudad
# [disc] P5000: numero de cuartos
# [disc] P5010: en cuantos cuartos duermen
# [disc] P5090: estado de vivienda
# [cont] P5100: pago cuota amortizacion
# [cont] P5130: pago estimado arriendo
# [cont] P5140: pago arriendo mensual
# [cont] Nper: numero personas 
# [cont] Npersug: numero personas en unidad gasto
# [cont] Li: linea de indigencia
# [cont] Lp: linea de pobreza
# [cont] Fex_c: factor de expansion anualizado
# [disc] Depto: departamento
# [disc] Fex_depto: factor expansion departamental
# [disc] Pobre: pobre
# [cont] num_mujeres: numero de mujeres en el hogar
# [disc] jefe_mujer: mujer es jefe del hogar
# *??? [cont] P6040: años tiene la persona?
# *??? [disc] P6090: la persona esta afiliada a seguridad social en salud
# [disc] jefe_cotiza: jefe del hogar cotiza
# [disc] relab_jefe: relacion laboral del jefe
# [cont] Horas_Hogar: horas totales trabajadas en el hogar
# [cont] Horas_reales: horas trabajadas jefe del hogares

# convertir variables discretas (categoricas y dicotomas) a factores
 
train_hogares$Clase <- factor(train_hogares$Clase)
train_hogares$Dominio <- factor(train_hogares$Dominio)
train_hogares$P5000 <- factor(train_hogares$P5000)
train_hogares$P5010 <- factor(train_hogares$P5010)
train_hogares$P5000 <- factor(train_hogares$P5090)
train_hogares$Depto <- factor(train_hogares$Depto)
train_hogares$Pobre <- factor(train_hogares$Pobre)
train_hogares$jefe_mujer <- factor(train_hogares$jefe_mujer)
train_hogares$P6090 <- factor(train_hogares$P6090)
train_hogares$jefe_cotiza <- factor(train_hogares$jefe_cotiza)
train_hogares$relab_jefe <- factor(train_hogares$relab_jefe)


# Dummyficar la base ANTES de hacer split en train-set y test-set 
# matiz de Igancio
#df_ml <- model.matrix(~., train_hogares)

df_ml <-train_hogares


# crear variables para los modelos

df_ml <- df_ml%>%mutate(edad2_jefe=P6040^2)








# dividir la base en train-set y test-set
set.seed(1000)

# 80% train-set, 20% test-set
id_train <- sample(1:nrow(df_ml), size=0.8*nrow(df_ml), replace=FALSE)

# split dataset en train-set y test-set para entrenar el modelo y realizar predicciones
test_set <- df_ml[-id_train,]
train_set <- df_ml[id_train,]


# estandarizar variables discretas con escalador después de hacer split de train-set y test-set
# aplicar escalar a x-train y x-test

vars_cont <- c("P5100","P5130","P5140","Nper","Npersug","Li","Lp","Fex_c","num_mujeres","P6040","Horas_Hogar","Horas_reales")

# crear escalador para las variables continuas
escalador <- preProcess(train_set[, vars_cont])

train_set[, vars_cont] <- predict(escalador, train_set[, vars_cont])
test_set[, vars_cont] <- predict(escalador, test_set[, vars_cont])




# train_s <- data.frame(train_s)
# test_s <- data.frame(test_s)
# train <- data.frame(train)
# test <- data.frame(test)





#Modelos:

#LinearReg
modelo1 <- lm(y_def_2~P, data = train_base1)
summary(modelo1)



