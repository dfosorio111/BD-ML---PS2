#Código de modelos de ML

#Limpiar el ambiente
rm(list=ls())

# importar librerias
#Importar pquetes y cargar librerías
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
train_hogares <- read.csv("train_completa.csv")
train_hogares <- data.frame(train_hogares)
train_hogares$Valor_Arriendo <- ifelse(is.na(train_hogares$P5130),train_hogares$P5140, train_hogares$P5130)
train_hogares <- train_hogares[-c(1:5,12:14,17,19,21)]

# quitar columnas de la base
train_hogares <- train_hogares[-c(1:5,12:14,17,19,21)]


# Check los NAs de la base
sapply(train_hogares, function(x) sum(is.na(x)))

train_hogares$age2 <- train_hogares$P6040^2

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


# agregar variables
# crear variable age2=age^2 del jefe del hogar
train_hogares$age2 <-train_hogares$P6040^2




# convertir variables discretas (categoricas y dicotomas) a factores

train_hogares$Clase <- as.factor(train_hogares$Clase)
train_hogares$Dominio <- as.factor(train_hogares$Dominio)
train_hogares$P5000 <- as.factor(train_hogares$P5000)
train_hogares$P5010 <- as.factor(train_hogares$P5010)
train_hogares$P5090 <- as.factor(train_hogares$P5090)
train_hogares$Depto <- as.factor(train_hogares$Depto)
train_hogares$Pobre <- as.factor(train_hogares$Pobre)
train_hogares$jefe_mujer <- as.factor(train_hogares$jefe_mujer)
train_hogares$P6090 <- as.factor(train_hogares$P6090)
train_hogares$jefe_cotiza <- as.factor(train_hogares$jefe_cotiza)
train_hogares$relab_jefe <- as.factor(train_hogares$relab_jefe)



train_hogares$age2 <- train_hogares$P6040^2   
train_hogares$age_mujer <- train_hogares$P6040*train_hogares$jefe_mujer
train_hogares$age2_mujer <- train_hogares$age2*train_hogares$jefe_mujer

train_hogares$Clase <- as.factor(train_hogares$Clase)
train_hogares$Dominio <- as.factor(train_hogares$Dominio)
train_hogares$P5090 <- as.factor(train_hogares$P5090)
train_hogares$Depto <- as.factor(train_hogares$Depto)
train_hogares$jefe_mujer <- as.factor(train_hogares$jefe_mujer)
train_hogares$P6090 <- as.factor(train_hogares$P6090)

#Recordar que esta es afiliación a seguridad social en salud
train_hogares$jefe_cotiza <- as.factor(train_hogares$jefe_cotiza)
train_hogares$relab_jefe <- as.factor(train_hogares$relab_jefe)
train_hogares$max_edu_lev_h <- as.factor(train_hogares$max_edu_lev_h)
train_hogares$max_empl<- as.factor(train_hogares$max_empl)
train_hogares$Relab1 <- as.factor(train_hogares$Relab1)
train_hogares$Relab2 <- as.factor(train_hogares$Relab2)
train_hogares$Relab3 <- as.factor(train_hogares$Relab3)
train_hogares$Educ1 <- as.factor(train_hogares$Educ1)
train_hogares$Educ2 <- as.factor(train_hogares$Educ2)
train_hogares$Educ3 <- as.factor(train_hogares$Educ3)
train_hogares$hijos <- as.factor(train_hogares$hijos)
train_hogares$pareja <- as.factor(train_hogares$pareja)
train_hogares$nietos <- as.factor(train_hogares$nietos)
train_hogares$otros_parientes <- as.factor(train_hogares$otros_parientes)
train_hogares$no_parientes <- as.factor(train_hogares$no_parientes)
train_hogares$emp_pen <- as.factor(train_hogares$emp_pen)



train_hogares$prop_cotiza <- train_hogares$Cant_cotiza_recibe/train_hogares$Num_pet_hogar
train_hogares$ppc <- train_hogares$Nper/train_hogares$P5010


# Dummyficar la base ANTES de hacer split en train-set y test-set 
# matiz de Igancio

# las variables de interes para construir el modelo 
train_hogares <- train_hogares[c("Lp","Pobre", "Clase", "jefe_mujer", "max_edu_lev_h", "max_empl", "Horas_Hogar", "P6040", "age2", "prop_ocupados_pet", "relab_jefe", "prop_cotiza", "Ingtotugarr", "Valor_Arriendo", "Relab2", "nietos", "no_parientes", "otros_parientes", "prop_mujeres_pet", "ppc", "P5090", "Npersug")]


# se crea el dataframe que dummyfica las variables
df_ml <- model.matrix(~ .  - 1, train_hogares)




# dividir la base en train-set y test-set
set.seed(1000)

# split la base en train-set y test-st (80/20)
n <- nrow(df_ml)
smp_size <- floor(0.8*n)
train_id <- sample(1:n, size = smp_size)

train_set <- df_ml[train_id, ]
test_set <- df_ml[-train_id, ]



# estandarizar variables discretas para de la base x con escalador después de hacer split de train-set y test-set
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

