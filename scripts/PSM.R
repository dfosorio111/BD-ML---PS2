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
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here, hablar, plyr)

#Importar bases de datos
train_completa <- read_csv("data/train_completa.csv")
test_completa <- read_csv("test_completa.csv")

#Revisar nombres de las variables
names(train_completa)
names(test_completa)

#Quitar variables que no nos importan
test_completa <- test_completa[-c(1)]
train_completa <- train_completa[-c(1:8, 25, 58, 59)]

#Crear un indicador para cada observación que sea 0 si viene del train y 1 del test
train_completa$psm <- 0
test_completa$psm <- 1

#Juntar ambas bases
datospsm <- rbind.fill(train_completa, test_completa)

write_csv(datospsm, "datospsm.csv")

#Revisar que no haya NAs
names(which(colSums(is.na(datospsm))>0))


