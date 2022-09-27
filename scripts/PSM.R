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
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here, hablar, plyr)

#Importar bases de datos
train_completa <- read_csv("train_completa.csv")
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

lista <- c("P5100","P5130","P5140","años_educ2","años_educ3")
names(datospsm)

categoricas <- c("Clase", "Dominio", "P5090", "Depto", "jefe_mujer", "P6090", "jefe_cotiza", "relab_jefe",
                 "P6090", "jefe_cotiza", "relab_jefe", "max_edu_lev_h", "max_empl", "Relab1", "Relab2", "Relab3",
                 "Educ1", "Educ2", "Educ3", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen",
                 "recibe_arriendos")

for (var in categoricas) {
  datospsm[,var] <- as.factor(datospsm[,var, drop = TRUE])
}

datospsm <- datospsm[, -c(7:9,12:14,16,22,55:57)]
names(datospsm)

datospsm <- datospsm%>%subset(Relab1!=13)

logit <- glm(formula = psm ~ . -id, family=binomial(link="logit") , data=datospsm)
resultados <- predict(logit, newdata=datospsm, type="response")

resultados <- as.data.frame(resultados)
resultados$psm <- datospsm$psm
resultados$id <- datospsm$id
resultados$combinado <- resultados$resultados*resultados$psm
resultados$combinado <- ifelse(resultados$combinado == 0, NA, resultados$combinado)
minimo <- min(resultados$combinado, na.rm = TRUE)

train_definitva <- resultados%>%subset(psm == 0 & resultados > minimo)
train_definitva <- train_definitva%>%select("id")
train_definitva <- left_join(train_definitva, datospsm)

mean(train_definitva$Horas_Hogar)
mean(train_completa$Horas_Hogar)
mean(test_completa$Horas_Hogar)

prop.table(table(train_definitva$Clase))
prop.table(table(train_completa$Clase))
prop.table(table(test_completa$Clase))

