####Primeros modelos, Daniel Franco.


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
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here,
       modelsummary, # tidy, msummary
       gamlr,        # cv.gamlr
       ROCR, # ROC curve
       class, glmnet)

data <- read.csv("train_completa.csv")
names(data)
data$Valor_Arriendo <- ifelse(is.na(data$P5130),data$P5140, data$P5130)
data <- data[-c(1:3,10:12,15,17,19)]

#Las variables que son factores ponerlas como factores
names(data)
data$age2 <- data$P6040^2
#data$age_mujer <- data$P6040*data$jefe_mujer
#data$age2_mujer <- data$age2*data$jefe_mujer
data$Clase <- as.factor(data$Clase)
data$Dominio <- as.factor(data$Dominio)
data$P5090 <- as.factor(data$P5090)
data$Depto <- as.factor(data$Depto)
#Ojo con esta variable, hay varios 98 y 99 que podrían asignarse mejor con KNN

data$jefe_mujer <- as.factor(data$jefe_mujer)
data$P6090 <- as.factor(data$P6090)
#Recordar que esta es afiliación a seguridad social en salud
data$jefe_cotiza <- as.factor(data$jefe_cotiza)
data$relab_jefe <- as.factor(data$relab_jefe)
data$max_edu_lev_h <- as.factor(data$max_edu_lev_h)
data$max_empl<- as.factor(data$max_empl)
data$Relab1 <- as.factor(data$Relab1)
data$Relab2 <- as.factor(data$Relab2)
data$Relab3 <- as.factor(data$Relab3)
data$Educ1 <- as.factor(data$Educ1)
data$Educ2 <- as.factor(data$Educ2)
data$Educ3 <- as.factor(data$Educ3)
data$hijos <- as.factor(data$hijos)
data$pareja <- as.factor(data$pareja)
data$nietos <- as.factor(data$nietos)
data$otros_parientes <- as.factor(data$otros_parientes)
data$no_parientes <- as.factor(data$no_parientes)
data$emp_pen <- as.factor(data$emp_pen)
data$prop_cotiza <- data$Cant_cotiza_recibe/data$Num_pet_hogar
data$ppc <- data$Nper/data$P5010


names(data)

plot(data$ppc, log(data$Ingpcug))

is.na(data$P5090)%>%table()

data <- data[c("Lp","Pobre", "Clase", "jefe_mujer", "max_edu_lev_h", "max_empl", "Horas_Hogar", "P6040", "age2", "prop_ocupados_pet", "relab_jefe", "prop_cotiza", "Ingpcug", "Valor_Arriendo", "Relab2", "nietos", "no_parientes", "otros_parientes", "prop_mujeres_pet", "ppc", "P5090")]

#Se crea la matriz que le gusta a Ignacio
df <- model.matrix(~ .  - 1, data)


# Dividimos train/test (70/30)
n <- nrow(df)
smp_size <- floor(0.7*n)
train_ind <- sample(1:n, size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]





# Estandarizamos DESPUÉS de partir la base en train/test
variables_numericas <- c( "P6040", "Horas_Hogar",
                          "prop_ocupados_pet", "age2", "prop_cotiza", "Valor_Arriendo")
escalador <- preProcess(train[, variables_numericas])
train_s <- train
test_s <- test

train_s[, variables_numericas] <- predict(escalador, train[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, test[, variables_numericas])

train_s <- data.frame(train_s)
test_s <- data.frame(test_s)
train <- data.frame(train)
test <- data.frame(test)

names(train_s)

y_train <- train_s[,"Ingpcug"]
X_train <- select(train, -c("Ingpcug", "Lp", "Pobre"))

#Iniciamos con la regresión lineal

names(train_s)

modelo1 <- lm(formula = Ingpcug ~ . -1-Lp-Pobre, data = train_s)
insample1 <- predict(modelo1, train_s)

train_s$prediccion <- insample1
train_s$pobre_lm <- ifelse(train_s$prediccion <= train_s$Lp, 1, 0)

tabla_lm <- train_s%>%select(Pobre, pobre_lm)%>%table()

cm_lm <- confusionMatrix(data=factor(train_s$pobre_lm) , 
                          reference=factor(train_s$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_lm


#En este modelo el sesnsitivity es bajo(0.53), pero tenemos un specificty bueno (0.89) y un accuracy decennte (0.82)
Agregado_lm <- cm_lm$byClass[[1]]*0.75 +cm_lm$byClass[[2]]*0.25  #0.62





names(data)

#Logit
logit <- glm(formula = factor(Pobre) ~ . - 1- Lp - Ingpcug, family=binomial(link="logit") , data=train_s)
tidy(logit)
train_s$pobre_log <- predict(logit , newdata=train_s , type="response")

rule <- 0.5
train_s$pred_log <- ifelse(train_s$pobre_log >= rule, 1, 0)
tabla_log <- train_s%>%select(Pobre, pred_log)%>%table()


cm_log <- confusionMatrix(data=factor(train_s$pred_log) , 
                          reference=factor(train_s$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_log
#Lo primero importante es conseguir un sensitivity alto, el del logit dio 0.52, specificity de 0.94 y accuracy de 0.86

Agregado_log <- cm_log$byClass[[1]]*0.75 +cm_log$byClass[[2]]*0.25 #0.62




####Lasso y Ridge para el lineal


modelo_ridge <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 0,
  nlambda = 300,
  standardize = FALSE
)

predicciones <- train_s$Pobre
predicciones <- cbind(predicciones,train_s$Ingpcug, train_s$Lp)
predicciones <- as.data.frame(predicciones)
colnames(predicciones) <- c("PobrezaReal", "IngpcReal", "Lp")

predicciones_ridge <- predict(modelo_ridge, 
                              newx = as.matrix(X_train))

lambdas_ridge <- modelo_ridge$lambda

resultados_ridge <- data.frame()
for (i in 1:length(lambdas_ridge)) {
  l <- lambdas_ridge[i]
  y_hat_out3 <- predicciones_ridge[, i]
  r23 <- R2_Score(y_pred = y_hat_out3, y_true = y_train)
  rmse3 <- RMSE(y_pred = y_hat_out3, y_true = y_train)
  resultado <- data.frame(Modelo = "Ridge",
                          Muestra = "Dentro",
                          Lambda = l,
                          R2_Score = r23, 
                          RMSE = rmse3)
  resultados_ridge <- bind_rows(resultados_ridge, resultado)
}

ggplot(resultados_ridge, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  theme_test() +
  scale_y_continuous(labels = scales::comma)


ggplot(resultados_ridge, aes(x = Lambda, y = R2_Score)) +
  geom_point() +
  geom_line() +
  theme_test() +
  scale_y_continuous(labels = scales::comma)

filtro <- resultados_ridge$RMSE == min(resultados_ridge$RMSE)
mejor_lambda_ridge <- resultados_ridge[filtro, "Lambda"]

resultados_ridge%>%head()

resultados_ridge$Lambda

# Guardamos el mejor Ridge
y_hat_in3<- predict.glmnet(modelo_ridge,
                            newx = as.matrix(X_train),
                            s = mejor_lambda_ridge)

y_pobre <- ifelse(y_hat_in3 <= predicciones$Lp, 1, 0)

predicciones$Predicho <- y_hat_in3


plot(predicciones$Predicho, predicciones$IngpcReal)

#Ideas sueltas:
#tal vez nos va mejor si predecimos el ingreso total, teniendo en cuenta el número de personas y de ahí se calcula el per cápita y la pobreza

#Cotiza o recibe pensión
ggplot(data = data, aes(x = factor(Pobre), y = prop_cotiza))+
  geom_boxplot()

ggplot(data = data, aes(x = factor(Pobre), y = prop_ocupados_pet))+
  geom_boxplot()

ggplot(data = data, aes(x = factor(Pobre), y = ppc))+
  geom_boxplot()

dm <- data%>%
  group_by(jefe_mujer, Pobre)%>%
  summarise(n = n())%>%
  mutate(perc = n/sum(n))

ggplot(data = dm, aes(x = factor(jefe_mujer), y = perc, fill = factor(Pobre)))+
  geom_bar(stat="identity", width = 0.7)


dc <- data%>%
  group_by(Clase, Pobre)%>%
  summarise(n = n())%>%
  mutate(perc = n/sum(n))

ggplot(data = dc, aes(x = factor(Clase), y = perc, fill = factor(Pobre)))+
  geom_bar(stat="identity", width = 0.7)
