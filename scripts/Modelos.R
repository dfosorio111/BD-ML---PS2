####Primeros modelos, Daniel Franco.


#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2/data")


#Importar paquetes y cargar librerías
require(pacman)
p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe, 
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here,
       modelsummary, # tidy, msummary
       gamlr,        # cv.gamlr
       ROCR, # ROC curve
       class, glmnet, janitor, doParallel, rattle, fastDummies, tidymodels, themis, AER)

data <- read.csv("train_completa.csv")

data$prop_ocupados_pet <- ifelse(is.na(data$prop_ocupados_pet),0, data$prop_ocupados_pet)
data$prop_Desocupados_pet <- ifelse(is.na(data$prop_Desocupados_pet),0, data$prop_Desocupados_pet)
data$prop_cotiza <- ifelse(is.na(data$prop_cotiza),0, data$prop_cotiza)


# Check los NAs de la base
sapply(data, function(x) sum(is.na(x)))

#Revisemos qué variables tenemos
names(data)

# evaluar correlacion entre variables
cor(data$Cant_cotiza_recibe, data$prop_cotiza)




#ingreso en logaritmo
data$log_ingtot <- log(data$Ingtotugarr+1)


is.na(data$P5100)%>%table()

categoricas <- c("Pobre", "Clase", "Dominio", "P5090", "Depto", "jefe_mujer", "P6090", "jefe_cotiza", "relab_jefe",
                 "P6090", "jefe_cotiza", "relab_jefe", "max_edu_lev_h", "max_empl", "Relab1", "Relab2", "Relab3",
                 "Educ1", "Educ2", "Educ3", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen",
                 "recibe_arriendos")

for (var in categoricas) {
  data[,var] <- as.factor(data[,var, drop = TRUE])
}

data <- data[-c(1:8,10,14:17,19,21,23,28,30,37,41:44,51,53,56,57,59)]


#Se crea la matriz de dummys
df <- model.matrix(~ .  - 1, data)%>%data.frame()

write.csv(df,"dfdummy.csv")

# Dividimos train/test (70/30)

#Se establece semilla
set.seed(1000)
n <- nrow(df)
smp_size <- floor(0.8*n)
train_ind <- sample(1:n, size = smp_size)
#Crear train set para ajustar los parámetros
train <- df[train_ind, ]
#Crear test set para evaluar el modelo
test <- df[-train_ind, ]





# Estandarizamos DESPUÉS de partir la base en train/test

names(df)
names(data)
variables_numericas <- c("P5000", "P5010", "num_mujeres", "P6040", "Horas_Hogar", "Horas_reales",
                         "Num_ocu_hogar", "Num_des_hogar", 
                         "prop_ocupados_pet",
                         "prop_Desocupados_pet", "prop_mujeres_total",
                         "prop_cotiza", "ppc", "Valor_Arriendo", "age2")
                        


escalador <- preProcess(train[, variables_numericas])
train_s <- train
test_s <- test

train_s[, variables_numericas] <- predict(escalador, train[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, test[, variables_numericas])

train_s <- data.frame(train_s)
test_s <- data.frame(test_s)
train <- data.frame(train)
test <- data.frame(test)


write.csv(train_s,"train_s.csv")
write.csv(train_s,"tets_s.csv")

#Oversampling
train_s$Pobre1 <- factor(train_s$Pobre1)
test_s$Pobre1 <- factor(test_s$Pobre1)

sum(is.na(train_s))

train_s_over <- recipe(Pobre1~., data = train_s)%>%
  themis::step_smote(Pobre1)%>%
  prep()%>%
  bake(new_data = NULL)

###Undersampling
train_s_under <- recipe(Pobre1~., data = train_s)%>%
  themis::step_downsample(Pobre1, under_ratio = 1.5)%>%
  prep()%>%
  bake(new_data = NULL)

prop.table(table(train_s$Pobre1))
prop.table(table(train_s_under$Pobre1))
prop.table(table(test_s$Pobre1))

#y_train <- train_s[,"Ingtotugarr"]
#X_train <- select(train, -c("Lp", "Pobre", "Ingtotugarr", "Npersug"))





#Iniciamos con la regresión lineal

names(train_s_under)

modelo1 <- lm(formula = log_ingtot ~. -Clase1-Pobre1-Lp-Ingtotugarr , data = train_s_under)
insample1 <- predict(modelo1, train_s)
y_hat_test1 <- predict(modelo1, test_s)


#Dentro de muestra
resultados <- train_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
resultados$pred_lm <- exp(insample1)
resultados$pobre_lm <- ifelse(resultados$pred_lm/resultados$Npersug <= resultados$Lp, 1, 0)


cm_lm <- confusionMatrix(data=factor(resultados$pobre_lm) , 
                         reference=factor(resultados$Pobre) , 
                         mode="sens_spec" , positive="1")
cm_lm



#Fuera de muestra
resultados2 <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
resultados2$pred_lm <- exp(y_hat_test1)
resultados2$pobre_lm <- ifelse(resultados2$pred_lm/resultados2$Npersug <= resultados2$Lp, 1, 0)



cm_lm2 <- confusionMatrix(data=factor(resultados2$pobre_lm) , 
                         reference=factor(resultados2$Pobre1) , 
                         mode="sens_spec" , positive="1")
cm_lm2



#En este modelo el sesnsitivity es bajo(0.53), pero tenemos un specificty bueno (0.90) y un accuracy decennte (0.83)
Agregado_lm <- cm_lm$byClass[[1]]*0.75 +cm_lm$byClass[[2]]*0.25  #0.63



names(data)

#Logit
logit <- glm(formula = Pobre1 ~ . -Clase1-log_ingtot-Lp-Ingtotugarr, family=binomial(link="logit") , data=train_s)
tidy(logit)
resultados$pobre_log <- predict(logit , newdata=train_s , type="response")

rule <- 0.5
resultados$pred_log <- ifelse(resultados$pobre_log >= rule, 1, 0)


cm_log <- confusionMatrix(data=factor(resultados$pred_log) , 
                          reference=factor(resultados$Pobre1) , 
                          mode="sens_spec" , positive="1")
cm_log
#Lo primero importante es conseguir un sensitivity alto, el del logit dio 0.53, specificity de 0.95 y accuracy de 0.86

Agregado_log <- cm_log$byClass[[1]]*0.75 +cm_log$byClass[[2]]*0.25 #0.63




####Lasso y Ridge para el lineal





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

#Árboles de decisión y random forest

