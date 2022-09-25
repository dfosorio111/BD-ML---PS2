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

#max(train_s$prop_cotiza)

names(train_s)
x_train_s = train_s_under[-c(1,11,35,106,112)]
x_test = test_s[-c(1,11,35,106,112)]

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

train_s_under <- as.data.frame(train_s_under)

write.csv(train_s_under,"train_s_under.csv")

prop.table(table(train_s$Pobre1))
prop.table(table(train_s_under$Pobre1))
prop.table(table(test_s$Pobre1))

train_s_under$log_ingtot

y_train <- train_s_under[,"log_ingtot"]
y_test <-  test_s[,"log_ingtot"]
p_test <-  test_s[,"Pobre1"]
#X_train <- select(train, -c("Lp", "Pobre", "Ingtotugarr", "Npersug"))





#Iniciamos con la regresión lineal

names(train_s_under)

modelo1 <- lm(formula = log_ingtot ~. -Clase1-Pobre1-Lp-Ingtotugarr , data = train_s_under)
insample1 <- predict(modelo1, train_s)
y_hat_test1 <- predict(modelo1, test_s)


df_coeficientes_reg <- modelo1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes_reg %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
             y = coeficiente)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes del modelo de regresión", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()

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


#agregado
train_s_under$jefe_mujer1

names(train_s_under)

x_train_s = train_s_under[c("P6040", "age2", "jefe_mujer1", "max_edu_lev_h2", "max_edu_lev_h3",
                            "max_edu_lev_h4", "max_edu_lev_h5", "max_edu_lev_h6", "max_empl1",
                            "max_empl2", "max_empl3", "max_empl4", "max_empl5", "max_empl6",
                            "max_empl7", "max_empl8", "max_empl9", "Relab12", "Relab13", 
                            "Relab14", "Relab15", "Relab16", "Relab17", "Relab18", "Relab19",
                            "Relab110", "Relab111", "Relab112", "Relab113")]
x_test = test_s[c("P6040", "age2", "jefe_mujer1", "max_edu_lev_h2", "max_edu_lev_h3",
                  "max_edu_lev_h4", "max_edu_lev_h5", "max_edu_lev_h6", "max_empl1",
                  "max_empl2", "max_empl3", "max_empl4", "max_empl5", "max_empl6",
                  "max_empl7", "max_empl8", "max_empl9", "Relab12", "Relab13", 
                  "Relab14", "Relab15", "Relab16", "Relab17", "Relab18", "Relab19",
                  "Relab110", "Relab111", "Relab112", "Relab113")]

#Lasso y Ridge para regresión lineal
# Ridge. Alpha = 0
modelo_ridge <- glmnet(
  x = x_train_s,
  y = y_train,
  alpha = 0,
  lambda = seq(0,5,0.001),
  standardize = FALSE
)



y_hat_train_ridge <- predict(modelo_ridge, 
                              newx = as.matrix(x_train_s))

y_hat_test_ridge <- predict(modelo_ridge, 
                       newx = as.matrix(x_test))

p_hat_test <- ifelse(exp(predict(modelo_ridge, newx = as.matrix(x_test)))/test_s$Npersug < test_s$Lp,1,0)
  
  
#Lambdas del modelo ridge
lambdas_ridge <- modelo_ridge$lambda

#Validación cruzada
resultados_ridge <- data.frame()
for (i in 1:length(lambdas_ridge)) {
  lreg <- lambdas_ridge[i]
  y_hat_test_pred_ridge <- y_hat_test_ridge[, i]
  p_hat_test_pred_ridge <- p_hat_test[, i]
  r23 <- R2_Score(y_pred = y_hat_test_pred_ridge, y_true = y_test)
  rmse3 <- RMSE(y_pred = y_hat_test_pred_ridge, y_true = y_test)
  cm_ridge_test <- confusionMatrix(data=factor(p_hat_test_pred_ridge) , 
                            reference=factor(p_test) , 
                            mode="sens_spec" , positive="1")
  recall <- cm_ridge_test$byClass[[1]]
  resultado <- data.frame(Modelo = "Ridge",
                          Muestra = "Fuera",
                          Lambda = lreg,
                          R2_Score = r23, 
                          RMSE = rmse3,
                          RECAL = recall)
  resultados_ridge <- bind_rows(resultados_ridge, resultado)
}

ggplot(resultados_ridge, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


filtro <- resultados_ridge$RMSE == min(resultados_ridge$RMSE)
mejor_lambda_ridge <- resultados_ridge[filtro, "Lambda"]

filtro_recall <- resultados_ridge$RECAL == max(resultados_ridge$RECAL)
mejor_lambda_ridge_recall <- resultados_ridge[filtro_recall, "Lambda"]

y_hat_out3 <- predict.glmnet(modelo_ridge,
                             newx = as.matrix(x_test),
                             s = mejor_lambda_ridge_recall)


y_hat_out3 <- as.data.frame(y_hat_out3)
y_hat_out3$ingtot <- exp(y_hat_out3$s1)
y_hat_out3$Lp <- test_s$Lp
y_hat_out3$Pobre <- test_s$Pobre1
y_hat_out3$Npersug <- test_s$Npersug
y_hat_out3$Pred_Pobre <- ifelse(y_hat_out3$ingtot/y_hat_out3$Npersug < y_hat_out3$Lp, 1, 0)

cm_lmridge_test <- confusionMatrix(data=factor(y_hat_out3$Pred_Pobre) , 
                          reference=factor(y_hat_out3$Pobre) , 
                          mode="sens_spec" , positive="1")
cm_lmridge_test

names(data)

#Logit
logit <- glm(formula = Pobre1 ~ . -Clase1-log_ingtot-Lp-Ingtotugarr, family=binomial(link="logit") , data=train_s_under)
tidy(logit)
resultados$pobre_log <- predict(logit , newdata=train_s, type="response")

rule <- 0.5
resultados$pred_log <- ifelse(resultados$pobre_log >= rule, 1, 0)


cm_log <- confusionMatrix(data=factor(resultados$pred_log) , 
                          reference=factor(resultados$Pobre1) , 
                          mode="sens_spec" , positive="1")
cm_log



#Fuera de muestra
resultados2$pobre_log_test <- predict(logit , newdata=test_s , type="response")

rule <- 0.5
resultados2$pred_log_test <- ifelse(resultados2$pobre_log_test >= rule, 1, 0)


cm_log2 <- confusionMatrix(data=factor(resultados2$pred_log_test) , 
                          reference=factor(resultados2$Pobre1) , 
                          mode="sens_spec" , positive="1")
cm_log2


#Lo primero importante es conseguir un sensitivity alto, el del logit dio 0.53, specificity de 0.95 y accuracy de 0.86

Agregado_log <- cm_log$byClass[[1]]*0.75 +cm_log$byClass[[2]]*0.25 #0.63








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

