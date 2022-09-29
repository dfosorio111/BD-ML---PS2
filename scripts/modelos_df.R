
# Modelos en R Diego

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
       class, glmnet, janitor, doParallel, rattle, fastDummies, tidymodels, themis, AER, tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)

data <- read.csv("train_completa.csv")


# crear variables sobre la base
data$prop_ocupados_pet <- ifelse(is.na(data$prop_ocupados_pet),0, data$prop_ocupados_pet)
data$prop_Desocupados_pet <- ifelse(is.na(data$prop_Desocupados_pet),0, data$prop_Desocupados_pet)
data$prop_cotiza <- ifelse(is.na(data$prop_cotiza),0, data$prop_cotiza)


# Check los NAs de la base
sapply(data, function(x) sum(is.na(x)))

# variables en la base
names(data)

# evaluar correlacion entre variables
cor(data$Cant_cotiza_recibe, data$prop_cotiza)




# crear nueva variables log_ingtot que es logaritmo del ingreso+1
data$log_ingtot <- log(data$Ingtotugarr+1)


is.na(data$P5100)%>%table()


# crear diccionario de variables categoricas
categoricas <- c("Pobre", "Clase", "Dominio", "P5090", "Depto", "jefe_mujer", "P6090", "jefe_cotiza", "relab_jefe",
                 "P6090", "jefe_cotiza", "relab_jefe", "max_edu_lev_h", "max_empl", "Relab1", "Relab2", "Relab3",
                 "Educ1", "Educ2", "Educ3", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen",
                 "recibe_arriendos")

# convertir variables categoricas en factores
for (var in categoricas) {
  data[,var] <- as.factor(data[,var, drop = TRUE])
}

names(data)

# quitar variables con multi colinealidad
data <- data[-c(1:9,11,15:18,20,22,24,29,31,38,42:45,52,54,57,58,60, 66:68)]

#Se crea la matriz de dummys para las variables categoricas
df <- model.matrix(~ .  - 1, data)%>%data.frame()
write.csv(df,"df_dummy.csv")

# dividir la base en train-set y test-set

#Se establece semilla
set.seed(1000)
n <- nrow(df)
smp_size <- floor(0.8*n)
train_ind <- sample(1:n, size = smp_size)
#Crear train set para ajustar los parámetros
train <- df[train_ind, ]
#Crear test set para evaluar el modelo
test <- df[-train_ind, ]





# estandarizar las variables continuas  DESPUÉS de partir la base en train-set y test-set

names(df)
names(data)
variables_numericas <- c("P5000", "P5010", "num_mujeres", "P6040", "Horas_Hogar", "Horas_reales",
                         "Num_ocu_hogar", "Num_des_hogar", 
                         "prop_ocupados_pet",
                         "prop_Desocupados_pet", "prop_mujeres_total",
                         "prop_cotiza", "ppc", "Valor_Arriendo", "age2", "años_educ_promedio")

# crear escalador
escalador <- preProcess(train[, variables_numericas])
train_s <- train
test_s <- test

# crear bases con variables continuas escaladas
train_s[, variables_numericas] <- predict(escalador, train[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, test[, variables_numericas])

train_s <- data.frame(train_s)
test_s <- data.frame(test_s)
train <- data.frame(train)
test <- data.frame(test)



names(train_s)


#write.csv(train_s,"train_s.csv")
#write.csv(train_s,"tets_s.csv")


#Se crean pobre como factores
train_s$Pobre1 <- factor(train_s$Pobre1)
test_s$Pobre1 <- factor(test_s$Pobre1)


###Undersampling
train_s_under <- recipe(Pobre1~., data = train_s)%>%
  themis::step_downsample(Pobre1, under_ratio = 1.5)%>%
  prep()%>%
  bake(new_data = NULL)

train_s_under <- as.data.frame(train_s_under)

#write.csv(train_s_under,"train_s_under.csv")

prop.table(table(train_s$Pobre1))
prop.table(table(train_s_under$Pobre1))
prop.table(table(test_s$Pobre1))




y_train <- train_s_under[,"log_ingtot"]
y_test <-  test_s[,"log_ingtot"]


p_train <- train_s_under[,"Pobre1"]
p_test <-  test_s[,"Pobre1"]




names(train_s_under)
x_train_s = train_s_under[-c(1,11,105,112,113)]
x_test = test_s[-c(1,11,105,112,113)]


# ggplot: histograma de distribucion de la variable a predecir (para variables continuas)
ggplot(df, aes(x = log_ingtot)) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "distribución", y = "log-ingresos") +
  theme_bw() 


#X_train <- select(train, -c("Lp", "Pobre", "Ingtotugarr", "Npersug"))

# Bases para los modelos























# Modelos

#  Regresion Lineal

# crear modelo de regresión lineal simple - sin regularización ni control 
lin1 <- lm(formula = log_ingtot ~. -Clase1-Pobre1-Lp-Ingtotugarr , data = train_s_under)


# análisis preliminar del modelo
summary(lin1)

# analisis descriptivo del modelo como dataframe
tidy(lin1)

# reporte de errores estandar robustos

summ(lin1, robust='HC1')


# reporte con los coeficientes estandarizados
summ(lin1, scale = TRUE )


# análisis descriptivo del modelo
lin1_preds <- lin1$coefficients %>%enframe(name = "predictor", value = "valor")
lin1_res <- lin1$residuals%>%enframe(name = "residual", value = "valor")


# ggplot:  bar grafico de los pesos de los estimadores/predictores
lin1_preds %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(valor)), 
             y = valor)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes del modelo de regresión", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()




lin1_summary_print = summary(lin1)$coefficients
lin1_summary_print[,'t value'] = abs(lin1_summary_print[,'t value'])

pretty_rownames = function(rnames){
  rnames = gsub('^`', '', rnames)
  rnames = gsub('`$', '', rnames)
  rnames = gsub('`', ':', rnames)
  rnames
}


plot_summs(lin1, colors = "black", robust = TRUE)




# crear vectores  y_hat_train,y_hat_test de prediccion en train-set y test-set

y_hat_train <- predict(lin1, train_s)
y_hat_test <- predict(lin1, test_s)


# convertir y_hat en discreto en train-set y test-set 

res_train <- train_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_train$pred_lin1 <- exp(y_hat_train)
res_train$pobre_lin1 <- ifelse(res_train$pred_lin1/res_train$Npersug <= res_train$Lp, 1, 0)


res_test <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_test$pred_lin1 <- exp(y_hat_test)
res_test$pobre_lin1 <- ifelse(res_test$pred_lin1/res_test$Npersug <= res_test$Lp, 1, 0)



# calcular matriz de confusion
# data = factor()
# reference = factor

# matriz de confusion en train-set
cm_lin1_train <- confusionMatrix(data=factor(res_train$pobre_lin1) , 
                         reference=factor(res_train$Pobre1) , 
                         mode="sens_spec" , positive="1")
cm_lin1_train


# matriz de confusion en test-set

cm_lin1_test <- confusionMatrix(data=factor(res_test$pobre_lin1) , 
                           reference=factor(res_test$Pobre1) , 
                           mode="sens_spec" , positive="1")
cm_lin1_test





# Regularizacion: Lasso y Ridge


# Lasso:  alpha=1 

# crear modelo Lasso x_train_s, y_train
# lambda: barrido sobre lambda de regularizacion

lasso <-  glmnet(
  x = x_train_s,
  y = y_train,
  alpha = 1,
  lambda = seq(0,2,0.1),
  standardize = FALSE
)

summary(lasso)


regularizacion <- lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = lasso$lambda)


regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )




regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")




# crear y_hat vector de predicciones en train-set y test-set

lambdas_lasso <- lasso$lambda
y_hat_lasso_train <- predict(lasso, 
                              newx = as.matrix(x_train_s))

#Predicción pobre en el test (del)
p_hat_train  <- ifelse(exp(predict(lasso, newx = as.matrix(x_train_s)))/x_train_s$Npersug < x_train_s$Lp,1,0)





y_hat_lasso_test <- predict(lasso,
                            newx = as.matrix(x_test))

#Predicción pobre en el test (del)
p_hat_test  <- ifelse(exp(predict(lasso, newx = as.matrix(x_test)))/test_s$Npersug < test_s$Lp,1,0)





#Predicción pobre en el test
#p_hat_lasso_train <- ifelse(exp(predict(lasso, newx = as.matrix(x_train_s)))/x_train_s$Npersug < x_train_s$Lp,1,0)
# convertir y_hat en discreto en train-set y test-set 
#res_lasso_train <- train_s_under%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
#res_lasso_train$pred_lasso1 <- exp(y_hat_lasso_train)
#res_train$pobre_lasso1 <- ifelse(res_lasso_train$pred_lasso1/res_lasso_train$Npersug <= res_lasso_train$Lp, 1, 0)


#res_test <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
#res_test$pred_lin1 <- exp(y_hat_test)
#res_test$pobre_lin1 <- ifelse(res_test$pred_lin1/res_test$Npersug <= res_test$Lp, 1, 0)








y_hat_lasso_test <- predict(lasso, 
                             newx = as.matrix(x_test))

#Predicción pobre en el test (del)
p_hat_lasso_test <- ifelse(exp(predict(lasso, newx = as.matrix(x_test)))/test_s$Npersug < test_s$Lp,1,0)




# Cada vector de prediccion para cada lambda de regularizacion se hacen metricas de evaluacion
resultados_lasso <- data.frame()

# iterar sobre los lambdas de regularizacion
for (i in 1:length(lambdas_lasso_train)) {
  lambda_actual <- lambdas_lasso_train[i]
  y_hat_lasso_train_actual <- y_hat_lasso_train[, i]
  
  # convertir en discreta la clasificacion
  
  
  
  
  
  r22 <- R2_Score(y_pred = y_hat_out2, y_true = y_test)
  rmse2 <- RMSE(y_pred = y_hat_out2, y_true = y_test)
  resultado <- data.frame(Modelo = "Lasso",
                          Muestra = "Fuera",
                          Lambda = l,
                          R2_Score = r22, 
                          RMSE = rmse2)
  resultados_lasso <- bind_rows(resultados_lasso, resultado)
}




ggplot(resultados_lasso, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)





# Ridge:  alpha=0

# crear modelo Ridge x_train_s, y_train
# lambda: barrido sobre lambda de regularizacion

ridge <-  glmnet(
  x = x_train_s,
  y = y_train,
  alpha = 0,
  lambda = seq(0,2,0.01),
  standardize = FALSE
)

summary(ridge)


reg_ridge <- ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = ridge$lambda)


reg_ridge <- reg_ridge %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )




reg_ridge %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")


# crear vector de prediccion y_hat en train-set y test-set


y_hat_train_ridge <- predict(ridge, 
                             newx = as.matrix(x_train_s))
p_hat_train <- ifelse(exp(predict(ridge, newx = as.matrix(x_train_s)))/train_s_under$Npersug < train_s_under$Lp,1,0)




y_hat_test_ridge <- predict(ridge, 
                            newx = as.matrix(x_test))
p_hat_test <- ifelse(exp(predict(modelo_ridge, newx = as.matrix(x_test)))/test_s$Npersug < test_s$Lp,1,0)



# crear vector de lambda: regularización
lambdas_ridge <- ridge$lambda



# validacion cruzada

res_ridge <- data.frame()

for(i in 1:length(lambdas_ridge)){
  
  reg <-  lambdas_ridge[i]
  
  y_hat_ridge_train <- y_hat_train_ridge[, i]
  
  
  
  
}






