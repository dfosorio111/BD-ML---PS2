## Arboles y Random Forest en R: Diego Osorio

#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2/data")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2/data")

#UNI
setwd("C:/Users/de.franco/Documents/diego")



#Importar paquetes y cargar librerías
require(pacman)
#install.packages()

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

names(data)
data <- data[-c(1:9,11,15:18,20,22,24,29,31,38,42:45,52,54,57,58,60, 66:68)]

#Se crea la matriz de dummys
df <- model.matrix(~ .  - 1, data)%>%data.frame()

#write.csv(df,"dfdummy.csv")



# Dividimos train/test (80/20)

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


# variables de regresion
y_train <- train_s_under[,"log_ingtot"]
y_test <-  test_s[,"log_ingtot"]


# variables de clasificacion
p_train <- train_s_under[,"Pobre1"]
p_test <-  test_s[,"Pobre1"]
names(train_s_under)


# quitar variables con multicolinealidad 
x_train_s = train_s_under[-c(1,11,105,112,113)]
x_test = test_s[-c(1,11,105,112,113)]


#X_train <- select(train, -c("Lp", "Pobre", "Ingtotugarr", "Npersug"))
names(train_s_under)

# regresion lineal simple (corroborar ajuste de modelos)
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



names(train_s_under)


#Árboles de decisión y random forest

### Clasificacion 
# convertir p_train y p_test como factor

p_train <- factor(p_train)
p_test <- factor(p_test)


# crear arbol virgen

tree1 <- decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("classification")


# fit/train el modelo }
tree1_fit <- fit(tree1, p_train~. -Clase1-Pobre1-Lp-Ingtotugarr-log_ingtot, data = train_s_under)

#formula = log_ingtot ~. -Clase1-Pobre1-Lp-Ingtotugarr , 


# Gráfica del modelo
plot <- fancyRpartPlot(tree1_fit$fit, main = "Árbol sin fine tuning", 
                       sub = "")


# Importancia de las variables
feature_importancia <- varImp(tree1_fit$fit)

feature_importancia <- feature_importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))


# ggplot: bar graph % de importancia
ggplot(feature_importancia, aes(x = Porcentaje, 
                                y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()



# calcular y_hat vector de predicciones

y_hat_train_tree1 <- predict(tree1_fit, train_s_under)$.pred_class
y_hat_test_tree1 <- predict(tree1_fit, test_s)$.pred_class


# calcular dataframe de resultados

res_train_tree <- train_s_under%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_train_tree$y_hat_train_tree1 <- y_hat_train_tree1

res_test_tree <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_test_tree$y_hat_test_tree1 <- y_hat_test_tree1



# calcular matriz de confusion
cm_train_tree1 <- confusionMatrix(data= factor(y_hat_train_tree1),
                                  reference = factor(train_s_under$Pobre1),
                                  mode = "sens_spec",
                                  positive = "1")

cm_train_tree1

cm_test_tree1 <- confusionMatrix(data= factor(y_hat_test_tree1),
                                 reference = factor(test_s$Pobre1),
                                 mode = "sens_spec",
                                 positive = "1")

cm_test_tree1

# FALTA RESULTADOS Y F1(?) ver complementaria



# crear tunning tree
# hiper parametros: cost-complexity, tree_depth, min_n

tree_tune1 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")



# grid: crear grid de hiper parametros a optimizar

tree_grid <- crossing(
  cost_complexity = seq(0.005,0.1,0.01),
  min_n = c(330,450,550,650),
  tree_depth = c(5, 8, 10)
)



# replicabilidad: set.seed

set.seed(1233)

# k-folds cross-val: k=10

folds <- vfold_cv(train_s_under, strata = Pobre1, v =10)


# Entrenamos el modelo utilizando procesamiento en paralelo
#cl <- makePSOCKcluster(n_cores -1) 
#registerDoParallel(cl)


# establecer funcion avg de metricas de evaluacion para maximizar
#avg <- metric_set(0.75*recall)

#gridsearch

set.seed(1232)
tree_tune1_cv <- tune_grid(
  tree_tune1,
  Pobre1 ~ . -Clase1-Pobre1-Lp-Ingtotugarr-log_ingtot,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(recall),
  control = control_grid(event_level = 'second')
)



collect_metrics(tree_tune1_cv)
autoplot(tree_tune1_cv) + 
  theme_light() +
  labs(y = "recall")

# Escogemos el modelo óptimo
tree_opt1 <- finalize_model(tree_tune1, select_best(tree_tune1_cv))

# Entrenamos el mejor modelo
tree_opt1_fit  <- fit(tree_opt1, Pobre1 ~ . -Clase1-Pobre1-Lp-Ingtotugarr-log_ingtot, train_s_under)



# Cortes del modelo
# tree_opt1 
# Gráfica del modelo
fancyRpartPlot(tree_opt1_fit$fit, main = "Árbol con Tuning de hiper-parámetros", 
               sub = "")


# Importancia de las variables
feature_imp <- varImp(tree_opt1_fit$fit)
feature_imp <- feature_imp %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))


ggplot(feature_imp, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()





# calcular y_hat vector de predicciones

y_hat_train_tree2 <- predict(tree_opt1_fit, train_s_under)$.pred_class
y_hat_test_tree2 <- predict(tree_opt1_fit, test_s)$.pred_class


# calcular dataframe de resultados

res_train_tree2 <- train_s_under%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_train_tree2$y_hat_train_tree2 <- y_hat_train_tree2

res_test_tree2 <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_test_tree2$y_hat_test_tree2 <- y_hat_test_tree2



# calcular matriz de confusion
cm_train_tree2 <- confusionMatrix(data= factor(y_hat_train_tree2),
                                  reference = factor(train_s_under$Pobre1),
                                  mode = "sens_spec",
                                  positive = "1")

cm_train_tree2

cm_test_tree2 <- confusionMatrix(data= factor(y_hat_test_tree2),
                                 reference = factor(test_s$Pobre1),
                                 mode = "sens_spec",
                                 positive = "1")

cm_test_tree2









### Regresion

# convertir p_train y p_test como factor

p_train <- factor(p_train)
p_test <- factor(p_test)


# crear arbol virgen

tree_reg1 <- decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("regression")


# fit/train el modelo }
tree_reg1_fit <- fit(tree_reg1, log_ingtot ~. -Clase1-Pobre1-Lp-Ingtotugarr, data = train_s_under)

#formula = log_ingtot ~. -Clase1-Pobre1-Lp-Ingtotugarr , 


# Gráfica del modelo
plot <- fancyRpartPlot(tree_reg1_fit$fit, main = "Árbol de Clasificación sin fine tuning", 
                       sub = "")


# Importancia de las variables
feature_imp_reg1 <- varImp(tree_reg1_fit$fit)

feature_imp_reg1 <- feature_imp_reg1 %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))


# ggplot: bar graph % de importancia
ggplot(feature_imp_reg1, aes(x = Porcentaje, 
                             y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()



# calcular y_hat vector de predicciones

y_hat_train_tree_reg1 <- predict(tree_reg1_fit, train_s_under)$.pred_class
y_hat_test_tree_reg1 <- predict(tree_reg1_fit, test_s)$.pred_class


# calcular dataframe de resultados

res_train_tree_reg1 <- train_s_under%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_train_tree_reg1$y_hat_train_tree_reg1 <- y_hat_train_tree_reg1

res_test_tree_reg1 <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_test_tree_reg1$y_hat_test_tree_reg1 <- y_hat_test_tree_reg1



# calcular matriz de confusion
cm_train_tree_reg1 <- confusionMatrix(data= factor(y_hat_train_tree_reg1),
                                      reference = factor(train_s_under$Pobre1),
                                      mode = "sens_spec",
                                      positive = "1")

cm_train_tree_reg1

cm_test_tree_reg1 <- confusionMatrix(data= factor(y_hat_test_tree_reg1),
                                     reference = factor(test_s$Pobre1),
                                     mode = "sens_spec",
                                     positive = "1")

cm_test_tree_reg1

# FALTA RESULTADOS Y F1(?) ver complementaria



# crear tunning tree
# hiper parametros: cost-complexity, tree_depth, min_n


tree_tune_reg1 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")



# grid: crear grid de hiper parametros a optimizar

tree_grid_reg1 <- crossing(
  cost_complexity = seq(0.005,0.1,0.01),
  min_n = c(330,450,550,650),
  tree_depth = c(5, 8, 10)
)



# replicabilidad: set.seed

set.seed(1233)

# k-folds cross-val: k=10

folds2 <- vfold_cv(train_s_under, strata = log_ingtot, v =10)


# Entrenamos el modelo utilizando procesamiento en paralelo
#cl <- makePSOCKcluster(n_cores -1) 
#registerDoParallel(cl)


# establecer funcion avg de metricas de evaluacion para maximizar
#avg <- metric_set(0.75*recall)

#gridsearch

set.seed(1232)
tree_tune_reg1_cv <- tune_grid(
  tree_grid_reg1,
  log_ingtot ~ . -Clase1-Pobre1-Lp-Ingtotugarr,
  resamples = folds,
  grid = tree_grid_reg1,
  metrics = metric_set(recall),
  control = control_grid(event_level = 'second')
)



collect_metrics(tree_tune_reg1_cv)
autoplot(tree_tune_reg1_cv) + 
  theme_light() +
  labs(y = "recall")

# Escogemos el modelo óptimo
tree_opt2 <- finalize_model(tree_tune_reg1, select_best(tree_tune_reg1_cv))

# Entrenamos el mejor modelo
tree_opt2_fit  <- fit(tree_opt2, log_ingtot ~ . -Clase1-Pobre1-Lp-Ingtotugarr, train_s_under)



# Cortes del modelo
# tree_opt1 
# Gráfica del modelo
fancyRpartPlot(tree_opt2_fit$fit, main = "Árbol con Tuning de hiper-parámetros", 
               sub = "")


# Importancia de las variables
feature_imp2 <- varImp(tree_opt2_fit$fit)
feature_imp2 <- feature_imp %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))


ggplot(feature_imp, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()





# calcular y_hat vector de predicciones

y_hat_train_tree2 <- predict(tree_opt1_fit, train_s_under)$.pred_class
y_hat_test_tree2 <- predict(tree_opt1_fit, test_s)$.pred_class


# calcular dataframe de resultados

res_train_tree2 <- train_s_under%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_train_tree2$y_hat_train_tree2 <- y_hat_train_tree2

res_test_tree2 <- test_s%>%select(Ingtotugarr, Pobre1, Npersug, Lp)
res_test_tree2$y_hat_test_tree2 <- y_hat_test_tree2



# calcular matriz de confusion
cm_train_tree2 <- confusionMatrix(data= factor(y_hat_train_tree2),
                                  reference = factor(train_s_under$Pobre1),
                                  mode = "sens_spec",
                                  positive = "1")

cm_train_tree2

cm_test_tree2 <- confusionMatrix(data= factor(y_hat_test_tree2),
                                 reference = factor(test_s$Pobre1),
                                 mode = "sens_spec",
                                 positive = "1")

cm_test_tree2
