#Código de limpieza de datos

#Limpiar el ambiente
rm(list=ls())
#Establecer directorios
#Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS2/data")
#Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS2")
#Samuel
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS2")
#Computador universidad
setwd("C:/Users/de.franco/Documents/ps2")


#Se establece semilla
set.seed(1000)
#Importar paquetes y cargar librerías
require(pacman)
p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe, 
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here, GGally)

#Importar base de datos
train_completa <- read.csv("data/train_completa.csv")

#Quitar Bogota
test_hogares <- test_hogares%>%subset(Dominio!="BOGOTA")

#Arreglar algunas variables

train_completa$jefe_cotiza <- train_completa$jefe_cotiza-1

categoricas <- c("Pobre", "Clase", "Dominio", "P5090", "Depto", "jefe_mujer", "jefe_cotiza", "relab_jefe",
                 "P6090", "jefe_cotiza", "relab_jefe", "max_edu_lev_h", "max_empl", "Relab1", "Relab2", "Relab3",
                 "Educ1", "Educ2", "Educ3", "hijos", "pareja", "nietos", "otros_parientes", "no_parientes", "emp_pen",
                 "recibe_arriendos")

for (var in categoricas) {
  train_completa[,var] <- as.factor(train_completa[,var, drop = TRUE])
}

table(train_completa$max_empl)

train_completa$max_edu_lev_h <- factor(train_completa$max_edu_lev_h, order = TRUE, levels=c(1,2,3,4,5,6))

train_completa$max_empl <- factor(train_completa$max_empl, order = TRUE, levels=c(0,1,2,3,4,5,6,7,8,9))

#Logaritmo del ingreso
train_completa <- train_completa %>% mutate(logy = log(Ingtotugarr))

#Logaritmo del valor arriendo
train_completa <- train_completa %>% mutate(logarriendo = log(Valor_Arriendo))

#Nuevas categorias empresa
train_completa <- train_completa %>%
  mutate(max_empleo = case_when(max_empl == 0 ~ 0,
                                max_empl == 1 ~ 1,
                                max_empl == 2 ~ 2,
                                max_empl == 3 ~ 2,
                                max_empl == 4 ~ 2,
                                max_empl == 5 ~ 3,
                                max_empl == 6 ~ 3,
                                max_empl == 7 ~ 3,
                                max_empl == 8 ~ 4,
                                max_empl == 9 ~ 4))

train_completa$max_empleo <- factor(train_completa$max_empleo, order = TRUE, levels=c(0,1,2,3,4))


train_completa$Pobre <- factor(train_completa$Pobre,
                               labels = c("No Pob", "Pobres"))

train_completa <- train_completa %>% mutate(logpet = log(prop_cotiza))

train_completa <- train_completa %>% mutate(logppc = log(ppc))

table(is.na(train_completa$logpet))

train_completa <- train_completa[complete.cases(train_completa[ , 76]),]

#Gráfico matriz

install.packages("extrafont")
library(extrafont)
font_import()


ggpairs(train_completa, 
        columns = c("logpet", "logarriendo","logppc",  "años_educ_promedio", "max_empl"),
        columnLabels = c("Pensión (%PET)", "Log del arriendo", "Cuartos por persona","Años de educación", "Firma más grande"),
        ggplot2::aes(colour = Pobre, alpha=0.5),
        title = "Características de la muestra según pobreza y variables relevantes ",
        lower = list(continuous = "smooth", combo = "facetdensity"),
        upper = list(combo = "box", continuous="cor"),
        showStrips = FALSE)+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5))

ggsave("matrizgraficos", dpi=300, dev='png', height=7, width=7, units="in")

#Gráfico diferencia

train_completa <- train_completa %>% mutate (diferencia1 = Ingpcug-Lp)

train_completa <- train_completa %>% mutate (diferencia2 = abs(Ingpcug-Lp))

train_completa <- train_completa %>% mutate (diferencia3 = log(diferencia1))

train_completa <- train_completa %>% mutate (diferencia4 = log(Ingpcug)-log(Lp))

train_completa$Pobre <- factor(train_completa$Pobre,
                               labels = c("No Pobres", "Pobres"))

ggplot(train_completa, aes(x = diferencia4, fill = Pobre)) + 
  geom_histogram()+
  ylab("Frecuencia")+
  xlab("Log(Ingreso per capita - Línea de pobreza)")+
  ggtitle("Distancia a la línea de pobreza")+
  labs(fill = "")+
  theme_classic()+
  geom_vline(xintercept = 0, linetype="solid", color = "black")+
  theme(plot.title=element_text(hjust=0.5))

ggsave("diferencia", dpi=300, dev='png', height=7, width=7, units="in")


#Más importantes regresión

# Boxplot horas trabajadas por nivel de pobreza
ggplot(train_completa, aes(x = Pobre , y = Horas_Hogar, fill=Pobre)) +
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Horas trabajadas según pobreza")+
  ylab("Horas trabajadas")+
  xlab("")+
  theme_classic()+
  labs(fill = "")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

ggsave("stores/horas", dpi=300, dev='png', height=7, width=7, units="in")

# Boxplot proporcion de ocupados
ggplot(train_completa, aes(x = Pobre , y = prop_ocupados_pet, fill=Pobre)) +
  geom_boxplot()+
  ggtitle("Proporción de ocupados según pobreza")+
  ylab("Ocupados / PET")+
  xlab("")+
  theme_classic()+
  labs(fill = "")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

ggsave("stores/ocupados", dpi=300, dev='png', height=7, width=7, units="in")

# Boxplot proporcion de desocupados
ggplot(train_completa, aes(x = Pobre , y = prop_Desocupados_pet, fill=Pobre)) +
  geom_boxplot()+
  ggtitle("Proporción de desocupados según pobreza")+
  ylab("Desocuados / PET")+
  xlab("")+
  theme_classic()+
  labs(fill = "")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Boxplot proporcion de arriendos

train_completa$numericarriendo <- as.numeric(train_completa$recibe_arriendos)

means <- train_completa %>% group_by(Pobre) %>% 
  summarise(mean = mean(numericarriendos))

ggplot(train_completa, aes(y = recibe_arriendos, fill=Pobre)) +
  geom_bar()+
  ggtitle("Recibe o no ingresos por arriendos según pobreza")+
  ylab("")+
  xlab("")+
  theme_classic()+
  labs(fill = "")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

ggsave("stores/arriendos", dpi=300, dev='png', height=7, width=7, units="in")

#Gráficos de pobreza

train_completa$pobre1 <- as.numeric(train_completa$Pobre)
train_completa$pobre1 <- train_completa$pobre1-1

# Gráfico de barras para el tamaño de la empresa
labels3 <- c("No \n trabaja", "Trabaja \n solo", "2-3", "4-5 ", 
             "6-10", "11-19", "20-30", "31-50", "51-100", "101 \n o más")

dfempl <- train_completa %>% 
  group_by(max_empl) %>% 
  summarise(pobre1 = mean(pobre1,na.rm=TRUE))

ggplot(dfempl, aes(x = max_empl, y = pobre1)) + 
  geom_col(fill="lightblue")+
  ggtitle("Pobreza según firma más grande del hogar")+
  ylab("Porcentaje de hogares pobres")+
  xlab("Tamaño de la firma por número de empleados")+
  scale_color_viridis(option = "D")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=labels3)

ggsave("stores/empleado", dpi=300, dev='png', height=7, width=10, units="in")

# Gráfico de area barras sel relab

train_completa$Relab1 <- factor(train_completa$Relab1, order = FALSE, levels=c(1,2,3,4,5,6,7,8,9,10,11,12))

labels4 <- c("Obrero / \n empleado de \n empresa \n particular",
             "Obrero / \n empleado del \n gobierno", 
             "Empleado \n doméstico", "Cuenta \n propia", 
             "Patrón \n empleador", "Trabajador \n familiar \n sin \n remuneración", 
             "Trabajador \n sin \n remuneración \n en empresas \n externas", 
             "Jornalero / \n peón", "Otro", "Estudiante", "Inactivos no \n estudiando", 
             "Desocupados")

dfrelab <- train_completa %>% 
  group_by(Relab1) %>% 
  summarise(pobre1 = mean(pobre1,na.rm=TRUE))

dfrelab$pobrerounded <- mutate(pobrerounded = round(dfrelab$pobre1, digits =2))

ggplot(dfrelab, aes(x = Relab1, y = pobre1)) + 
  geom_col(fill="lightblue")+
  ggtitle("Pobreza según relación laboral del jefe")+
  ylab("Porcentaje de hogares pobres")+
  xlab("Relación laboral del jefe")+
  scale_color_viridis(option = "D")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=labels4, guide = guide_axis(n.dodge=1)) 
  geom_text(aes(label = pobre1), vjust = -0.2)

ggsave("stores/relab", dpi=300, dev='png', height=7, width=11, units="in")

# Gráficos Prop Cotiza

aggregate(train_completa$prop_cotiza, list(train_completa$Pobre), FUN=mean)

train_completa %>% select("prop_cotiza", "Pobre") %>% head(,100)

ggplot(train_completa, aes(x=prop_cotiza, y=logy))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE, fullrange=TRUE, level=0.95)

ggplot(train_completa, aes(x = Pobre  , y = prop_cotiza)) +
  geom_boxplot()+
  ggtitle("Proporción de cotizantes en el hogar según pobreza")+
  ylab("Proporción de cotizantes")+
  xlab("")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Gráficos educación

ggplot(train_completa, aes(x=años_educ_promedio, y=logy))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE, fullrange=TRUE, level=0.95)