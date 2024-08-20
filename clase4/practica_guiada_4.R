## ----setup, message=FALSE, warning=FALSE, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	fig.width = 8,
	message = FALSE,
	warning = FALSE)


## ----librerias, results='hide'-----------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(gtsummary)
library(modelsummary)
library(gt)

#options(dplyr.summarise.inform = FALSE)
options(scipen = 999)


## ----------------------------------------------------------------------------------------------------
base_juguete <- readRDS(file = "fuentes/eut_juguete_clase4.RDS")


ggplot(base_juguete,aes(x = horas_trabajo_mercado,
                        y = ingreso_familiar,
                        shape = realiza_trabajo_domestico))+
  geom_point(size = 3)


## ----------------------------------------------------------------------------------------------------
log_model <- logistic_reg() %>% #Defino el tipo de modelo
  set_mode("classification") %>%  #el modo (regresión o clasificación)
  set_engine("glm") #el motor en este caso es glm ("Generalized linear models")



## ----------------------------------------------------------------------------------------------------
base_para_modelar <- base_juguete %>% 
  mutate(realiza_trabajo_domestico = factor(realiza_trabajo_domestico,levels = c("No","Si")))

#Lo entreo con los datos 
log_fit <- log_model %>% 
  fit(realiza_trabajo_domestico ~ horas_trabajo_mercado + 
                                  ingreso_familiar,
      data = base_para_modelar)



## ----------------------------------------------------------------------------------------------------
log_fit %>% 
  tidy() %>% 
  gt()


## ----------------------------------------------------------------------------------------------------

log_fit2 <- log_model %>% 
  fit(realiza_trabajo_domestico ~ horas_trabajo_mercado +
                                  ingreso_familiar +
                                  menores_hogar,
      data = base_para_modelar)


modelsummary(
  list("logistica 2 predictores " = log_fit,
       "logistica 3 predictores" = log_fit2)
  )


## ----------------------------------------------------------------------------------------------------
data_nueva <- data.frame(horas_trabajo_mercado = c(40,13),
                        ingreso_familiar = c(60000,10000))

log_fit %>% 
  predict(new_data = data_nueva)


## ----------------------------------------------------------------------------------------------------
log_fit %>% 
  augment(new_data = data_nueva)



## ----------------------------------------------------------------------------------------------------
base_con_pred <- log_fit %>% 
  augment(base_para_modelar) %>% 
  mutate(prediccion_80 = ifelse(.pred_No > 0.8, yes = "No", no = "Si"))

base_con_pred


## ----------------------------------------------------------------------------------------------------
# Aplicamos orden alternativo
base_metricas <- base_con_pred %>% 
  mutate(realiza_trabajo_domestico = factor(realiza_trabajo_domestico,
                                            levels = c("Si","No")),
         .pred_class = factor(.pred_class,
                              levels = c("Si","No"))) 

matriz_confusion <- conf_mat(data = base_metricas ,
                             truth = realiza_trabajo_domestico,
                             estimate = .pred_class)

matriz_confusion


## ----------------------------------------------------------------------------------------------------
accu <- accuracy(data = base_metricas,
            truth = realiza_trabajo_domestico,
            estimate =  .pred_class)

accu


## ----------------------------------------------------------------------------------------------------

sens <- sensitivity(base_metricas,truth = realiza_trabajo_domestico,estimate =  .pred_class)

spec <- specificity(base_metricas,truth = realiza_trabajo_domestico,estimate =  .pred_class)

prec <- precision(base_metricas,truth = realiza_trabajo_domestico,estimate =  .pred_class)

bind_rows(accu,sens,spec,prec)


## ----------------------------------------------------------------------------------------------------
ggplot(base_con_pred,aes(x = horas_trabajo_mercado,
                         y = ingreso_familiar,
                         shape = realiza_trabajo_domestico,
                         color = .pred_Si))+
  geom_point(size = 3)+
  scale_color_viridis_c()


## ----------------------------------------------------------------------------------------------------
ggplot(base_con_pred,aes(x = horas_trabajo_mercado,
                         y = ingreso_familiar,
                         shape = realiza_trabajo_domestico,
                         color = .pred_class))+
  geom_point(size = 3)+
  scale_color_viridis_d()+
  theme_minimal()


## ----------------------------------------------------------------------------------------------------
set.seed(2024) #Cualquier cosa

base_split <-  initial_split(base_para_modelar, prop = 0.8)
base_train <-  training(base_split)
base_test <-  testing(base_split)



## ----------------------------------------------------------------------------------------------------
log_fit_train <- log_model  %>% 
  fit(realiza_trabajo_domestico ~ horas_trabajo_mercado +
                                  ingreso_familiar,
      data = base_train)

log_fit_train %>% tidy()


## ----------------------------------------------------------------------------------------------------
base_test_con_pred <- log_fit_train %>% 
  augment(new_data = base_test)

base_test_con_pred


## ----------------------------------------------------------------------------------------------------
matriz_confusion_test<- conf_mat(data = base_test_con_pred,
                                 truth = realiza_trabajo_domestico,
                                 estimate =.pred_class)

matriz_confusion_test


## ----include=FALSE-----------------------------------------------------------------------------------



## ----include=FALSE-----------------------------------------------------------------------------------



## ----------------------------------------------------------------------------------------------------
# Base de datos
base_real <- readRDS(file = "fuentes/base_EAUH_TNR.RDS") %>% 
  mutate(realiza_trabajo_domestico = factor(ifelse(TIEMPO_TDNR != 0,
                                                   yes = "Si",
                                                   no = "No"),
                                            levels = c("No","Si"))) %>% 
  rename("sexo"=CH04)

# Train-test split
set.seed(123)
base_split <- initial_split(base_real)
base_train <- training(base_split)
base_test <- testing(base_split)



## ----------------------------------------------------------------------------------------------------
# Elijo modelo
log_model <- logistic_reg() %>% 
  set_mode("classification") %>%  
  set_engine("glm")

# Estimo con fórmula 1
log_fit <- log_model %>% 
  fit(realiza_trabajo_domestico ~ horas_mercado +
                                  menores_hogar + 
                                  NIVEL_ED,
      data = base_train)  

# Estimo con fórmula 2
log_fit2 <- log_model %>% 
  fit(realiza_trabajo_domestico ~ horas_mercado + 
                                  ingreso_indiv + 
                                  sexo, 
      data = base_train)  


## ----------------------------------------------------------------------------------------------------
log_fit %>% 
  tidy()

log_fit2 %>% 
  tidy()

modelsummary::modelsummary(
  list("modelo 1 " = log_fit,
       "modelo 2" = log_fit2)
  )


## ----------------------------------------------------------------------------------------------------
contrasts(base_train$NIVEL_ED)


## ----------------------------------------------------------------------------------------------------
log_fit_test <- log_fit %>% 
  augment(base_test) %>%
  rename(pred_m1 = .pred_class) 

log_fit2_test <- log_fit2 %>%
  predict(base_test) %>% 
  rename(pred_m2 = .pred_class)

base_2modelos <- log_fit_test %>% 
  bind_cols(log_fit2_test)

base_2modelos %>% conf_mat(truth = realiza_trabajo_domestico,estimate = pred_m1)
base_2modelos %>% conf_mat(truth = realiza_trabajo_domestico,estimate = pred_m2)


## ----------------------------------------------------------------------------------------------------
table(base_real$realiza_trabajo_domestico)

base_balanceada <- base_real %>% 
  group_by(realiza_trabajo_domestico) %>% 
  sample_n(size = 15000) %>% 
  ungroup()


## ----------------------------------------------------------------------------------------------------
table(base_balanceada$realiza_trabajo_domestico)


## ----------------------------------------------------------------------------------------------------

# Split
set.seed(1234)
base_b_split <- initial_split(base_balanceada)
base_b_train <- training(base_b_split)
base_b_test <- testing(base_b_split)

# Model
log_model <- logistic_reg() %>% #Defino el tipo de modelo
  set_mode("classification") %>%  #el modo (regresión o clasificación)
  set_engine("glm")

# Fit
log_fit_1b <- log_model %>% 
  fit(realiza_trabajo_domestico ~ horas_mercado + 
                                  menores_hogar + 
                                  NIVEL_ED,
      data = base_b_train)  

log_fit_2b <- log_model %>% 
  fit(realiza_trabajo_domestico ~ horas_mercado +
                                  ingreso_indiv +
                                  sexo,
      data = base_b_train)  

log_fit_1b %>% 
  tidy()

log_fit_2b %>% 
  tidy()


## ----------------------------------------------------------------------------------------------------

log_fit_1b_test <- log_fit_1b %>% 
  augment(base_b_test) %>% 
  select(realiza_trabajo_domestico,.pred_class) %>% 
  rename(pred_m1 = .pred_class)

log_fit_2b_test <- log_fit_2b %>% 
  predict(base_b_test) %>% 
  rename(pred_m2 = .pred_class)

base_2modelos_b <- log_fit_1b_test %>% 
  bind_cols(log_fit_2b_test)



## ----------------------------------------------------------------------------------------------------
base_2modelos_b <- base_2modelos_b %>%  
  mutate(realiza_trabajo_domestico = factor(realiza_trabajo_domestico,
                                            levels = c("Si","No")),
         pred_m1 = factor(pred_m1,levels = c("Si","No")),
         pred_m2 = factor(pred_m2,levels = c("Si","No"))) 

base_2modelos_b %>% conf_mat(realiza_trabajo_domestico,pred_m1)
base_2modelos_b %>% conf_mat(realiza_trabajo_domestico,pred_m2)


## ----------------------------------------------------------------------------------------------------
base_2modelos %>% sensitivity(truth = realiza_trabajo_domestico,estimate = pred_m1)
base_2modelos %>% sensitivity(truth = realiza_trabajo_domestico,estimate = pred_m2)

base_2modelos_b %>% sensitivity(truth = realiza_trabajo_domestico,estimate = pred_m1)
base_2modelos_b %>% sensitivity(truth = realiza_trabajo_domestico,estimate = pred_m2)

base_2modelos %>% accuracy(realiza_trabajo_domestico,pred_m1)
base_2modelos %>% accuracy(realiza_trabajo_domestico,pred_m2)

base_2modelos_b %>% accuracy(realiza_trabajo_domestico,pred_m1)
base_2modelos_b %>% accuracy(realiza_trabajo_domestico,pred_m2)


