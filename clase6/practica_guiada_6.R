## ----setup, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=FALSE, tidy=FALSE,
                      fig.width=8)


## ----librerias, results='hide'-----------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(kknn)
library(gt)
library(discrim)

options(dplyr.summarise.inform = FALSE, scipen = 999)
theme_set(theme_bw())


## ----------------------------------------------------------------------------------------------------
base_juguete <- readRDS("fuentes/eut_juguete.RDS")

set.seed(42)
base_folds <- vfold_cv(data = base_juguete, v = 10)

base_folds



## ----------------------------------------------------------------------------------------------------
# Especificación del modelo
lm_model <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

# Receta/fórmula 
lm_rec <- recipe(formula = horas_trabajo_domestico ~ horas_trabajo_mercado +
                                                     ingreso_individual,
                 data =  base_juguete)


## ----------------------------------------------------------------------------------------------------
lm_val_fit <- fit_resamples(
  object       = lm_model, # Definición de mi (mis) modelos
  preprocessor = lm_rec, # Fórmula a aplicar
  resamples    = base_folds, # De donde saco las particiones
  metrics = metric_set(rmse, mae), # Root mean squear error y Mean abs error
  control = control_resamples(save_pred = TRUE) # Guardar predicciones
)


## ----------------------------------------------------------------------------------------------------
head(lm_val_fit)


## ----------------------------------------------------------------------------------------------------
lm_val_fit %>% 
  pull(.predictions) %>% 
  pluck(1)


## ----------------------------------------------------------------------------------------------------
lm_val_fit %>% 
  collect_metrics()


## ----------------------------------------------------------------------------------------------------
receta_basica <- recipe(
  horas_trabajo_domestico ~ horas_trabajo_mercado + 
                            ingreso_individual + 
                            sexo +
                            menores_hogar,
  data = base_juguete
)

receta_2 <- receta_basica  %>%
    # Agrego término al cuadrado para variables numericas
    step_poly(all_numeric_predictors(), 
              degree = 2, 
              options = c(raw = TRUE)) 

receta_3 <- receta_basica  %>%
    # Saco 2 variables a ver qué pasa
    step_rm(c(menores_hogar,ingreso_individual))

recetario <- list(
  basica = receta_basica,
  poly = receta_2,
  dos_x = receta_3
)

modelos <- list(
  reg_lineal = lm_model
)


## ----------------------------------------------------------------------------------------------------
lm_wflowset <- workflow_set(
  preproc = recetario, # Recetas a aplicar
  models = modelos,    # Lista de modelos (en este caso es uno solo)
  cross = TRUE)        #¿quiero hacer todas las combinaciones de recetas-modelos?

lm_wflowset


## ----------------------------------------------------------------------------------------------------
lm_wflowset_fit <- lm_wflowset %>% 
  workflow_map(fn = "fit_resamples",   #Hacer cross validation
               resamples = base_folds, #Con que folds
               metrics = metric_set(rmse, mae), #Recolectando cuales metricas
               seed = 1101            #Semilla para reproducibilidad
               ) 


## ----------------------------------------------------------------------------------------------------
collect_metrics(lm_wflowset_fit)


## ----------------------------------------------------------------------------------------------------
collect_metrics(lm_wflowset_fit) %>%
  select(wflow_id, .metric, mean) %>%
  pivot_wider(id_cols = wflow_id, names_from=.metric, values_from = mean) %>%
  gt() %>% fmt_number()


## ----------------------------------------------------------------------------------------------------
receta_basica <- recipe(
  ingreso_individual ~ horas_trabajo_mercado +
                       sexo +
                       menores_hogar,
  data = base_juguete)


## ----------------------------------------------------------------------------------------------------
receta_para_tunear <- receta_basica %>%
  step_poly(all_numeric_predictors(), 
            degree = tune()             #ACA LA CLAVE
  )

#Creamos el workflow y agregamos la receta y el modelo
workflow_tuneo <- workflow() %>%
  add_recipe(receta_para_tunear) %>% 
  add_model(lm_model)



## ----------------------------------------------------------------------------------------------------
grilla <- tibble(degree = 1:5)


## ----------------------------------------------------------------------------------------------------
set.seed(42)

tune_res <- tune_grid(
  object = workflow_tuneo,   # Qué modelo voy a tunear
  resamples = base_folds,    # De dónde saco los folds de datos 
  grid = grilla,             # Hiperparametros a evaluar
  metrics = metric_set(rmse, rsq) # Métricas a evaluar: RMSE y R^2 ajustado
)


## ----------------------------------------------------------------------------------------------------
tune_res %>% autoplot()


## ----------------------------------------------------------------------------------------------------
tune_res %>% 
  show_best(metric = "rmse", n = 2)


## ----------------------------------------------------------------------------------------------------
set.seed(42)
folds <- vfold_cv(data = base_juguete, 
                  v = 5, 
                  strata='realiza_trabajo_domestico')



## ----------------------------------------------------------------------------------------------------
receta_basica <- recipe(
  formula = realiza_trabajo_domestico ~ horas_trabajo_mercado + 
                                        menores_hogar + 
                                        sexo + 
                                        ingreso_individual,
  data = base_juguete) %>% 
  # Codificamos variable sexo
  step_dummy(all_nominal_predictors()) 

# Extendemos
receta_extendida <- receta_basica %>% 
  
  #Interacciones
  step_interact(~ all_predictors():all_predictors()) %>% 
  
  #Elimino variables altamente correlacionadas
  step_corr(all_numeric_predictors(), 
            threshold = 0.8) %>% 
  
  #Elimino variables con varianza cercana a 0
  step_nzv(all_predictors()) 


## ----------------------------------------------------------------------------------------------------
# KNN 
knn_model <- nearest_neighbor(
    neighbors = tune(),
    weight_func = "rectangular") %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

# QDA
qda_model <- discrim_quad() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")



## ----------------------------------------------------------------------------------------------------
wflowset <- workflow_set(
      preproc = list(basica = receta_basica, 
                     extendida = receta_extendida),
      models = list(knn = knn_model,
                    qda = qda_model),
      cross = TRUE #Todas las combinaciones posibles 
   )


## ----------------------------------------------------------------------------------------------------
grilla <- c(1,4,6)

set.seed(42)

wflowset_fit <- wflowset %>%
  workflow_map(
    fn = "tune_grid",
    resamples = folds,
    grid = grilla,
    metrics = metric_set(accuracy, roc_auc))


## ----------------------------------------------------------------------------------------------------
wflowset_fit %>% autoplot()


## ----------------------------------------------------------------------------------------------------
wflowset_fit %>% rank_results(rank_metric = "accuracy",
                               select_best = TRUE) %>%
  dplyr::select(wflow_id, .metric, mean) %>%
  pivot_wider(id_cols = wflow_id,
              names_from = .metric,
              values_from = mean) 


## ----------------------------------------------------------------------------------------------------
# Se extrae el workflow
wflowset_fit %>% 
  extract_workflow_set_result(id = 'extendida_knn') %>% 
  collect_metrics()

