## ----setup, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  tidy = FALSE,
  fig.width = 8
)


## ----librerias, results='hide'-----------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(gt)
library(kableExtra)
library(discrim)
library(kknn)
library(naivebayes)
library(patchwork) # Visualizar múltiples plots juntos


options(dplyr.summarise.inform = FALSE, scipen = 999)
shape_values = c(4,1)
theme_set(theme_bw())


## ----------------------------------------------------------------------------------------------------
base_juguete <- readRDS(file = "fuentes/eut_juguete_clase4.RDS") %>%
  mutate(realiza_trabajo_domestico = factor(realiza_trabajo_domestico,
                                            levels = c("Si", "No")))

base_juguete %>%
  ggplot(
    aes(x = horas_trabajo_mercado,
        y = ingreso_familiar,
        shape = realiza_trabajo_domestico)
  ) +
  geom_point(size = 3)+
  scale_shape_manual(values=shape_values)



## ----------------------------------------------------------------------------------------------------
knn_model5 <- nearest_neighbor(
    neighbors = 5,
    weight_func = "rectangular" #Sin asignar peso a las distancias
  ) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")


## ----------------------------------------------------------------------------------------------------
knn_fit <-  fit(
  object = knn_model5,
  formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado +
                                         ingreso_familiar,
  data = base_juguete
)



## ----------------------------------------------------------------------------------------------------
base_pred <- knn_fit %>% 
  augment(base_juguete) 

set.seed(42)
base_pred %>% 
  select(realiza_trabajo_domestico,.pred_class,.pred_No,.pred_Si) %>% 
  sample_n(50)


## ----------------------------------------------------------------------------------------------------
ggplot(
    base_pred,
    aes(
      x = horas_trabajo_mercado,
      y = ingreso_familiar,
      shape = realiza_trabajo_domestico,
      color = .pred_class
    )
  ) +
  geom_point(size = 3) +
  scale_shape_manual(values=shape_values)+
  scale_color_manual(values=c("red","blue"))


## ----echo=FALSE--------------------------------------------------------------------------------------
base_juguete_loop <- base_juguete

for (k in c(1, 3, 5, 50)) {
  knn_modelk <- nearest_neighbor(neighbors = k, weight_func = "rectangular") %>%
    set_engine("kknn") %>%
    set_mode("classification")
  
  knn_fitk <-  fit(
    object = knn_modelk,
    formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado +
                                           ingreso_familiar,
    data = base_juguete
  )
  
  preds <- knn_fitk %>% 
    predict(base_juguete_loop)

  base_juguete_loop[,paste0("k_",k)] = preds
  
  names(base_juguete_loop)[names(base_juguete_loop) == ".pred_class"] <-
    paste0("k_", k)
  
}


## ----echo=FALSE--------------------------------------------------------------------------------------
base_juguete_loop %>%
  select(
    horas_trabajo_mercado,
    ingreso_familiar,
    realiza_trabajo_domestico,
    starts_with("k_")
  ) %>%
  pivot_longer(cols = starts_with("k_"),
               names_to = "k_elegido",
               values_to = ".pred_class") %>%
  ggplot(
    .,
    aes(
      x = horas_trabajo_mercado,
      y = ingreso_familiar,
      shape = realiza_trabajo_domestico,
      color = .pred_class
    )
  ) +
  geom_point(size = 3) +
  facet_wrap(vars(k_elegido))+
  scale_shape_manual(values=shape_values)+
  scale_color_manual(values=c("red","blue"))


## ----------------------------------------------------------------------------------------------------
# Split
set.seed(132)

base_split <- initial_split(base_juguete)
base_train <- training(base_split)
base_test <- testing(base_split)

# Estimo con train
knn_fit_train <-  knn_model5 %>% 
  fit(formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado + 
                                             ingreso_familiar,
      data = base_train)

# Aplico a test
knn_fit_test <- knn_fit_train %>% 
  augment(base_test)

ggplot(
  knn_fit_test,
  aes(
    x = horas_trabajo_mercado,
    y = ingreso_familiar,
    shape = realiza_trabajo_domestico,
    color = .pred_class
  )
) +
  geom_point(size = 3) +
  scale_shape_manual(values=shape_values)+
  scale_color_manual(values=c("red","blue"))



## ----------------------------------------------------------------------------------------------------
cm <- conf_mat(data = knn_fit_test,
               truth = realiza_trabajo_domestico,
               estimate = .pred_class )

cm %>% autoplot("heatmap")+scale_fill_viridis_b(alpha = .3)


## ----------------------------------------------------------------------------------------------------
accuracy(data = knn_fit_test,
         truth = realiza_trabajo_domestico,
         estimate = .pred_class)


## ----------------------------------------------------------------------------------------------------
lda_model <- discrim_linear() %>% 
  set_engine("MASS") %>% 
  set_mode("classification") 
  

lda_fit <-  lda_model %>% 
  fit(formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado + 
                                             ingreso_familiar,
      data = base_train)

lda_fit_test <- lda_fit %>% 
  augment(base_test)

conf_mat(data = lda_fit_test,
         truth = realiza_trabajo_domestico,
         estimate = .pred_class)

accuracy(data = lda_fit_test,
         truth = realiza_trabajo_domestico,
         estimate = .pred_class)


## ----------------------------------------------------------------------------------------------------
nb_model <- naive_Bayes() %>%
  set_engine('naivebayes') %>% 
  set_mode("classification")

nb_fit <-  nb_model %>%
  fit(formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado +
                                             ingreso_familiar,
      data = base_train)



## ----------------------------------------------------------------------------------------------------
log_model <-  logistic_reg() %>%
  set_mode("classification") %>% 
  set_engine("glm")

log_fit <- log_model %>% 
  fit(formula = realiza_trabajo_domestico  ~ horas_trabajo_mercado +
                                             ingreso_familiar,
      data = base_train)



## ----------------------------------------------------------------------------------------------------
#Lista de modelos
modelos <- list("logistica" = log_fit,
                "LDA" = lda_fit,
                "KNN-5" = knn_fit, 
                "Naive bayes" = nb_fit
            )

names(modelos)



## ----------------------------------------------------------------------------------------------------
base_test_preds <- imap_dfr(modelos,
                            augment,
                            new_data = base_test,
                            .id = "model")

#Visualizando las columnas generadas
base_test_preds %>%
  select(model,
         realiza_trabajo_domestico,
         .pred_class,
         .pred_No,
         .pred_Si) %>% 
  sample_n(10)


## ----------------------------------------------------------------------------------------------------
multi_metric <- metric_set(accuracy, sensitivity, specificity)



## ----------------------------------------------------------------------------------------------------
base_test_preds %>%
  group_by(model) %>%
  multi_metric(truth = realiza_trabajo_domestico,
               estimate = .pred_class) %>% 
  arrange(.metric, .estimate)


## ----fig.width=10, fig.height=10---------------------------------------------------------------------
source("plot_decision_boundary.R")
p_knn5 <- plot_decision_boundary(
  data = base_juguete,
  model = knn_fit,
  x_names = c("horas_trabajo_mercado", "ingreso_familiar"),
  y_name = "realiza_trabajo_domestico",
  n_points = 100,
  model_name = "KNN (k=5)"
)

p_lda <- plot_decision_boundary(
  data = base_juguete,
  model = lda_fit,
  x_names = c("horas_trabajo_mercado", "ingreso_familiar"),
  y_name = "realiza_trabajo_domestico",
  n_points = 100,
  model_name = "LDA"
)

p_log <- plot_decision_boundary(
  data = base_juguete,
  model = log_fit,
  x_names = c("horas_trabajo_mercado", "ingreso_familiar"),
  y_name = "realiza_trabajo_domestico",
  n_points = 100,
  model_name = "Regresión logística"
)

p_nb <- plot_decision_boundary(
  data = base_juguete,
  model = nb_fit,
  x_names = c("horas_trabajo_mercado", "ingreso_familiar"),
  y_name = "realiza_trabajo_domestico",
  n_points = 500,
  model_name = "Naive Bayes"
)

(p_knn5 | p_lda) / (p_log | p_nb)

