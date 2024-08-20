## ----setup, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=FALSE, tidy=FALSE,
                      fig.width=8)


## ----librerias, results='hide'-----------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(GGally)
library(gtsummary)
library(gt)
tidymodels_prefer()

options(dplyr.summarise.inform = FALSE)


## ----------------------------------------------------------------------------------------------------
base_juguete <- readRDS("./fuentes/eut_juguete.RDS")


## ----------------------------------------------------------------------------------------------------
names(base_juguete)


## ----------------------------------------------------------------------------------------------------
#modifico los nombres usando espacios para que entren mejor en las etiquetas
names(base_juguete) <- gsub(x = names(base_juguete),
                            pattern = "\\_",
                            replacement = " ")
names(base_juguete)


## ----------------------------------------------------------------------------------------------------
#graficamos
ggpairs(base_juguete, 
        labeller = label_wrap_gen(width = 5)) +  #Para que las etiquetas entren mejor
  theme_minimal() +
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10))


## ----------------------------------------------------------------------------------------------------
ggpairs(base_juguete,
        ggplot2::aes(
          color = sexo,
          fill = sexo,
          alpha = 0.8
        ),
        labeller = label_wrap_gen(width = 5)) +
  theme_minimal() +
  scale_color_manual(values = c("#45818e", "#CA225E")) +
  scale_fill_manual(values = c("#45818e", "#CA225E")) +
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10))


## ----include=F---------------------------------------------------------------------------------------
#vuelvo a dejar los nombres como estaban
base_juguete <- base_juguete %>% janitor::clean_names()


## ----------------------------------------------------------------------------------------------------
cor(base_juguete$horas_trabajo_domestico, 
    base_juguete$ingreso_individual)


## ----------------------------------------------------------------------------------------------------
base_juguete%>% 
  ggplot(aes(x=horas_trabajo_domestico, y=ingreso_individual)) + 
  geom_point() + 
  geom_smooth(
    method='lm', # Indica que queremos graficar una relación lineal
    se = FALSE, # Añadir el intervalo de confianza en torno a los puntos
    color = 'red'
  ) + 
  theme_minimal()


## ----------------------------------------------------------------------------------------------------
lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_model


## ----------------------------------------------------------------------------------------------------
lm_model %>% translate()


## ----------------------------------------------------------------------------------------------------
lm_fit <- lm_model %>%
  fit(ingreso_individual ~ horas_trabajo_domestico, 
      data = base_juguete)

#Siempre va "y" del lado izquierdo y "x" del lado derecho


## ----------------------------------------------------------------------------------------------------
tabla_coeficientes <- lm_fit %>% tidy() 

tabla_coeficientes %>% 
  gt() %>%   #para visualizar de forma más prolija
  fmt_number(decimals=2) %>% 
  tab_header("Regresión lineal")


## ----include=FALSE-----------------------------------------------------------------------------------
options(scipen = 999) #Evitar notación científica

intercepto <- round(tabla_coeficientes$estimate[1],1)
beta1 <- round(tabla_coeficientes$estimate[2])


## ----include=FALSE-----------------------------------------------------------------------------------
options(scipen = 0) #notación científica


## ----------------------------------------------------------------------------------------------------
names(lm_fit)

lm_fit %>% 
  pluck("fit") %>% #Pluck es la versión "tidy" de lm_fit[["fit"]]
  summary()


## ----------------------------------------------------------------------------------------------------



## ----------------------------------------------------------------------------------------------------
#respuesta
lm_model %>%
  fit(ingreso_individual ~ horas_trabajo_mercado, 
      data = base_juguete) %>%
  pluck("fit") %>%
  summary()


## ----------------------------------------------------------------------------------------------------
lm_fit %>% 
  predict(new_data = base_juguete
          #,type = "pred_int"  #Si queremos el intervalo de confianza de la prediccion
          )


## ----------------------------------------------------------------------------------------------------
lm_fit %>% 
  augment(new_data = base_juguete)


## ----------------------------------------------------------------------------------------------------
lm_fit %>% 
  augment(new_data = base_juguete) %>%
  dplyr::select(.resid, .pred) %>%
  ggplot(aes(x = .pred, y = .resid)) +
  geom_point() +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = 'dashed') 


## ----------------------------------------------------------------------------------------------------
lm_fit %>% 
  augment(new_data = base_juguete) %>% 
  pull(.pred) %>% 
  max()

lm_fit %>% 
  augment(new_data = base_juguete) %>% 
  pull(ingreso_individual) %>% 
  max()



## ----------------------------------------------------------------------------------------------------
base_juguete %>%
  ggplot(aes(x = horas_trabajo_domestico, 
             y = ingreso_individual, 
             color = sexo)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', se = FALSE) +
  theme_minimal()

