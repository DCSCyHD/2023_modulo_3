library(tidyverse)
library(tidymodels)
library(GGally)
library(viridis)
tidymodels_prefer()

#En esta práctica vamos a implementar una regresión lineal simple sobre la siguiente base de datos

df <- read_delim('https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/Howell1.csv', delim=";") %>% mutate(male = factor(male))

#Son datos de un censo parcial de un área de Botswana y, específicamente, de la etnia !Kung San. Son datos compilados a partir de e entrevistas realizadas por Nancy Howell a fines de la década de 1960.

##### Ejercicio 1 #####

#Explore la base de datos y defina un predictor numérico relevante para la variable weight
head(df)
ggpairs(df)

##### Ejercicio 2 #####

#Plantee un modelo de regresión lineal simple entre weight y la variable seleccionada usando herramientas de tidymodels

lm_spec <- linear_reg() %>%
  set_engine("lm")

lm_fit <- lm_spec %>%
  fit(weight ~ height, data = df)

##### Ejercicio 3 #####

#Evalúe el modelo. 

lm_fit %>% 
  pluck("fit") %>%
  summary()

#bonus: grafique las predicciones respecto a los valores reales 

augment(lm_fit, new_data = df) %>% 
  select(weight, .pred, .resid) %>% 
  ggplot(aes(x=.pred, y=weight,color=.resid)) + 
  geom_point() + 
  theme_minimal()+
  scale_color_viridis()

#¿Cuán fuerte es la relación entre la variable dependiente y la variable independiente? ¿Esta relación es estadísticamente significativa? ¿Por qué? Interprete el coeficiente beta. 

#Desarrolle