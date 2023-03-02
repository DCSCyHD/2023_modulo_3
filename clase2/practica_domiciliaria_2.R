library(tidyverse)
library(tidymodels)
library(GGally)
tidymodels_prefer()

#En esta práctica vamos a implementar una regresión lineal simple sobre la siguiente base de datos

df <- read_delim('https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/Howell1.csv', delim=";")

#Son datos de un censo parcial de un área de Botswana y, específicamente, de la etnia !Kung San. Son datos compilados a partir de e entrevistas realizadas por Nancy Howell a fines de la década de 1960.

##### Ejercicio 1 #####

#Explore la base de datos y defina un predictor numérico relevante para la variable weight

##### Ejercicio 2 #####

#Plantee un modelo de regresión lineal simple entre weight y la variable seleccionada usando herramientas de tidymodels

##### Ejercicio 3 #####

#Evalúe el modelo. 
#¿Cuán fuerte es la relación entre la variable dependiente y la variable independiente? ¿Esta relación es estadísticamente significativa? ¿Por qué? Interprete el coeficiente beta. 