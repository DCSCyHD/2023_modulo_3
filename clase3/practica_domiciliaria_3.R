library(tidyverse)
library(tidymodels)
options(scipen = 999)

#En esta práctica vamos a implementar una regresión lineal múltiple sobre la siguiente base de datos

df <- read_delim('https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/Howell1.csv', delim=";")

#Son datos de un censo parcial de un área de Botswana y, específicamente, de la etnia !Kung San. Son datos compilados a partir de e entrevistas realizadas por Nancy Howell a fines de la década de 1960.

##### Ejercicio 1 #####

#Proponga un modelo de regresión lineal múltiple usando todas las variables disponibles definiendo como variable dependiente a la variable weight (usando herramientas de tidymodels).
#¿Todas las variables aportan al modelo? Considere las variaciones del R cuadrado y R cuadrado ajustado al incorporar/quitar distintas variables para llegar a una selección final de variables relevantes.


##### Ejercicio 2 #####

#Evalúe el modelo. 
#¿Cuán fuerte es la relación entre la variable dependiente y las variables independientes? ¿Esta relación es estadísticamente significativa? ¿Por qué? 
#Interprete los coeficientes beta. 
#¿Se modifican los valores de los coeficientes respecto a aquellos obtenidos para el modelo de regresión simple?
