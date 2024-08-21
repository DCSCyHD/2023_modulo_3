## ----setup, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
knitr::opts_chunk$set(
  echo=TRUE, 
  message=TRUE, 
  warning=FALSE, 
  tidy=FALSE,
  fig.width=8)


## ----librerias, results='hide', message=FALSE, warning=FALSE-------------
library(tidyverse)
library(tidymodels)
library(viridis)
library(car)
library(performance)
tidymodels_prefer()

options(dplyr.summarise.inform = FALSE)


## ------------------------------------------------------------------------
base_juguete <- readRDS("./fuentes/eut_juguete.RDS")


## ------------------------------------------------------------------------
lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")


## ------------------------------------------------------------------------
lm_fit <- lm_model %>%
  fit(ingreso_individual ~ horas_trabajo_domestico,
      data = base_juguete)


## ------------------------------------------------------------------------
lm_vif <- lm(ingreso_individual ~ . , #Con . indicamos "todas las demas"
               data = base_juguete)

# Calculo vif (variance inflation factor)
vif(lm_vif)

# Lo miro con un plot
vif(lm_vif) %>% data.frame() %>% 
  rownames_to_column(var="variable") %>% 
  ggplot(aes(x=variable,y=.))+
  geom_col(fill="#00BFC4")+
  geom_hline(yintercept = 5, linetype = "dashed")+
  coord_flip()+
  theme_minimal()



## ------------------------------------------------------------------------
lm_fit2 <- lm_model %>%
  fit(formula = ingreso_individual ~ ., #Con "." indicamos "todas las demás"
      data = base_juguete)


## ------------------------------------------------------------------------
lm_fit2 %>% 
  pluck("fit") %>%
  summary()


## ------------------------------------------------------------------------
#planteamos el nuevo modelo
lm_fit2 <- lm_model %>%
  fit(ingreso_individual ~ horas_trabajo_domestico + 
                           horas_trabajo_mercado + 
                           sexo + 
                           menores_hogar,
    data = base_juguete)

#y vemos los coeficientes
lm_fit2 %>%
  pluck("fit") %>%
  summary()


## ------------------------------------------------------------------------
#calculamos
lm_fit2 %>% 
  augment(new_data = base_juguete) %>%
  dplyr::select(.resid, .pred) %>%
  #graficamos
  ggplot(aes(x = .pred, y = .resid)) +
    geom_point() +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = 'dashed') 


## ------------------------------------------------------------------------
#usamos los ":" para agregar la interacción
lm_fit3 <- lm_model %>%
  fit(ingreso_individual ~ horas_trabajo_domestico +
                         sexo:horas_trabajo_mercado +
                         horas_trabajo_mercado + 
                         sexo + 
                         menores_hogar,
    data = base_juguete)


## ------------------------------------------------------------------------
lm_fit3 %>% 
  pluck("fit") %>%
  summary()


## ------------------------------------------------------------------------
lm_wflow <- workflow() %>% 
  add_model(lm_model)


## ------------------------------------------------------------------------
lm_wflow <- lm_wflow %>% 
  add_formula(ingreso_individual ~ horas_trabajo_domestico + 
                                   sexo:horas_trabajo_mercado +
                                   horas_trabajo_mercado + 
                                   sexo + 
                                   menores_hogar
  )


## ------------------------------------------------------------------------
lm_fit3 <- lm_wflow %>% 
  fit(base_juguete)


## ------------------------------------------------------------------------
lm_fit3 %>% 
  extract_fit_engine() %>% 
  summary() #%>% tidy()


## ------------------------------------------------------------------------
#incorporamos la fórmula
lm_rec <- recipe(
    ingreso_individual ~ horas_trabajo_domestico +
                         horas_trabajo_mercado +
                         sexo +
                         menores_hogar,
    data = base_juguete
  ) %>% 
  
  step_mutate(
    menores_hogar = as.character(menores_hogar), 
    id="mutate_inicial"
  ) %>% 
  
  #incorporamos transformaciones
  #agrego interacciones entre sexo y todas las variables que empiecen con "horas"
  step_interact(
    ~sexo:starts_with("horas"), 
    id="interacciones"
  ) %>% 
  
  #vuelvo dummy la variable sexo
  step_dummy(sexo, id="dummy") %>% 
  
  #agrupo en niveles que representen al menos el 20% de las observaciones la variable menores_hogar, agrupando a los otros valores en la categoría "otros"
  step_other(menores_hogar, threshold = 0.2, id="other") 


## ------------------------------------------------------------------------
lm_rec %>% 
  prep() %>% #Aplica transformaciones a las variables (hace la cuenta)
  juice()  #Extrae las variables transformadas (nos muestra el resultado)


## ------------------------------------------------------------------------
lm_wflow <- lm_wflow %>% 
  
  #se remueve la fórmula anterior ya que ahora la receta contiene la fórmula
  remove_formula() %>% 
  
  #se añade la receta de preprocesamiento
  add_recipe(lm_rec)


## ------------------------------------------------------------------------
lm_fit4 <- lm_wflow %>% 
  fit(base_juguete)


## ------------------------------------------------------------------------
glance(lm_fit4)


## ------------------------------------------------------------------------
lm_fit4 %>% 
  tidy()


## ------------------------------------------------------------------------
 lm_fit4 %>% 
  extract_recipe(estimated = TRUE) %>% 
  tidy(.,id = "other") #id del step (también es posible buscar por número del step (number=4))


## ----results='asis'------------------------------------------------------
lm_fit4 %>% 
  extract_recipe(estimated = TRUE)


## ------------------------------------------------------------------------
lm_fit4 %>% 
augment(new_data = base_juguete) %>%
  mutate(.resid = ingreso_individual - .pred) %>%
  #graficamos
  dplyr::select(.resid, .pred) %>%
  ggplot(aes(x = .pred, y = .resid)) +
    geom_point() +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = 'dashed') 


## ------------------------------------------------------------------------
lm_fit4 %>% 
augment(new_data = base_juguete) %>%
  mutate(.resid = ingreso_individual - .pred) %>%
  dplyr::select(ingreso_individual, .pred, .resid) %>%
  #graficamos
  ggplot(aes(y = .pred, x = ingreso_individual, color = .resid)) +
    geom_point() +
    theme_minimal()  +
    geom_abline(
      intercept = 0,
      slope = 1,
      linewidth = 1,
      color = "grey"
    ) +
    scale_color_viridis(option = "C")


## ------------------------------------------------------------------------
# Elijo el modelo
lm_model <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

# Armo mi receta (fórmula y transformaciones)
lm_rec <- recipe(
    ingreso_individual ~ sexo,
    data = base_juguete) %>% 
  step_dummy(sexo, id="dummy") 

# Junto todo en un workflow
lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(lm_rec)

# Estimo
lm_fit <- lm_wflow %>% 
  fit(base_juguete)
  
# Extraigo resultados
lm_fit %>% 
  tidy()

lm_fit %>% 
  augment(base_juguete) %>% select(sexo,ingreso_individual,.pred)

