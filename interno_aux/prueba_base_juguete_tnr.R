library(tidyverse)
library(tidymodels)

set.seed(999)
ejemplo<- data.frame(
  realiza_trabajo_domestico = c(rep(0,100),rep(1,100)),
  horas_trabajo_mercado = c(sample(30:70,size = 100,replace = T),
                            sample(0:50,size = 100,replace = T)
  ),
  ingreso_individual    = round(c(
    abs(rnorm(n = 90))*30000+5000,
    abs(rnorm(n = 10))*10000,
    abs(rnorm(n = 10))*30000+5000,
    abs(rnorm(n = 90))*10000),
    digits = 1),
  menores_hogar         = c(sample(c("Si","No"),prob = c(0.2,0.8),replace = T,size = 100),
                            sample(c("Si","No"),prob = c(0.8,0.2),replace = T,size = 100))
) 

ejemplo$ingreso_individual[ejemplo$horas_trabajo_mercado %in% 35:45 & ejemplo$realiza_trabajo_domestico == 0] <- sample(
  x = 1:70000,
  prob = c(rep(0.000006666667,30000),
                       rep(0.00002,40000)), 
  size = sum(ejemplo$horas_trabajo_mercado %in% 35:45 & ejemplo$realiza_trabajo_domestico == 0),
  replace = T)

###### mi version ########

t_muestra <- 600

set.seed(333)
ejemplo<- data.frame(
  sexo = c(rep(2,0.5*t_muestra), #2 mujer
           rep(1,0.5*t_muestra)), #1 varon
  
  realiza_trabajo_domestico = c(sample(c(0,1),prob = c(0.083,0.917),replace = T,size = 0.5*t_muestra),
                                sample(c(0,1),prob = c(0.249,0.751),replace = T,size = 0.5*t_muestra)),
  
  horas_trabajo_mercado = c(sample(0:50,
                                   size = 0.5*t_muestra,replace = T),
                            sample(30:70,
                                   size = 0.5*t_muestra,replace = T)
  ),
  
  horas_trabajo_domestico =c(rep(0,t_muestra)),
  
  ingreso_individual    = c(rep(0,t_muestra)),
  
  menores_hogar         = c(sample(c(0:7),prob = c(0.4,0.3,0.2,0.05,0.05/4,0.05/4,0.05/4,0.05/4),replace = T,size = 0.5*t_muestra),
                            sample(c(0:7),prob = c(0.5,0.3,0.1,0.05,0.05/4,0.05/4,0.05/4,0.05/4),replace = T,size = 0.5*t_muestra))
) 



###Genero la variable de horas de trabajo doméstico######

ejemplo$horas_trabajo_domestico[ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 2] <- sample(1:90,
       prob = c(rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 2)))/2,20),
                rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 2)))*2,20),
                rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 2)))*1,30),
                rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 2)))/4,20)),
       size = sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 2),replace = T)



ejemplo$horas_trabajo_domestico[ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 1] <- sample(1:60,
prob = c(rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 1)))*1,10),
         rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 1)))/2,10),
         rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 1)))/4,20),
         rep((1/(sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 1)))/8,20)),
size = sum(ejemplo$realiza_trabajo_domestico == 1 & ejemplo$sexo == 1),replace = T)

###Genero la variable de ingresos######


###varones

ejemplo$ingreso_individual[ejemplo$horas_trabajo_domestico < 20& ejemplo$sexo == 1] <- sample(
  x = 20000:65000,
  
  prob = c(rep((1/(sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 1)))/2,15000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 1)))*1,15000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 1)))/2,15001)),
  
  size = sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 1),
  replace = T)

ejemplo$ingreso_individual[ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 1] <- sample(
  x = 10000:60000,
  
  prob = c(rep((1/(sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 1)))/2,15000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 1)))*1,20000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 1)))/2,15001)),
  
  size = sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 1),
  replace = T)


ejemplo$ingreso_individual[ejemplo$horas_trabajo_domestico >= 36& ejemplo$sexo == 1] <- sample(
  x = 1000:50000,
  
  prob = c(rep((1/(sum(ejemplo$horas_trabajo_domestico >= 36 & ejemplo$sexo == 1)))/2,14500),
           rep((1/(sum(ejemplo$horas_trabajo_domestico >= 36 & ejemplo$sexo == 1)))*1,20000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico >= 36 & ejemplo$sexo == 1)))/2,14501)),
  
  size = sum(ejemplo$horas_trabajo_domestico >= 36& ejemplo$sexo == 1),
  replace = T)


##Mujeres

ejemplo$ingreso_individual[ejemplo$horas_trabajo_domestico < 20& ejemplo$sexo == 2] <- sample(
  x = 20000:60000,
  
  prob = c(rep((1/(sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 2)))/2,10000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 2)))*1,20000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico < 20 & ejemplo$sexo == 2)))/2,10001)),
  
  size = sum(ejemplo$horas_trabajo_domestico < 20& ejemplo$sexo == 2),
  replace = T)

ejemplo$ingreso_individual[ejemplo$horas_trabajo_domestico %in% 21:35& ejemplo$sexo == 2] <- sample(
  x = 10000:40000,
  
  prob = c(rep((1/(sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 2)))/2,5000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 2)))*1,20000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico %in% 21:35 & ejemplo$sexo == 2)))/2,5001)),
  
  size = sum(ejemplo$horas_trabajo_domestico %in% 21:35& ejemplo$sexo == 2),
  replace = T)

ejemplo$ingreso_individual[ejemplo$horas_trabajo_domestico >= 36& ejemplo$sexo == 2] <- sample(
  x = 0:30000,
  
  prob = c(rep((1/(sum(ejemplo$horas_trabajo_domestico >= 36 & ejemplo$sexo == 2)))/2,5000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico >= 36 & ejemplo$sexo == 2)))*1,20000),
           rep((1/(sum(ejemplo$horas_trabajo_domestico >= 36 & ejemplo$sexo == 2)))/2,5001)),
  
  size = sum(ejemplo$horas_trabajo_domestico >= 36& ejemplo$sexo == 2),
  replace = T)


ejemplo%>% 
  filter(horas_trabajo_domestico > 0) %>% 
  ggplot(aes(x=horas_trabajo_domestico, y=ingreso_individual, color = sexo)) + 
  geom_point() + 
  geom_smooth(method='lm', color = 'red', se = FALSE) +
  theme_minimal()


####Toco la variable de horas trabajadas en el mercado#####


ejemplo$horas_trabajo_mercado[ejemplo$sexo == 2&ejemplo$ingreso_individual > 50000] <- sample(30:50, size = sum(ejemplo$sexo == 2&ejemplo$ingreso_individual > 50000),replace = T)

ejemplo$horas_trabajo_mercado[ejemplo$sexo == 2&ejemplo$ingreso_individual %in% 30000:50000] <- sample(10:40, size = sum(ejemplo$sexo == 2&ejemplo$ingreso_individual %in% 30000:50000),replace = T)

ejemplo$horas_trabajo_mercado[ejemplo$sexo == 2&ejemplo$ingreso_individual < 30000] <- sample(0:20, size = sum(ejemplo$sexo == 2&ejemplo$ingreso_individual < 30000),replace = T)



##varones


ejemplo$horas_trabajo_mercado[ejemplo$sexo == 1&ejemplo$ingreso_individual > 60000] <- sample(40:70, size = sum(ejemplo$sexo == 1&ejemplo$ingreso_individual > 60000),replace = T)

ejemplo$horas_trabajo_mercado[ejemplo$sexo == 1&ejemplo$ingreso_individual %in% 30000:60000] <- sample(30:50, size = sum(ejemplo$sexo == 1&ejemplo$ingreso_individual %in% 30000:60000),replace = T)

ejemplo$horas_trabajo_mercado[ejemplo$sexo == 1&ejemplo$ingreso_individual < 30000] <- sample(25:40, size = sum(ejemplo$sexo == 1&ejemplo$ingreso_individual < 30000),replace = T)


ejemplo%>% 
  filter(horas_trabajo_mercado > 0) %>% 
  ggplot(aes(x=horas_trabajo_mercado, y=ingreso_individual, color = sexo)) + 
  coord_flip()+
  geom_point() + 
  geom_smooth(method='lm', color = 'red', se = FALSE) +
  theme_minimal()


###Toco la variable de menores en el hogar######

##mujeres

ejemplo$menores_hogar[ejemplo$ingreso_individual <40000 & ejemplo$sexo == 2] <- sample(c(0:7),
                                                     prob = c(0.01,0.7,0.3,0.2,0.01,0.01,0.01,0.01),replace = T,
                                                     size = sum(ejemplo$ingreso_individual <40000  & ejemplo$sexo == 2))

ejemplo$menores_hogar[ejemplo$ingreso_individual >=40000   & 
                        ejemplo$sexo == 2] <- sample(c(0:2),
                                                     prob = c(0.7,0.2,0.1),replace = T,
                                                     size = sum(ejemplo$ingreso_individual >= 40000 & ejemplo$sexo == 2))


##varones

ejemplo$menores_hogar[ejemplo$ingreso_individual <40000 & 
                        ejemplo$sexo == 1] <- sample(c(0:7),
                                                     prob = c(0.01,0.7,0.3,0.2,0.01,0.01,0.01,0.01),replace = T,
                                                     size = sum(ejemplo$ingreso_individual <40000 & ejemplo$sexo == 1))

ejemplo$menores_hogar[ejemplo$ingreso_individual >=40000 & 
                        ejemplo$sexo == 1] <- sample(c(0:2),
                                                     prob = c(0.7,0.2,0.1),replace = T,
                                                     size = sum(ejemplo$ingreso_individual >=40000 & ejemplo$sexo == 1))


ejemplo%>% 
  ggplot(aes(x=menores_hogar, y=ingreso_individual, color = sexo)) + 
  coord_flip()+
  geom_point() + 
  geom_smooth(method='lm', color = 'red', se = FALSE) +
  theme_minimal()

#summary(ejemplo)

ejemplo$sexo[ejemplo$sexo == 2] <- "Mujer"
ejemplo$sexo[ejemplo$sexo == 1] <- "Hombre"

ejemplo$realiza_trabajo_domestico[ejemplo$realiza_trabajo_domestico == 0] <- "No"
ejemplo$realiza_trabajo_domestico[ejemplo$realiza_trabajo_domestico == 1] <- "Si"

####Pruebo hacer un modelito######


lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_fit <- lm_spec %>%
  fit(ingreso_individual ~ horas_trabajo_domestico, data = ejemplo)

lm_fit %>% 
  pluck("fit") %>%
  summary()

lm_fit2 <- lm_spec %>%
  fit(ingreso_individual ~ horas_trabajo_domestico + horas_trabajo_mercado + sexo + menores_hogar, data = ejemplo)

lm_fit2 %>% 
  pluck("fit") %>%
  summary()

lm_fit3 <- lm_spec %>%
  fit(ingreso_individual ~ horas_trabajo_domestico + sexo:horas_trabajo_mercado +horas_trabajo_mercado + sexo + menores_hogar, data = ejemplo)

lm_fit3 %>% 
  pluck("fit") %>%
  summary()


#####Guardo#####

saveRDS(ejemplo, "fuentes/eut_juguete.RDS")

