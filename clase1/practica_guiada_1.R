## ----setup, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=FALSE, tidy=FALSE,
                      fig.width=8)


## ----librerias, results='hide'-----------------------------------------------------------------------
library(tidyverse)
library(viridis)
library(ggridges)
options(dplyr.summarise.inform = FALSE)


## ----------------------------------------------------------------------------------------------------
base_EAUH_TNR <- readRDS("./fuentes/base_EAUH_TNR.RDS") %>% 
  rename("Sexo" = "CH04")


## ----------------------------------------------------------------------------------------------------
names(base_EAUH_TNR)


## ----------------------------------------------------------------------------------------------------
head(base_EAUH_TNR,10)


## ----------------------------------------------------------------------------------------------------
mean(c(1,2,3,4))


## ----------------------------------------------------------------------------------------------------
var(c(1,2,3,4))


## ----------------------------------------------------------------------------------------------------
sd(c(1,2,3,4))

sd(c(1,2,3,4))**2



## ----------------------------------------------------------------------------------------------------

base_EAUH_TNR %>% 
  filter(CH06 > 17) %>% #Mayores de 18
ggplot(aes(x=TIEMPO_TDNR)) +
    geom_density(adjust=1.5, alpha=.4, fill = "green") +
    theme_minimal()+
  xlab("Horas de trabajo no remunerado") +
  xlim(0,40)


## ----------------------------------------------------------------------------------------------------
base_EAUH_TNR %>% 
  filter(CH06 > 17) %>% 
ggplot(aes(fill=Sexo, y=TIEMPO_TDNR, x=Sexo)) + 
    geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    scale_fill_manual(values = c("#91CBD765","#CA225E"))+
    theme_minimal()  +
    xlab("") +
    ylab("Horas de trabajo no remunerado") +
    ylim(0,40)


## ----------------------------------------------------------------------------------------------------
base_EAUH_TNR %>%
  filter(CH06 > 17) %>% 
  filter(!is.na(horas_mercado)) %>% 
mutate(horas_mercado_agrup = factor(case_when(horas_mercado < 15~ "< 15",
                                              horas_mercado %in% 15:34~ "15 a 34",
                                              horas_mercado %in% 35:45~ "35 a 45",
                                              horas_mercado > 45~ "> 45"),
                                    levels = c( "< 15","15 a 34","35 a 45","> 45"))) %>% 
ggplot(aes(y=TIEMPO_TDNR, x =horas_mercado_agrup , fill=Sexo)) +
    geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    theme_minimal()  +
    ylim(0,40)+
  ylab("Horas de trabajo no remunerado") +
  xlab("Horas de trabajo remunerado") +
  theme(legend.position = "top")+
  scale_fill_manual(values = c("#91CBD765","#CA225E"))


## ----------------------------------------------------------------------------------------------------
base_EAUH_TNR%>% 
  filter(CH06 > 17) %>% 
ggplot(aes(x=TIEMPO_TDNR, y=ingreso_indiv, color = Sexo)) + 
  geom_point(alpha = 0.4) + 
  ylim(0,100000)+
  xlim(0,40)+
  ylab("Ingresos individuales") +
  xlab("Horas de trabajo no remunerado") +
  geom_hline(yintercept = mean(base_EAUH_TNR$ingreso_indiv[base_EAUH_TNR$ingreso_indiv >0]),
             linewidth = 0.5)+
  geom_vline(xintercept = mean(base_EAUH_TNR$TIEMPO_TDNR), linewidth = 0.5)+
  theme_minimal()+
  scale_color_manual(values = c("#91CBD968","#CA225E"))+
  facet_wrap(~Sexo)


## ----------------------------------------------------------------------------------------------------

base_EAUH_TNR %>% 
  mutate(menores_hogar_agrup = factor(case_when(menores_hogar == 0~ "0",
                                              menores_hogar  == 1~ "1",
                                              menores_hogar  == 2~ "2",
                                              menores_hogar > 2~ "> 2"),
                                    levels = c( "0","1","2","> 2"))) %>%
  filter(CH06 > 17) %>% 
ggplot(aes(fill=Sexo, y=TIEMPO_TDNR, x=menores_hogar_agrup)) + 
    geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
    scale_fill_manual(values = c("#91CBD765","#CA225E"))+
    theme_minimal()  +
    xlab("Menores en el hogar") +
    ylab("Horas de trabajo no remunerado") +
    ylim(0,40)+
  theme(legend.position = "top")



## ----------------------------------------------------------------------------------------------------
corr <- base_EAUH_TNR %>% 
  # Seleccionamos las columnas numéricas
  select_if(is.numeric) %>% 
  # Calculamos la matriz de correlaciones
  cor()

corr


## ----------------------------------------------------------------------------------------------------
library(corrplot)


## ----------------------------------------------------------------------------------------------------
#Graficamos con nuestro propio espectro de colores
mi_paleta <- colorRampPalette(c("navy","#91CBD765", "#CA225E"))

corr %>% 
  corrplot(col = mi_paleta(200), tl.col = "black", method = "square")

