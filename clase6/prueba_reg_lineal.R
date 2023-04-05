library(eph)
library(tidyverse)
library(tidymodels)

eph <- eph::get_microdata(year = 2022,trimester = 3) 

base_con_clasificadores <-   eph %>% 
  filter(ESTADO == 1) %>% #Ocupados 
  filter(CAT_OCUP %in%c(1,2,3)) %>%
  eph::organize_caes() %>% 
  eph::organize_cno()

base_con_clasificadores$P21[base_con_clasificadores$P21 == -9] <- 0
base_con_clasificadores$TOT_P12[base_con_clasificadores$TOT_P12 == -9] <- 0
base_con_clasificadores$PP3E_TOT[base_con_clasificadores$PP3E_TOT == 999] <- 0
base_con_clasificadores$PP3F_TOT[base_con_clasificadores$PP3F_TOT == 999] <- 0
base_con_clasificadores$V5_M[base_con_clasificadores$V5_M == -9] <- 0
base_con_clasificadores$V12_M[base_con_clasificadores$V12_M == -9] <- 0

base_transf <- base_con_clasificadores %>% 
  filter(CH06 !=-1) %>% 
  filter(CH06 >17) %>% 
  mutate(
    grupos.calif = factor(case_when(
      CALIFICACION %in% c("Profesionales","Técnicos") ~ "Alta",
      CALIFICACION ==   "Operativos" ~ "Media",
      CALIFICACION ==   "No calificados" ~ "Baja"),
      levels = c("Baja","Media","Alta")),
    precario = factor(case_when(
      PP07H == 1 ~ "No",
      PP07H == 2 ~ "Si",
      PP07H == 0 | is.na(PP07H) ~ "No asalariado"),
      levels = c("No","Si","No asalariado")),
    tamanio.establecim = factor(case_when(
      PP04C %in% 1:6  |(PP04C %in% 99 & PP04C99 == 1)~ "Pequeño",
      PP04C %in% 7:8  ~ "Mediano",
      PP04C %in% 9:12 |(PP04C %in% 99 & PP04C99 == 3)~ "Grande"),
      levels = c("Pequeño","Mediano","Grande")),
    ingreso_laboral = TOT_P12 + P21,
    horas_totales = PP3E_TOT + PP3F_TOT
  ) %>% 
  filter(ingreso_laboral >0)


lm_spec <- linear_reg() %>%
  # set_mode("regression") %>%
  set_engine("lm")


receta <- recipe(x = base_transf,ingreso_laboral ~  precario + grupos.calif + tamanio.establecim + CH04 + CH06 + NIVEL_ED + caes_eph_label+horas_totales + PP03G) %>% 
  step_num2factor(CH04, levels = c("Varon","Mujer")) %>% 
  step_num2factor(PP03G, levels = c("mas_horas","ok_horas")) %>% 
  step_mutate(NIVEL_ED = ifelse(NIVEL_ED == 7,yes = 1,no = NIVEL_ED)) %>% 
  step_rename(EDAD = CH06) %>% 
  step_num2factor(NIVEL_ED,levels = c("Primaria incompleta","Primaria completa","Secundaria incompleta",
                                      "Secundaria completa","Terciario/univ incompleta","Terciario/univ completa")) %>% 
  step_interact( ~ CH04:grupos.calif) %>%
  step_other(caes_eph_label,threshold = 0.05) 


# receta_cocinada <- receta %>%
#   prep() %>%
#   bake(new_data = NULL)
# 
# contrasts(receta_cocinada$caes_eph_label)



wf <- workflow() %>%
  add_model(lm_spec) %>% 
  add_recipe(receta)  


fitted_model <- wf %>% 
  fit(.,data = base_transf)

veo <- tidy(fitted_model) %>% arrange(p.value)

glance(fitted_model)

### Alternativa más parecida a Mincer: 
## - Solo asalariados, 
## - log (Ingreso de ocup principal) 
## - Horas solo de ocup principal
unique(asalariados$REGION)

asalariados <- base_transf %>% 
  filter(CAT_OCUP == 3,P21>0,PP3E_TOT>0) %>% # Asalariados con ingreso y con horas
  mutate(log_salario = log(P21)) %>% 
  mutate(REGION = factor(REGION,labels = c("GBA","NOA","NEA","Cuyo","Pampeana","Patagonia")))

receta_mincer <- recipe(x = asalariados,log_salario ~  precario + grupos.calif + tamanio.establecim + CH04 + CH06 + NIVEL_ED + caes_eph_label+ PP3E_TOT + REGION) %>% 
  step_num2factor(CH04, levels = c("Varon","Mujer")) %>% 
#  step_num2factor(REGION, levels = c("GBA","NOA","NEA","Cuyo","Pampeana","Patagonia")) %>% 
#  step_num2factor(PP03G, levels = c("mas_horas","ok_horas")) %>% 
  step_mutate(NIVEL_ED = ifelse(NIVEL_ED == 7,yes = 1,no = NIVEL_ED)) %>% 
  step_rename(EDAD = CH06,horas_ocup_ppal = PP3E_TOT) %>% 
  step_num2factor(NIVEL_ED,levels = c("Primaria incompleta","Primaria completa","Secundaria incompleta",
                                      "Secundaria completa","Terciario/univ incompleta","Terciario/univ completa")) %>% 
  step_interact( ~ CH04:grupos.calif) %>%
  step_other(caes_eph_label,threshold = 0.05) 


wf <- workflow() %>%
  add_model(lm_spec) %>% 
  add_recipe(receta_mincer)  


fitted_model_2 <- wf %>% 
  fit(.,data = asalariados)

ver_model2 <- tidy(fitted_model_2) %>% arrange(p.value)

glance(fitted_model_2)

# Alternativa salario horario #### 
## - Solo asalariados, 
## - log (salario horario) 
## - Horas solo de ocup principal

asalariados_v3 <- base_transf %>% 
  filter(CAT_OCUP == 3,P21>0,PP3E_TOT>0) %>% # Asalariados con ingreso y con horas
  mutate(log_salario_horario = log(P21/PP3E_TOT)) %>% 
  mutate(REGION = factor(REGION,labels = c("GBA","NOA","NEA","Cuyo","Pampeana","Patagonia")))

receta_mincer_v3 <- recipe(x = asalariados_v3,log_salario_horario ~  precario + grupos.calif + tamanio.establecim + CH04 + CH06 + NIVEL_ED + caes_eph_label + REGION) %>% 
  step_num2factor(CH04, levels = c("Varon","Mujer")) %>% 
  #  step_num2factor(REGION, levels = c("GBA","NOA","NEA","Cuyo","Pampeana","Patagonia")) %>% 
  #  step_num2factor(PP03G, levels = c("mas_horas","ok_horas")) %>% 
  step_mutate(NIVEL_ED = ifelse(NIVEL_ED == 7,yes = 1,no = NIVEL_ED)) %>% 
  step_rename(EDAD = CH06) %>% 
  step_num2factor(NIVEL_ED,levels = c("Primaria incompleta","Primaria completa","Secundaria incompleta",
                                      "Secundaria completa","Terciario/univ incompleta","Terciario/univ completa")) %>% 
  step_interact( ~ CH04:grupos.calif) %>%
  step_other(caes_eph_label,threshold = 0.05) 


wf3 <- workflow() %>%
  add_model(lm_spec) %>% 
  add_recipe(receta_mincer_v3)  


fitted_model_3 <- wf3 %>% 
  fit(.,data = asalariados_v3)

ver_model3 <- tidy(fitted_model_3) %>% arrange(p.value)

glance(fitted_model_3)
