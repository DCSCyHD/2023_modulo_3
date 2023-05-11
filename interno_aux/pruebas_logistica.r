library(tidyverse)
ejemplo <- data.frame(x = seq(-10,10,0.1)) %>% 
  mutate(beta10 = 10*x,
      #   beta5 = 5*x,
         beta2 = 2*x,
         beta0.5 = 0.5*x,
         beta_menos_2 = -2*x,
         beta_menos_0.5 = -0.5*x,
         beta_menos_10 = -10*x) %>% 
  mutate_at(.vars = 2:7,~(1/(1+exp(-.)))) %>% 
  pivot_longer(cols = 2:7) %>% 
  mutate(name = factor(name,levels = c("beta10",
                                  #     "beta5",
                                       "beta2",
                                       "beta0.5",
                                  "beta_menos_0.5",
                                  
                                       "beta_menos_2",
                                       "beta_menos_10")))
    
ejemplo %>% 
  filter((str_sub(name,1,5)=="beta_")) %>% 
  ggplot(aes(x = x,y = value,group = name,color = name))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d()
  

y_b1 = 1/(1+exp(-x)),
         y_b10 = 10*x)
