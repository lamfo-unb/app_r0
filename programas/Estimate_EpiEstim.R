#install.packages("EpiEstim")
library(EpiEstim)
library(ggplot2)
library(dplyr)
library(readxl)
library(utils)
library(httr)
library(tidyverse)
library(RCurl)

##########################################################################
###  Programa para estimar Número Básico de Repodrução Instantâneo     ###
###  https://cran.r-project.org/web/packages/EpiEstim/EpiEstim.pdf     ###
### by. Alex Rodrigues                                                 ###
##########################################################################

Sys.setlocale("LC_TIME", "Portuguese")

base.mun <- read.csv(text = getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"))
base.mun$state <- as.character(base.mun$state)
base.mun$date <- as.Date(base.mun$date)
base.mun$state <- ifelse(base.mun$state == "TOTAL", "Brasil", base.mun$state)
base.mun <- base.mun %>% filter(date!=max(date))


base <- base.mun %>% select(date, state, totalCases, newCases, deaths, newDeaths) %>% 
  group_by(date, state) %>% summarise_all(list(sum)) %>% 
  mutate(CFR = deaths/totalCases)


states.100casos <- base %>% group_by(state) %>%  filter(totalCases>100) %>% count(state) %>% filter(n>5)
base.100casos <- base %>% group_by(state) %>%  filter(totalCases>100 & state %in% states.100casos$state)
nmax <- max(states.100casos$n)

#janela rolante de 5 e 10 dias
resu5 <- list()
resu15<- list()


for(i in 1:nrow(states.100casos)){
     
  #print(states.100casos$state[i])
  base.state <-base.100casos %>% filter(state==states.100casos$state[i]) %>% arrange(date) %>% mutate(newCases=abs(newCases))
     
  if(nrow(base.state)>15){    
    for(j in c(5,15)){
        
        TT <-  nrow(base.state)
        t_start <- seq(2, TT-j)
        t_end <- t_start + j 
        
        ress <- estimate_R( base.state$newCases, 
                                 method="parametric_si",
                                 config = make_config(list(
                                   t_start = t_start,
                                   t_end = t_end,
                                   mean_si = 4.7, 
                                   std_si = 2.9))
                             )
        if(j==5){
          resu5[[i]] <- data.frame(date=base.state$date[t_start],IC.i.5=ress$R$`Quantile.0.025(R)`,Est.5=ress$R$`Mean(R)`,IC.s.5=ress$R$`Quantile.0.975(R)`)
          names(resu5)[i] <- states.100casos$state[i]
        }
        if(j==15){
          resu15[[i]] <- data.frame(date=base.state$date[t_start],IC.i.15=ress$R$`Quantile.0.025(R)`,Est.15=ress$R$`Mean(R)`,IC.s.15=ress$R$`Quantile.0.975(R)`)
          names(resu15)[i] <- states.100casos$state[i]
        }
      }
      
      
  }else{
    j=5
    TT <-  nrow(base.state)
    t_start <- seq(2, TT-j)
    t_end <- t_start + j 
    
    ress <- estimate_R( base.state$newCases, 
                        method="parametric_si",
                        config = make_config(list(
                          t_start = t_start,
                          t_end = t_end,
                          mean_si = 4.7, 
                          std_si = 2.9))
                        )
    resu5[[i]] <- data.frame(date=base.state$date[t_start],IC.i.5=ress$R$`Quantile.0.025(R)`,Est.5=ress$R$`Mean(R)`,IC.s.5=ress$R$`Quantile.0.975(R)`)
    names(resu5)[i] <- states.100casos$state[i]
    }
}



Rt.state <- resu5[["RJ"]] %>% left_join(resu15[["RJ"]],by="date") 


ggplot(Rt.state , aes(x = date)) +
  geom_line(aes(y = Est.5, color = "5 dias"), size = 0.8) + 
  geom_line(aes(y = Est.15, color = "15 dias"), size = 0.8) + 
  geom_ribbon(mapping = aes(x = date, 
                                   ymax = IC.s.5, ymin = IC.i.5),
              alpha = 0.5, fill = "brown1") +
  geom_ribbon(mapping = aes(x = date, 
                            ymax = IC.s.15, ymin = IC.i.15),
              alpha = 0.5, fill = "royalblue") +
  theme_classic() + 
  labs(title = paste("Número de Reprodução Instantâneo - 5 e 15 dias"),
       x = "Data",
       y = expression(R[t]),
       color="Janela Rolante:"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    #plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    axis.text.x=element_text(angle=75, hjust=1)
  ) +
  scale_x_date(date_breaks = "2 day", date_labels =  "%d %b")+
  scale_color_manual(values = c("5 dias" = "red","15 dias" = "blue"))
