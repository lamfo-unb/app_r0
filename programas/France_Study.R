library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(socialmixr)



#######################################
### Study replication : https://www.medrxiv.org/content/10.1101/2020.04.20.20072413v2
######################################



#Simulated Study
x <- seq(1,45)
y <- round(exp(x*0.3))

prob.hospit= 0.026
prob.ICU = 0.182
prob.death = 0.20

base <- data.frame(date=as.Date("2020-03-01")+x-1,totalCases = y,newCases=y-lag(y))
set.seed(94^3)
newHospit <- sapply(base$newCases,function(x){rbinom(1,size = x,prob =prob.hospit )})


dateHospit <- sapply(na.omit(newHospit),function(x){data.frame(round(rexp(x,rate=1/11)))}) 
names(dateHospit) <- base$date[!is.na(newHospit)]
dateHospit<- lapply(seq_along(dateHospit),function(aa,bb,i){as.numeric(aa[[i]]) + as.Date(bb[[i]])},
                    aa=dateHospit,bb=names(dateHospit))

dateICU <- sapply(dateHospit,function(x){x[rbernoulli(length(x),p = prob.ICU)] + rexp(1,rate=1/2)}) 
dateDeath <- sapply(dateHospit,function(x){x[rbernoulli(length(x),p = prob.death)] + rexp(1,rate=1/15)}) 

base.Hospit <- data.frame(table(do.call("c", dateHospit))) %>% mutate(Var1=as.Date(Var1))
names(base.Hospit) <- c("date","newHospit")
base.ICU <- data.frame(table(do.call("c", dateICU))) %>% mutate(Var1=as.Date(Var1))
names(base.ICU) <- c("date","newICU")
base.Death <- data.frame(table(do.call("c", dateDeath))) %>% mutate(Var1=as.Date(Var1))
names(base.Death) <- c("date","newDeath")

base <- base %>% full_join(base.Hospit,by = "date") %>% full_join(base.ICU,by = "date") %>%
                full_join(base.Death,by="date")

#####Contact Matrix

list_surveys()



