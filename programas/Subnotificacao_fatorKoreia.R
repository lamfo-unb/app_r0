library(readxl)
library(dplyr)
library(XLConnect)

install.packages("XLConnect")


############################
### Subnotificação COVID 19
## By. AleX R.

#####
##https://www.medrxiv.org/content/10.1101/2020.03.14.20036178v2.article-metrics
##https://ciis.fmrp.usp.br/covid19-subnotificacao/


#Manipulação Base
names.sheet <- excel_sheets(path = "projecoes_2018_populacao_IBGE.xls")

pop2020.UF <- list()

for(i in 1:length(names.sheet)){
  base.pop.t <- read_xls("projecoes_2018_populacao_IBGE.xls",sheet = names.sheet[i])
  l1 <- which(base.pop.t[,1]=="POPULAÇÃO TOTAL - GRUPOS ETÁRIOS")+2
  pop2020.UF[[i]]<- read_xls("projecoes_2018_populacao_IBGE.xls",sheet = names.sheet[i],range =  paste0("A",l1,":N",l1+20)) %>%
                    select(Faixa_Etaria=`GRUPO ETÁRIO`,`2020`)%>% filter( Faixa_Etaria !="Total" ) %>% mutate(Faixa_Etaria.id = c(rep(1:8,each=2),rep(9,3)) ) %>%
                    group_by(Faixa_Etaria.id) %>% summarise(Pop.2020=sum(`2020`)) %>% ungroup() %>% mutate(percent.pop=Pop.2020/sum(Pop.2020))
  names(pop2020.UF)[i] <- names.sheet[i]
}
#População Korea: http://data.un.org/Data.aspx?d=POP&f=tableCode%3A22
pop.Korea <- read.csv("UNdata_Export_20200508_095240534.csv") %>% filter(grepl("-",Age) & Sex=="Both Sexes" ) %>%
              mutate(Faixa_Etaria.id= c(rep(1,3),rep(2:8,each=2),rep(9,4))) %>% group_by(Faixa_Etaria.id) %>%
              summarise(pop.Korea.2019 = sum(Value)) %>% ungroup() %>% mutate(percent.pop.Korea=pop.Korea.2019/sum(pop.Korea.2019))
  
### CFR KOREA 
# https://europepmc.org/article/med/32233163#
CFR.korea <- read_xlsx("CFR.KOREA.xlsx")
V.B <- crossprod(pop.Korea$percent.pop.Korea,CFR.korea$CFR.KS)

vtb.ff <- function(x,base2,V.B=V.B){
  V.T <- crossprod(x$percent.pop,base2$CFR.KS)
  V.TB <- V.T/V.B 
  return(V.TB)
}
Vtb.UF <- data.frame(VTB=sapply(pop2020.UF, vtb.ff,base2=CFR.korea,V.B=V.B))
Vtb.UF$state <- row.names(Vtb.UF)
saveRDS( Vtb.UF,file = "Vtb.UF.RDS")


#Base Município
base.mun <- read.csv(text = getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"))
base.mun$state <- as.character(base.mun$state)
base.mun$date <- as.Date(base.mun$date)
base.mun$state <- ifelse(base.mun$state == "TOTAL", "BRASIL", base.mun$state)
base.mun <- base.mun %>% filter(date!=max(date))

#base estado
base <- base.mun %>% select(date, state, totalCases, newCases, deaths, newDeaths) %>% 
  group_by(date, state) %>% summarise_all(list(sum)) %>% 
  mutate(CFR.obs = deaths/totalCases) %>% ungroup() %>% 
  left_join(base.mun %>% group_by(state) %>% summarise(firt.date=min(date)),by="state")

#correção número de casos
CFR.KS.bb <-0.01635
lag.days <- 12
base.correct <- base %>% left_join(Vtb.UF,by="state" ) %>% filter(deaths>10) %>%
                      mutate(totalCases.est = deaths/(VTB*CFR.KS.bb),date.cases = date-lag.days) %>%  
                      arrange(state,date.cases)%>%group_by(state) %>%
                      mutate(newCases.est=totalCases.est-lag(totalCases.est)) %>%
                      select(date.cases,state,totalCases.est,newCases.est,VTB)
base <- base %>% left_join(base.correct, by=c("date"="date.cases","state"="state"))

