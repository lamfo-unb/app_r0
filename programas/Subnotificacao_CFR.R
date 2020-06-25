library(readxl)
library(data.table)
library(dplyr)
library(XLConnect)
library(RCurl)
library(tidyverse)


############################
### Subnotificação COVID 19
## By. AleX 

#####
##https://www.medrxiv.org/content/10.1101/2020.03.14.20036178v2.article-metrics
##https://ciis.fmrp.usp.br/covid19-subnotificacao/

#Análise Descritiva

covid.world <- read_xlsx("bases/COVID-19-World.xlsx")

covid.world <- covid.world  %>% rename(newdeaths= deaths, date = dateRep) %>% arrange(geoId,date) %>% 
  as.data.table()


covid.world[,totalCases := cumsum(cases),by=c("geoId")]
covid.world[,deaths := cumsum(newdeaths),by=c("geoId")]
covid.world[,CFR := deaths/totalCases,by=c("geoId")]


covid.world.plot <-  covid.world %>% filter(date>as.Date("2020-01-31") & geoId %in% c("BR","IT","KR","US","UK","DE"))
ggplot(covid.world.plot, aes(x=date,y=CFR, fill=geoId, colour=geoId)) + geom_line()+
  labs(colour="País:")+xlab("Data") + ylab("Taxa de Letalidade")+
  scale_colour_manual(values=c("green","blue","red","darkblue","darkred","darkgreen"),
                      breaks=c("BR","DE","IT","KR","UK","US"),
                      labels=c("Brasil","Alemanha","Itália","Coréia do Sul","Reino Unido","Estados Unidos"))

ggsave(paste0("resultados/CFR_world.pdf"),
       height = 8,
       width = 12)

#Manipulação Base
#names.sheet <- excel_sheets(path = "projecoes_2018_populacao_IBGE.xls")
#pop2020.UF <- list()
# for(i in 1:length(names.sheet)){
#   base.pop.t <- read_xls("projecoes_2018_populacao_IBGE.xls",sheet = names.sheet[i])
#   l1 <- which(base.pop.t[,1]=="POPULAÇÃO TOTAL - GRUPOS ETÁRIOS")+2
#   pop2020.UF[[i]]<- read_xls("projecoes_2018_populacao_IBGE.xls",sheet = names.sheet[i],range =  paste0("A",l1,":N",l1+20)) %>%
#                     select(Faixa_Etaria=`GRUPO ETÁRIO`,`2020`)%>% filter( Faixa_Etaria !="Total" ) %>% mutate(Faixa_Etaria.id = c(rep(1:8,each=2),rep(9,3)) ) %>%
#                     group_by(Faixa_Etaria.id) %>% summarise(Pop.2020=sum(`2020`)) %>% ungroup() %>% mutate(percent.pop=Pop.2020/sum(Pop.2020))
#   names(pop2020.UF)[i] <- names.sheet[i]
# }
#saveRDS(pop2020.UF,file="pop2020.UF.RDS")

pop2020.UF<-  readRDS("bases/pop2020.UF.RDS")


#População Korea: http://data.un.org/Data.aspx?d=POP&f=tableCode%3A22
pop.Korea <- read.csv("bases/UNdata_Export_20200508_095240534.csv") %>% filter(grepl("-",Age) & Sex=="Both Sexes" ) %>%
              mutate(Faixa_Etaria.id= c(rep(1,3),rep(2:8,each=2),rep(9,4))) %>% group_by(Faixa_Etaria.id) %>%
              summarise(pop.Korea.2019 = sum(Value)) %>% ungroup() %>% mutate(percent.pop.Korea=pop.Korea.2019/sum(pop.Korea.2019))

#População Itália 
pop.Italy <- read.csv("bases/UNdata_Export_20200519_003338514.csv") %>% filter(grepl("-",Age) & Sex=="Both Sexes" ) %>%
  mutate(Faixa_Etaria.id= c(rep(1,3),rep(2:8,each=2),rep(9,6))) %>% group_by(Faixa_Etaria.id) %>%
  summarise(pop.Italy.2018 = sum(Value)) %>% ungroup() %>% mutate(percent.pop.Italy=pop.Italy.2018/sum(pop.Italy.2018))


### CFR KOREA 
# https://europepmc.org/article/med/32233163#
CFR.korea <- read_xlsx("bases/CFR.KOREA.xlsx")
V.B <- crossprod(pop.Korea$percent.pop.Korea,CFR.korea$CFR)

### CFR ITALY 
# https://europepmc.org/article/med/32233163#
CFR.italy <- read_xlsx("bases/CFR.ITALIA.xlsx")
V.B.IT <- crossprod(pop.Italy$percent.pop.Italy,CFR.italy$CFR)



vtb.ff <- function(x,base2,V.B=V.B){
  V.T <- crossprod(x$percent.pop,base2$CFR)
  V.TB <- V.T/V.B 
  return(V.TB)
}
#Vtb.UF <- data.frame(VTB=sapply(pop2020.UF, vtb.ff,base2=CFR.korea,V.B=V.B))
#Vtb.UF$state <- row.names(Vtb.UF)
#saveRDS( Vtb.UF,file = "Vtb.UF.SK.RDS")
Vtb.UF.SK <- readRDS(file = "bases/Vtb.UF.SK.RDS")

#Vtb.UF.it <- data.frame(VTB=sapply(pop2020.UF, vtb.ff,base2=CFR.italy,V.B=V.B.IT))
#Vtb.UF.it $state <- row.names(Vtb.UF.it )
#saveRDS( Vtb.UF.it,file = "Vtb.UF.ITA.RDS")
Vtb.UF.ITA <- readRDS(file = "bases/Vtb.UF.ITA.RDS")



#Base Município
base.mun <- read.csv(text = getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"))
base.mun$state <- as.character(base.mun$state)
base.mun$date <- as.Date(base.mun$date)
base.mun$state <- ifelse(base.mun$state == "TOTAL", "BRASIL", base.mun$state)
base.mun <- base.mun %>% filter(date!=max(date))
base.mun <-  base.mun %>% mutate(is.last = max(date))

base.mun.last <- base.mun%>% filter(is.last==date)
base.mun.100 <- base.mun.last %>% filter(totalCases>100)
base.SP <- base.mun.last %>% filter(state=="SP")

hist(base.mun.100$deaths_by_totalCases,breaks = "fd")
summary(base.mun.100$deaths_by_totalCases)

#base estado
base <- base.mun %>% select(date, state, totalCases, newCases, deaths, newDeaths) %>% 
  group_by(date, state) %>% summarise_all(list(sum)) %>% 
  mutate(CFR.obs = deaths/totalCases) %>% ungroup() %>% 
  left_join(base.mun %>% group_by(state) %>% summarise(firt.date=min(date)),by="state")

# base <- base.mun %>% 
#   left_join(base.mun %>% group_by(state) %>% summarise(firt.date=min(date)),by="state") %>%
#   mutate(CFR.obs = deaths_by_totalCases)

#correção número de casos
CFR.KS.bb <-0.022
lag.days <- 12
base.correct <- base %>% left_join(Vtb.UF.SK,by="state" ) %>% filter(deaths>10) %>%
                      mutate(totalCases.est.SK = round(deaths/(VTB*CFR.KS.bb)),date.cases = date-lag.days) %>%  
                      arrange(state,date.cases)%>%group_by(state) %>%
                      mutate(newCases.est.SK=totalCases.est.SK-lag(totalCases.est.SK)) %>%
                      select(date.cases,state,totalCases.est.SK,newCases.est.SK,VTB)
#correção número de casos Itália
CFR.ITA.bb <-0.145
lag.days <- 12
base.correct.2 <- base %>% left_join(Vtb.UF.ITA,by="state" ) %>% filter(deaths>10) %>%
  mutate(totalCases.est.ITA = round(deaths/(VTB*CFR.ITA.bb)),date.cases = date-lag.days) %>%  
  arrange(state,date.cases)%>%group_by(state) %>%
  mutate(newCases.est.ITA=round(totalCases.est.ITA-lag(totalCases.est.ITA))) %>%
  select(date.cases,state,totalCases.est.ITA,newCases.est.ITA,VTB)



base <- base%>% left_join(base.correct, by=c("date"="date.cases","state"="state")) %>%
  left_join(base.correct.2, by=c("date"="date.cases","state"="state"))

base_long <- base %>% na.exclude() %>% gather(key="variavel",value="valor",-date,-state)

ggplot(base_long.plot %>% na.exclude(), aes(x=date,y=log(valor), fill=variavel,colour=variavel))+geom_line()+
  labs(color=NULL)+xlab("Data") + ylab("log(Estimativa)")+
  scale_colour_manual(values=c("red","blue","green"),
                      breaks=c("totalCases.est.SK","totalCases.est.ITA","totalCases"),
                      labels=c("Estimativa Coréia","Estimativa Itália","Observado"))

ggsave(paste0("resultados/CFR_",state.cr,".pdf"),
       height = 8,
       width = 12)


vars.cr =  c("totalCases", "totalCases.est.SK","totalCases.est.ITA")
base_long.plot <- base_long %>% filter(variavel %in% vars.cr) %>% na.omit()

base_long.plot_relativo <- base_long.plot %>%
  spread(key = "variavel",value="valor") %>%
  mutate(IT = totalCases/totalCases.est.ITA-1,
         SK = totalCases/totalCases.est.SK-1) %>%
  select(date,state,IT,SK) %>%
  gather(key = "variavel",value="valor",-date,-state)


ufscd <- c('11','RO','12','AC','13','AM','14','RR','15','PA','16','AP','17','TO','21','MA','22','PI','23','CE','24','RN','25','PB','26','PE','27','AL','28','SE','29','BA','31','MG','32','ES','33','RJ','35','SP','41','PR','42','SC','43','RS','50','MS','51','MT','52','GO','53','DF')
ufscd <- matrix(ufscd,ncol=2,byrow=T) %>% data.frame()
names(ufscd) <- c("cod","ufnome")  
ufscd <- ufscd %>%
  mutate(regiao = substr(cod,1,1))


library(lubridate)

base_long.plot_relativo <- base_long.plot_relativo %>%
  left_join(ufscd,by = c("state"="ufnome"))




# New facet label names for dose variable
dose.labs <- c("Norte", "Sul", "Centro-Oeste","Nordeste","Sudeste")
names(dose.labs) <- c("1", "4", "5","2","3")

# New facet label names for supp variable
supp.labs <- c("Padrão Itália", "Padrão Coréia do Sul")
names(supp.labs) <- c("IT", "SK")




ggplot()+
  geom_line(data = base_long.plot_relativo %>% na.exclude(), aes(x=date,y=valor, 
                                                          fill=state,
                                                          colour=state),alpha=.4)+
  geom_text(data = base_long.plot_relativo %>% na.exclude() %>% filter((day(date)%%10)==1), 
            mapping = aes(x=date,y=valor, 
                fill=state,
                colour=state,
                label = state),cex=2)+
  labs(color=NULL)+xlab("Data") + ylab("Relativo") +
  facet_grid(variavel~regiao,scale="free", 
             labeller = labeller(regiao = dose.labs, 
                                 variavel = supp.labs))+
  theme(legend.position = "none") +
  scale_y_continuous(labels= function(x) paste0(x*100,"%")) +
  geom_hline(yintercept = 0,linetype=2,col="gray60")+ scale_x_date(date_labels = "%b") 

ggsave(paste0("resultados/CFR_geral.pdf"),
       height =6 ,
       width = 10)
