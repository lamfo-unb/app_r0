rm(list = ls())
# SOURCES ----
# https://github.com/nytimes/covid-19-data
# http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927
#https://catalog.data.gov/dataset/tiger-line-shapefile-2015-state-new-york-primary-and-secondary-roads-state-based-shapefile
# https://gisdata-njdep.opendata.arcgis.com/datasets/newjersey::municipal-boundaries-of-nj?geometry=-89.545%2C37.149%2C-59.904%2C43.026
# https://www.pasda.psu.edu/uci/DataSummary.aspx?dataset=41
# https://portal.ct.gov/DEEP/GIS-and-Maps/Data/GIS-DATA


# https://catalog.data.gov/dataset/tiger-line-shapefile-2016-nation-u-s-primary-roads-national-shapefile
# https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-county-and-equivalent-national-shapefile
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
# https://www2.census.gov/geo/tiger/TIGER2013/PRISECROADS/
# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
  
# PACKAGES ----
library(ape)
library(wordspace)
library(lmtest)
library(pscl)
library(MASS)
library(dplyr)
library(geobr)
library(tidyverse)
library(sf)
library(readxl)
library(maptools)
library(spdep)
library(data.table)
library(stpp)
library(maps)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)


# library(devtools)
# install_github("r-spatial/sf")
# library(sf)



# READ FILES ----
# última atualização 25/06/2020
covidmun <- read.csv( "bases\\us-counties.csv")
covidmun <-  covidmun %>% 
  mutate(fips=ifelse(county=="New York City",36061,fips)) %>%
  filter(trunc(fips/1000) %in% c(36,9,34,42))

 

shapefile  <- read_sf(dsn = "C:\\Users\\ACER\\Documents\\ACADEMICO\\ARTIGOS\\subnotificacaocovid19\\bases/EUA/tl_2017_us_county", layer = "tl_2017_us_county")
shapefile <- shapefile %>% filter(STATEFP %in% c("09","34","36","42"))


# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())



covidmun_t <- covidmun %>%
  mutate(date=(date %>% as.character() %>% as.Date))

covidmun_t <- covidmun_t %>% filter(date==max(covidmun_t$date))
shapefileny <- shapefile %>%
  mutate(fips=as.numeric(GEOID)) %>%
  left_join(covidmun_t,
            by = "fips")


shapefileny$area <- as.numeric(st_area(shapefileny)/(1000^2))

shapefileny <- bind_cols(shapefileny,
                         data.frame(st_coordinates(st_centroid(shapefileny))))


shapefileny$xny<- shapefileny %>% filter(fips==36061) %>% .$X %>%unique()
shapefileny$yny<- shapefileny %>% filter(fips==36061) %>% .$Y %>%unique()

shapefileny <- shapefileny %>%
  mutate(dx = X- xny,
         dy = Y- yny)




ggplot() +
  geom_sf(data=shapefileny %>% filter(fips!=36061),color = "gray85",
          aes(fill=(deaths)),size=.05) +
  scale_fill_distiller(palette = "Blues",
                       name="Confirmed per Km²",
                       direction = 1)+
  theme_minimal() 


# ShapeRodovias ----
#S1100: Primary roads;  S1200:Secondary roads 
shprodovias <- read_sf(dsn = "C:\\Users\\ACER\\Documents\\ACADEMICO\\ARTIGOS\\subnotificacaocovid19\\bases/EUA/tl_2013_36_prisecroads", 
                       layer = "tl_2013_36_prisecroads") 
shprodovias <- rbind(shprodovias,
                     read_sf(dsn = "C:\\Users\\ACER\\Documents\\ACADEMICO\\ARTIGOS\\subnotificacaocovid19\\bases/EUA/tl_2013_34_prisecroads",
                             layer = "tl_2013_34_prisecroads"),
                     read_sf(dsn = "C:\\Users\\ACER\\Documents\\ACADEMICO\\ARTIGOS\\subnotificacaocovid19\\bases/EUA/tl_2013_09_prisecroads",
                             layer = "tl_2013_09_prisecroads"),
                     read_sf(dsn = "C:\\Users\\ACER\\Documents\\ACADEMICO\\ARTIGOS\\subnotificacaocovid19\\bases/EUA/tl_2013_42_prisecroads",
                             layer = "tl_2013_42_prisecroads"))

# a <- read_sf(dsn="bases/EUA/tl_2013_36_prisecroads",layer="tl_2013_36_prisecroads")

# shprodovias <- st_transform(shprodovias,26918)
#st_crs(shprodovias) <-  st_crs(shapefileny)

shprodoviasny <- st_intersection(shapefileny,
                                 shprodovias)

# lista de rodovias que acessam NY city
lista_fips_ny <- shprodoviasny %>% filter(fips==36061)
lista_fips_ny <- lista_fips_ny %>% as.data.frame() %>% select(FULLNAME,MTFCC.1)  %>% unique() %>%
  mutate(tem_rodo_1=1)

# shape das rodovias que acessam NY city
shprodoviasnyny <-  shprodoviasny %>%
  left_join(lista_fips_ny,by=c("FULLNAME","MTFCC.1")) %>%
  filter(tem_rodo_1==1) 
  
shprodoviasnyny <- shprodoviasnyny %>% filter(MTFCC.1=="S1100")
                           
# municipios que estão em rodovias que accessam
county.eua.ny<- shapefileny %>% 
  filter(GEOID %in% unique(shprodoviasnyny$GEOID))



# Mapa 1: Estado de NY e rodovias
mapa1 <- ggplot()+
  geom_sf(data=shapefileny)+
  geom_sf(data=county.eua.ny,col="orange")+
  geom_sf(data=shprodoviasnyny ,col="red")


# 
# ggplot() +
#   geom_sf(data=shapefileny,color = "gray85",
#           aes(fill=(deaths)),size=.05) +
#   scale_fill_distiller(palette = "Blues",
#                        name="Confirmed per Km²",
#                        direction = 1)+
#   theme_minimal() +
#   geom_sf(data=county.eua.ny,col="orange")+
#   geom_sf(data=shprodoviasnyny ,col="red")+
#   geom_text(data=shprodoviasnyny  %>% filter(grepl("Northway",FULLNAME)),
#             aes(X,Y,label=FULLNAME),
#             cex=.6)

# length(table(shprodoviasnyny$GEOID))

# A <- shprodoviasnyny %>% filter(grepl("Northway",FULLNAME))


shapefileny <- shapefileny %>%
  mutate(state=ifelse(is.na(state),"New York",as.character(state)),
         county=ifelse(is.na(county),as.character(NAME),as.character(county)))

# removendo SP

nc_sp <- as(shapefileny  %>%
              filter(GEOID!="36061"),
            'Spatial')

neighbors <- poly2nb(nc_sp, queen=TRUE)
neighbors_sf <- as(nb2lines(neighbors, coords = coordinates(nc_sp)), 'sf')
neighbors_sf <- st_set_crs(neighbors_sf, st_crs(shapefileny))
neighbors<-nb2listw(neighbors,
                    style="W",
                    zero.policy=TRUE)


# lista de municipios que possuem estradas principais 
lista_fips_s1100 <- county.eua.ny%>% .$GEOID %>% unique()

shapefileny <- shapefileny %>% mutate(tem_rodo_1= ifelse(GEOID %in% lista_fips_s1100,1,0))





ggplot() +
  geom_sf(data=shapefileny,color = "gray85",
          aes(fill=(deaths)),size=.05) +
  scale_fill_distiller(palette = "Blues",
                       name="Confirmed per Km²",
                       direction = 1)+
  theme_minimal() +
  geom_sf(data=county.eua.ny,col="orange")+
  geom_sf(data=shprodoviasnyny ,col="red")


populacao <- read_excel("bases/co-est2019-annres.xlsx",skip = 3)

populacao <- populacao %>%
  mutate(nomes = gsub(".(.*) County.*","\\1",`...1`),
         state=gsub(".(.*) County, (.*)","\\2",`...1`)) %>%
  mutate(Npop = `2010`)







ddates <- c("2020-04-01","2020-05-01","2020-06-01")
dd <- "2020-06-01"
for(dd in ddates){
  
  shapefilenymapa <- shapefileny%>%
    left_join(populacao %>% select(nomes,state,Npop),
              by=c("county"="nomes",
                   'state'="state"))
  
  covidmun_t_t <- covidmun %>%
    mutate(date=(date %>% as.character() %>% as.Date)) %>%
    filter(date==dd) %>%
    rename("ncases"="cases")
    
  shapefilenymapa <- shapefilenymapa %>%
    left_join(covidmun_t_t %>% select(fips,ncases),
              by = "fips")
  


  
mapa5<-ggplot() +
  geom_sf(data=shapefilenymapa, color= "gray85", size=.15)+
  geom_sf(data=shapefilenymapa,
          aes(fill=(ncases/Npop)*10^5),size=.05,
          color = ifelse(shapefilenymapa$GEOID %in% county.eua.ny$GEOID,
                         "orange","gray85"))+
  scale_fill_distiller(palette = "Blues",
                       name="Confirmed per 100K",
                       direction = 1) +
  geom_sf(data=shprodoviasnyny ,col="red") +
  geom_text(data = shapefilenymapa%>% filter(tem_rodo_1==1),
            aes(X, Y, label = ncases) ,size = 1.1) +
  labs(subtitle=paste0("Confirmed cases at ",format(as.Date(dd) , "%d-%b")), size=8)+
  theme_minimal() 


mapa5
ggsave(paste0("resultados/NY_Casos_",gsub("-","_",dd),".pdf"),height = 8,width = 12)
}



# Loop modelo ----










# populacao <- read_sf(dsn = "bases/EUA/NYS_Civil_Boundaries_SHP", layer = "Counties")
# populacao <- populacao %>% as.data.frame() %>% select(-geometry) %>%
#   select(FIPS_CODE,POP2010)
dat_it <-as.Date((unique(covidmun$date)))[45]
paras_full <- NULL
AICs_TOTAL_full <- NULL
forcasts_temp_full <- NULL
dat_it_vec <- as.Date((unique(covidmun$date)))


for(dat_it in dat_it_vec){
  base_final <- covidmun %>% 
    arrange(county,date) 
  
  base_final<- base_final %>%
    mutate(deaths=ifelse(is.na(deaths),0,deaths),
           cases=ifelse(is.na(cases),0,cases))
  
  
  
  
  # a <- shapefileny %>% as.data.frame() %>% select(-geometry)
  base_final <- shapefileny%>%
    left_join(populacao %>% select(nomes,state,Npop),
              by=c("county"="nomes",
                   'state'="state"))%>%
    mutate(dist= ((dx^2 + dy^2)^(1/2))) %>%
    dplyr::select(county,X,Y,fips,tem_rodo_1,Npop,dist,area)  %>%
    left_join(base_final %>%
                filter(as.Date(date)==dat_it) %>%
                dplyr::select(fips,deaths,cases),
              by = "fips") 
  # a <- base_final %>% as.data.frame() %>% select(-geometry)
    # REMOVENDO NEWYORK E VAZIOS
  base_final <- base_final %>%
    filter(dist!=0)
  base_final <- base_final %>%
    filter(!is.na(county))
  
  # a <- base_final %>% as.data.frame() %>% select(-geometry)
  
  base_final<- base_final %>%
    mutate(deaths=ifelse(is.na(deaths),0,deaths),
           cases=ifelse(is.na(cases),0,cases))
  
  if(sum(base_final$deaths)>1){
    

    base_final %>% data.frame() %>%
      group_by(tem_rodo_1) %>%
      summarise(N=length(fips))
    
    
    # MODELOS MORTE
    #AJUSTAR
    # base_final$tem_rodo_1
    
    
      
    # ggplot() +
    #   geom_sf(data=base_final %>% filter(tem_rodo_1==1),color = "orange",
    #           aes(fill=(cases/Npop)),size=.05)+
    #   geom_sf(data=base_final %>% filter(tem_rodo_1==0),color = "gray85",
    #           aes(fill=(cases/Npop)),size=.05)
    
    

    modeloglmmorte <- glm(data= base_final, deaths ~ dist + tem_rodo_1 ,
                          family = poisson(link = "log"),
                          offset = log(Npop));

    modeloglmmortenb <-glm.nb(data= base_final, 
                              deaths ~ dist + tem_rodo_1+ 
                                offset(log(Npop)));
    
    modeloglmmortezi<- zeroinfl(data= base_final, 
                                deaths ~ dist+ tem_rodo_1 |
                                  dist + tem_rodo_1,
                                dist = "pois",
                                offset = log(Npop))
    
    
    modeloglmmortenbzi<- zeroinfl(data= base_final, 
                                  deaths ~ dist + tem_rodo_1|
                                    dist+ tem_rodo_1,
                                  dist = "negbin",
                                  offset = log(Npop))
    
    # MODELOS CONFIRMADOS 
    
    modeloglmconfirmado <- glm(data= base_final, 
                               cases ~ dist + tem_rodo_1,
                               family = poisson(link = "log"),
                               offset = log(Npop));summary(modeloglmconfirmado)
    
    modeloglmconfirmadonb <-glm.nb(data= base_final, 
                                   cases ~ dist + tem_rodo_1+ offset(log(Npop)))
    
    if(min(base_final$cases)==0){
    modeloglmconfirmadonbzi<- zeroinfl(data= base_final, 
                                       cases ~ dist+ tem_rodo_1 |
                                         dist + tem_rodo_1,
                                       dist = "negbin",
                                       offset = log(Npop))
    
    modeloglmconfirmadozi<- zeroinfl(data= base_final, 
                                     cases ~ dist+ tem_rodo_1 |
                                       dist + tem_rodo_1,
                                     dist = "pois",
                                     offset = log(Npop))
    }else{
      modeloglmconfirmadonbzi<-modeloglmconfirmadonb
      modeloglmconfirmadozi<-modeloglmconfirmadonb
      
    }
    
    # forecast----
    
    a <- modeloglmconfirmadonb
    v1 <- predict(a,base_final)
    a$coefficients[2:3] <- modeloglmmortenb$coefficients[2:3]
    v2 <- predict(a,base_final,type="response")
    
    forcasts_temp <- data.frame(previsao_modelo = exp(v1),
                                previsao_ajustada = exp(v2),
                                data_ref= dat_it,
                                codi_mun = base_final$fips)
    
    
    # https://stat.ethz.ch/pipermail/r-sig-geo/2012-August/015795.html
    # https://rpubs.com/corey_sparks/111362
    # https://cran.r-project.org/web/packages/glmmfields/vignettes/spatial-glms.html
    # https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
    
    AICs<-c(AIC(modeloglmmorte),
            AIC(modeloglmmortenb),
            AIC(modeloglmmortezi),
            AIC(modeloglmmortenbzi))
    plot(AICs, type="l", lwd=1.4, xaxt="n", xlab="")
    axis(1, at=1:4,labels=F) #7= number of models
    labels<-c( "Poi","NBin","ZIPoi","ZINBin")
    text(1:4, par("usr")[3]-.25, srt=45, adj=1, labels=labels, xpd=T)
    mtext(side=1, text="Model Specification", line=3)
    symbols(x= which.min(AICs), y=AICs[which.min(AICs)], circles=1, fg=2,lwd=2,add=T)
    
    AICsm <- AICs
    
    AICs<-c(AIC(modeloglmconfirmado),
            AIC(modeloglmconfirmadonb),
            AIC(modeloglmconfirmadozi),
            AIC(modeloglmconfirmadonbzi))
    plot(AICs, type="l", lwd=1.5, xaxt="n", xlab="")
    axis(1, at=1:4,labels=F) #7= number of models
    labels<-c( "Poi","NBin","ZIPoi","ZINBin")
    text(1:4, par("usr")[3]-.25, srt=45, adj=1, labels=labels, xpd=T)
    mtext(side=1, text="Model Specification", line=3)
    symbols(x= which.min(AICs), y=AICs[which.min(AICs)], circles=1, fg=2,lwd=.1,add=T)
    
    AICs_TOTAL <- rbind(AICsm,AICs)
    colnames(AICs_TOTAL) <- c("modeloglmconfirmado",
                              "modeloglmconfirmadonb",
                              "modeloglmconfirmadozi",
                              "modeloglmconfirmadonbzi") 
    
    AICs_TOTAL <- AICs_TOTAL%>%
      data.frame()
    
    
    AICs_TOTAL$data_ref <- dat_it
    AICs_TOTAL$tipovar <- c("morte","confirmado")
    
    base_final$resconfirmado <- modeloglmconfirmadonb$residuals
    base_final$resmorte <- modeloglmmortenb$residuals
    
    moranmortos <- moran.mc(modeloglmmortenb$residuals,neighbors,999)
    
    a<-summary(modeloglmconfirmadonb)
    
    paras <- a$coefficients %>% data.frame()
    paras$tipo_var <- rownames(paras)
    paras$aic <- a$aic
    paras$theta <- a$theta
    paras$deviance <- a$deviance
    paras$nulldeviance <- a$null.deviance
    paras$twologlik <- a$twologlik
    
    paras <- bind_cols(paras  ,
                       confint(modeloglmconfirmadonb,level = c(.95))%>% data.frame(),
                       confint(modeloglmconfirmadonb,level = c(.90))%>% data.frame())
    
    paras$tipo <-  'confirmado'
    parastemp <- paras
    
    a<-summary(modeloglmmortenb)
    
    paras <- a$coefficients %>% data.frame()
    paras$tipo_var <- rownames(paras)
    paras$aic <- a$aic
    paras$theta <- a$theta
    paras$deviance <- a$deviance
    paras$nulldeviance <- a$null.deviance
    paras$twologlik <- a$twologlik
    
    paras <- bind_cols(paras  ,
                       confint(modeloglmmortenb,level = c(.95))%>% data.frame(),
                       confint(modeloglmmortenb,level = c(.90))%>% data.frame()) 
    
    
    paras$tipo <-  'morte'
    parastemp <- bind_rows(parastemp,paras)
    parastemp$data_ref <- dat_it
    parastemp$moranmortos <- moranmortos$p.value
    paras_full <- bind_rows(paras_full,parastemp)
    
    AICs_TOTAL_full <- bind_rows(AICs_TOTAL_full,AICs_TOTAL)
    
    forcasts_temp_full <- bind_rows(forcasts_temp_full,forcasts_temp)
  }
  print(paste0(dat_it))
}


saveRDS(forcasts_temp_full,"resultados/previsao_NY.rds")

saveRDS(AICs_TOTAL_full,"resultados/modelos_NY.rds")

paras_full_t <-paras_full 


names(paras_full_t) <- c("estimativa","sd","z","pvalor",
                         "tipo_var","aic","theta","deviance","nulldeviance","twologlik",
                         "q025","q975","q050","q950","tipo","data_ref","moran")

saveRDS(paras_full_t,"resultados/estimativas_NY.rds")


# Analise output modelos ----
i <- 1
AICs_TOTAL_full_t <- AICs_TOTAL_full

for(i in 1:nrow(AICs_TOTAL_full)){
  vartemp <- as.numeric(apply(AICs_TOTAL_full[i,],1,which.min))
  AICs_TOTAL_full_t[i,vartemp] <- -AICs_TOTAL_full[i,vartemp]
}


AICs_TOTAL_full_t <- AICs_TOTAL_full_t %>%
  gather(key="var",value="AIC",-data_ref,-tipovar) %>%
  mutate(varby = paste0(tipovar,var)) %>%
  dplyr::select(varby,AIC,data_ref)

AICs_TOTAL_full_t <- AICs_TOTAL_full_t %>%
  mutate(valoraic = ifelse(AIC<0,
                           paste0(round(AIC,1),"*"),paste(round(AIC,1))))%>%
  dplyr::select(-AIC) %>%
  spread(key ="varby",value="valoraic")%>%
  mutate(data_ref=as.Date(data_ref) %>% as.character())



print(xtable::xtable(AICs_TOTAL_full_t),include.rownames=F)


# Analise output parÂmetros ----

paras_full_t <- paras_full_t %>%
  filter(month(as.Date(data_ref))>3)  

supp.labs <- c("Distância","Rodovia Primária","Rodovia secundária","Rodovia","Hospital")
names(supp.labs) <-c("dist","tem_rodo_2","tem_rodo_1","tem_rodo","QTDE_Hospit")

unique(paras_full_t$tipo_var)

ggplot()+
  geom_ribbon(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
              aes(x=as.Date(data_ref),
                  fill=tipo,
                  # col= tipo,
                  ymin=exp(q025),
                  ymax=exp(q975)),alpha=0.12)+
  geom_line(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
            aes(x=as.Date(data_ref),
                y=exp(estimativa),
                col= tipo))+
  facet_grid(tipo_var~.,scales = "free", 
             labeller = labeller(tipo_var = supp.labs))+
  labs(color=NULL,fill=NULL)+
  scale_colour_manual(values=c("red","blue"),
                      breaks=c("confirmado","morte"),
                      labels=c("Confirmados","Mortos"))+
  scale_fill_manual(values=c("red","blue"),
                    breaks=c("confirmado","morte"),
                    labels=c("Confirmados","Mortos"))+
  xlab("Data")+
  ylab("Estimativa")+
  geom_point(data=paras_full_t  %>% filter(tipo_var!="(Intercept)"),
             aes(x=as.Date(data_ref),y=exp(estimativa),
                 col= tipo,
                 shape= ifelse((pvalor<0.01),"1%",
                               ifelse((pvalor<0.05),"5%",
                                      ifelse((pvalor<0.1),"10%","Não")))))+
  labs(shape="Significativo")+
  scale_shape_manual(values = c(16,17,15,4),
                     breaks = c("1%","5%","10%","Não"))




ggsave("resultados/BETAS_NY.pdf",height = 8,width = 12)


paras_full_t_t <- paras_full_t %>%
  dplyr::select(estimativa,tipo_var,tipo,data_ref) %>%
  gather(key="var",value = "valor",-data_ref,-tipo,-tipo_var) %>%
  spread(key="tipo",value="valor") %>%
  mutate(taxa = exp(confirmado)/exp(morte)) %>%
  gather(key="tipo",value = "valor",confirmado,morte,taxa) 


ggplot()+
  geom_line(data=paras_full_t_t %>% filter(tipo=="taxa" & tipo_var!="(Intercept)"),
            aes(x=as.Date(data_ref),
                y=1-valor,col=tipo_var))+
  labs(col=NULL)+
  xlab("Data")+
  ylab("Subnotificação")+
  scale_color_manual(values = c("blue","red","green","black"),
                     breaks= names(supp.labs),
                     labels=supp.labs)+
  labs(shape="Significativo")+
  scale_y_continuous(labels = function(x)paste0(round(x*100,0),"%"))


ggsave("resultados/TAXA_NY.pdf",
       height = 8,
       width = 12)

tab1 <- paras_full_t %>%
  mutate(estimativap=ifelse(tipo_var=="(Intercept)",round(estimativa,1),
                            round(exp(estimativa),2))) %>%
  mutate(valor_tabela = ifelse(pvalor<0.01,paste0(estimativap,"***"),
                               ifelse(pvalor<0.05,paste0(estimativap,"***"),
                                      ifelse(pvalor<0.1,paste0(estimativap,"***"),
                                             paste0(estimativap))))) %>%
  mutate(valor_tabela = ifelse(moran<0.05,valor_tabela,paste0(valor_tabela,"+"))) %>%
  mutate(varby = paste0(tipo_var,tipo))%>%
  dplyr::select(data_ref,varby,valor_tabela)%>%
  spread(key="varby",value="valor_tabela") 

tab10 <- paras_full_t_t %>%
  filter(tipo=="taxa") %>%
  dplyr::select(valor,tipo_var,data_ref)%>%
  spread(key='tipo_var',value="valor")


tab1 <- tab1 %>%
  left_join(tab10,by="data_ref") %>%
  mutate(data_ref=as.Date(data_ref) %>% as.character())

print(xtable::xtable(tab1),include.rownames=F)
