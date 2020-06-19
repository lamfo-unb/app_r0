rm(list = ls())
# SOURCES ----
# https://brasil.io/dataset/covid19/caso
# https://covid.saude.gov.br/
#http://transportes.gov.br/bit/63-bit/5124-bitmodosmapas.html

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
require(vcd)
library(hnp)
library(RCurl)
#install.packages("countreg", repos="http://R-Forge.R-project.org")


# library(devtools)
# install_github("r-spatial/sf")
# library(sf)

# READ FILES ----
base <- read.csv(text = getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"))
base <- base %>% mutate(state = as.character(state),date = as.Date(date),
                        state=ifelse(state == "TOTAL", "Brasil", state),
                        is_last = ifelse(date==max(date),T,F))
base.last <- base %>% filter(is_last==1)

pop <- read_excel("bases/estimativa_dou_2019.xls", sheet = "Municípios",skip = 1)


# Identificação estado e capital
cod.state <- 33
state.char <- "RJ"
cod.capital <- 3304557
 cod.state <- 35
 state.char <- "SP"
 cod.capital <- 3550308
#cod.state <- 26
#state.char <- "PE"
#cod.capital <- 	2611606

cod.state <- 23
state.char <- "CE"
cod.capital <- 	2304400



covidmun <-  base %>%
  filter(state==state.char)

shapefile <- geobr::read_municipality(code_muni = cod.state)
#save(shapefile,file="bases\\shapefile.rda")
shapefileuf <- geobr::read_state(code_state = cod.state)
#save(shapefileuf,file="bases\\shapefileuf.rda")




#hospit <- read.csv2("bases/Hospitais_sp.csv")

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())



shapefilesp <- shapefile %>%
  left_join(covidmun %>% filter(is_last==T) %>%
              dplyr::select(newCases,confirmed=totalCases,newDeaths,deaths,ibgeID),
            by = c("code_muni"='ibgeID'))




# shapefilesp <- shapefilesp %>% 
#   left_join(pop %>% mutate(code_muni=paste0(`COD. UF`,`COD. MUNIC`) %>% as.numeric,
#                            npop2019 = `POPULAÇÃO ESTIMADA`) %>%dplyr::select(code_muni,npop2019),
#             by = "code_muni")

shapefilesp <- shapefilesp %>%
  left_join(pop %>% mutate(code_muni=paste0(`COD. UF`,`COD. MUNIC`) %>% as.numeric,
                           npop2019 = `POPULAÇÃO ESTIMADA`) %>%dplyr::select(code_muni,npop2019),
            by = "code_muni") #%>% left_join(hospit %>% select(-NOME_MUN),by=c("code_muni2"="COD_MUN"))


  
shapefilesp <- shapefilesp %>%
  mutate(confirmed=ifelse(is.na(confirmed),0,confirmed),
         deaths=ifelse(is.na(deaths),0,deaths),
         npop2019 = as.numeric(npop2019))




shapefilesp$area <- as.numeric(st_area(shapefilesp)/(1000^2))

shapefilesp <- bind_cols(shapefilesp,
                         data.frame(st_coordinates(st_centroid(shapefilesp))))
# ShapeRodovias ----
shprodovias <- read_sf("bases\\Rodovias\\rodovias.shp")

st_crs(shprodovias) <- st_crs(shapefilesp)


shprodoviassp <- st_intersection(shapefilesp,
                                 shprodovias)

head(shprodovias)
# lista de rodovias que acessam SP
lista_codigos_sp <- unique(gsub("(\\d{3})\\w{3}.*(\\d{3})","\\1",shprodoviassp[shprodoviassp$code_muni %in% cod.capital,]$vl_codigo))

# shape das rodovias que acessam SP
shprodoviasspsp <-  shprodoviassp %>%
  filter(gsub("(\\d{3})\\w{3}.*(\\d{3})","\\1",vl_codigo) %in% lista_codigos_sp)

# municipios que estão em rodovias que accessam
munbrsp<- shapefilesp %>% 
  filter(code_muni %in% unique(shprodoviasspsp$code_muni))

# Mapa 1: BR e municípios diratetamente ligados À Capital
mapa1 <- ggplot()+
  geom_sf(data=shapefileuf)+
  geom_sf(data=munbrsp,col="orange")+
  geom_sf(data=shprodoviasspsp,col="red")







# lista de rodovias que acessam rodovias que acessam SP
lista_codigos_sp_2 <- shprodoviassp %>% filter(shprodoviassp$code_muni %in% munbrsp$code_muni) 
lista_codigos_sp_2 <- unique(lista_codigos_sp_2$vl_br)
lista_codigos_sp_2 <- setdiff(lista_codigos_sp_2,lista_codigos_sp)

# shape de rodovias que acessam rodovias que acessam SP

shprodoviasspsp_2 <- shprodoviassp %>% 
  filter(vl_br %in% lista_codigos_sp_2)

dim(munbrsp)
listamunauxi <- unique(shprodoviasspsp_2$code_muni)
listamunauxi <- setdiff(listamunauxi,unique(munbrsp$code_muni))

# municipios que estão em rodovias que accessam rodovias que acessam SP

munbrsp_2<- shapefilesp %>% 
  filter(code_muni %in% listamunauxi)

# Mapa 2: BR e municípios indiretamente ligados À SP
# mapa2 <- mapa1 +
#   geom_sf(data=munbrsp_2,
#           col="skyblue1")+
#   geom_sf(data=shprodoviasspsp_2,
#           col="darkblue")

vec_per <- (shapefilesp$confirmed/shapefilesp$npop2019)*10^5
vec_per<-vec_per[vec_per>0]
shapefilesp$levelper <- cut((shapefilesp$confirmed/shapefilesp$npop2019)*10^5,breaks = c(-1e10,quantile(vec_per)))


# Mapa 3: todos os municípios indiretamente ligados À SP

 # mapa3 <- ggplot()+
 #   geom_sf(data=shapefileuf)+
 #   geom_sf(data=shapefilesp,col="gray75",aes(fill=levelper))+
 #   geom_sf(data=munbrsp,col="orange")+
 #   geom_sf(data=munbrsp_2,col="blue")+
 #   geom_sf(data=shapefilesp,col="gray75",aes(fill=levelper))+
 #   geom_sf(data=shprodoviassp,col="gray50",size=1.1)+
 #   geom_sf(data=shprodoviasspsp,col="red",size=1.2)+
 #   geom_sf(data=shprodoviasspsp_2,col="darkblue",size=1.2)

vec_per <- (shapefilesp$deaths/shapefilesp$npop2019)*10^5
vec_per<-vec_per[vec_per>0]
shapefilesp$levelpermorte <- cut((shapefilesp$deaths/shapefilesp$npop2019)*10^5,breaks = c(-1e10,quantile(vec_per)))


# Mapa 3: todos os municípios indiretamente ligados À SP

# mapa4 <- ggplot()+
#   geom_sf(data=shapefileuf)+
#   geom_sf(data=shapefilesp,col="gray75",aes(fill=levelpermorte))+
#   geom_sf(data=munbrsp,col="orange")+
#   geom_sf(data=munbrsp_2,col="blue")+
#   geom_sf(data=shapefilesp,col="gray75",aes(fill=levelpermorte))+
#   geom_sf(data=shprodoviassp,col="gray50",size=1.1)+
#   geom_sf(data=shprodoviasspsp,col="red",size=1.2)+
#   geom_sf(data=shprodoviasspsp_2,col="darkblue",size=1.2)

baseunica <- shapefilesp %>%  data.frame() %>% dplyr::select(code_muni) %>% unique() %>%
  left_join(shprodoviassp %>%  mutate(tem_rodo=1) %>% data.frame() %>% dplyr::select(code_muni,tem_rodo) %>% unique(),
            by = "code_muni") %>%
  left_join(munbrsp %>%  mutate(tem_rodo_2=1)  %>% data.frame()%>% 
              dplyr::select(code_muni,tem_rodo_2)  %>% unique(),
            by = "code_muni") %>%
  left_join(munbrsp_2 %>% mutate(tem_rodo_1=1)%>% data.frame()  %>% 
              dplyr::select(code_muni,tem_rodo_1)  %>% unique(),
            by = "code_muni") 

baseunica %>% 
  group_by(tem_rodo,tem_rodo,tem_rodo_1,tem_rodo_2) %>%
  summarise(N=length(code_muni))




baseunica <- baseunica %>%
  gather(key="var",value="valor",-code_muni) %>%
  mutate(valor=ifelse(is.na(valor),0,valor)) %>%
  spread(key="var",value ="valor")%>%
  mutate(tem_rodo_1=ifelse(tem_rodo_2==1,0,tem_rodo_1))%>%
  mutate(tem_rodo=ifelse((tem_rodo_2+tem_rodo_1)>0,0,tem_rodo))



baseunica %>% 
  group_by(tem_rodo,tem_rodo,tem_rodo_1,tem_rodo_2) %>%
  summarise(N=length(code_muni))



shapefilesp <- shapefilesp %>%
  left_join(data.frame(baseunica),
            by = "code_muni")



mapa5<-ggplot() +
  geom_sf(data=shapefileuf, color= "gray85", size=.15)+
  geom_sf(data=shapefilesp,color = "gray85",
          aes(fill=(confirmed/npop2019)*10^5),size=.05) +
  scale_fill_distiller(palette = "Blues",
                       name="Confirmed per 100K",
                       direction = 1)+
  geom_sf(data=shprodoviassp ,col="red")+
  geom_text(data = shapefilesp%>% filter(tem_rodo_2==1),
            aes(X, Y, label = confirmed) ,size = 1.1) +
  labs(subtitle="Confirmed cases at 2th june", size=8)+
  theme_minimal() 

mapa5
ggsave(paste0("resultados/",state.char,"_Casos.pdf"),height = 8,width = 12)


mapa5.1 <- ggplot()+
  geom_sf(data=shapefilesp,
          aes(fill=(deaths/npop2019)*10^5),size=.05)+
  scale_fill_distiller(palette = "Blues",
                       name="Deaths per 100K",
                       direction = 1)+
  geom_sf(data=shprodoviassp,col="red")+
  geom_text(data = shapefilesp%>% filter(tem_rodo_2==1),
            aes(X, Y, label = deaths) ,size = 1.1) +
  theme_minimal() 

mapa5.1
ggsave(paste0("resultados/",state.char,"_mortes.pdf"),height = 8,width = 12)


shapefilesp$xsp<- shapefilesp[shapefilesp$code_muni==cod.capital,]$X
shapefilesp$ysp<- shapefilesp[shapefilesp$code_muni==cod.capital,]$Y

shapefilesp <- shapefilesp %>%
  mutate(dx = X- xsp,
         dy = Y- ysp)



# TABELA DESCRITIVA
tab0 <- shapefilesp %>% data.frame() %>%
  group_by(tem_rodo_2) %>%
  summarise(N = length(npop2019),
            SN = sum(npop2019),
            confirmados = sum(confirmed,na.rm = T),
            deaths = sum(deaths,na.rm = T))

#print(xtable::xtable(tab0,digits=0), include.rownames=F)

#print(xtable::xtable(matrix(paste0(as.matrix(tab0),"(",round(prop.table(as.matrix(tab0),margin=2)*100,1),"%)"),nrow=2),digits=0), include.rownames=F)


apply(as.matrix(tab0),2,sum)


data_ref <- min(covidmun$date %>% as.Date)

# removendo SP
nc_sp <- as(shapefilesp  %>%
              filter(code_muni!=cod.capital), 'Spatial')
neighbors <- poly2nb(nc_sp, queen=TRUE)
neighbors_sf <- as(nb2lines(neighbors, coords = coordinates(nc_sp)), 'sf')
neighbors_sf <- st_set_crs(neighbors_sf, st_crs(shapefilesp))
neighbors<-nb2listw(neighbors,
                    style="W",
                    zero.policy=TRUE)
print(neighbors, zero.policy=TRUE)

mapa6 <- ggplot(shapefilesp) + 
  geom_sf(fill = 'salmon', color = 'white') +
  geom_sf(data = neighbors_sf %>% filter(i %in% which(shapefilesp$tem_rodo==1))) +
  theme_minimal() +
  ylab("Latitude") +
  xlab("Longitude")

# Loop 1 modelo ----


dat_it <-as.Date((unique(covidmun$date)))[1]
paras_full <- NULL
AICs_TOTAL_full <- NULL
forcasts_temp_full <- NULL
dat_it_vec <- as.Date((unique(covidmun$date)))
dat_it_vec <- dat_it_vec[month(dat_it_vec)>=4]
#dat_it_vec <- dat_it_vec[-2]
dat_it<-dat_it_vec[1]  
for(dat_it in dat_it_vec){
  base_final <- covidmun %>% #filter(place_type!="state")%>% 
    arrange(ibgeID,date) %>% as.data.table()
  
  base_final[,maxconfirmed:=max(totalCases),by="ibgeID"]
  
  codspcity <- base_final$ibgeID[which.max(base_final$totalCases)]
  
  
  base_final <- shapefilesp %>%  
    mutate(densidade=(npop2019/area),
           dist= (dx^2 + dy^2)^(1/2)) %>%
    dplyr::select(X,Y,code_muni,tem_rodo,tem_rodo_1,tem_rodo_2,dist,densidade,npop2019,area)  %>%
    left_join(base_final %>%
                filter(date==dat_it) %>%
                dplyr::select(ibgeID,deaths,totalCases,maxconfirmed),
              by = c("code_muni"="ibgeID")) %>%
    mutate(capital = ifelse(code_muni==codspcity,1,0),
           totalCases = ifelse(is.na(totalCases),0,totalCases),
           deaths = ifelse(is.na(deaths),0,deaths)) %>%
    filter(code_muni!=cod.capital)
  
  
  
  base_final %>% data.frame() %>%
    group_by(tem_rodo,tem_rodo_1,tem_rodo_2) %>%
    summarise(N=length(code_muni))
  
  
  # MODELOS MORTE
  
  modeloglmmorte <- glm(data= base_final, deaths ~ dist + tem_rodo_2 ,
                        family = poisson(link = "log"),
                        offset = log(npop2019));summary(modeloglmmorte)
  #countreg::rootogram(modeloglmmorte)
  #distplot(base_final$deaths, type="poisson")
  #hnp(modeloglmmorte)
  
  modeloglmmortenb <-glm.nb(data= base_final, 
                            deaths ~ dist + tem_rodo_2 + offset(log(npop2019)))
  #countreg::rootogram( modeloglmmortenb)
  #distplot(base_final$deaths, type="nbinomial")
  #deviance(modeloglmmortenb)
  #hnp(modeloglmmortenb)
      
      
  modeloglmmortezi<- zeroinfl(data= base_final, 
                              deaths ~ dist + tem_rodo_2,
                              dist = "pois",
                              offset = log(npop2019))
  #countreg::rootogram(modeloglmmortezi)
  
  modeloglmmortenbzi<-  zeroinfl(data= base_final, 
                                 deaths ~ dist + tem_rodo_2 ,
                                 dist = "negbin",
                                 offset = log(npop2019))
  
  ###MODELO CASOS
  
  
  modeloglmconfirmado <- glm(data= base_final, totalCases ~ dist + tem_rodo_2 ,
                        family = poisson(link = "log"),
                        offset = log(npop2019));summary(modeloglmmorte)
  
  
  modeloglmconfirmadonb <-glm.nb(data= base_final, 
                                totalCases ~ dist + tem_rodo_2+  offset(log(npop2019)))
  
  #countreg::rootogram(modeloglmconfirmadonb)
  modeloglmconfirmadonbzi<- zeroinfl(data= base_final, 
                                     totalCases ~ dist + tem_rodo_2 ,
                                     dist = "negbin",
                                     offset = log(npop2019))
  
  
  modeloglmconfirmadozi<- zeroinfl(data= base_final, 
                                   totalCases ~ dist + tem_rodo_2,
                                   dist = "pois",
                                   offset = log(npop2019))
  
  
  # forecast----
  
  a <- modeloglmconfirmadonb
  v1 <- predict(a,base_final,type="response")
  a$coefficients[2:3] <- modeloglmmortenb$coefficients[2:3]
  v2 <- predict(a,base_final,type="response")
  
  forcasts_temp <- data.frame(previsao_modelo = v1,
                              previsao_ajustada = v2,
                              cases=base_final$totalCases,
                              data_ref= dat_it,
                              codi_mun = base_final$code_muni)

  
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
  
  moranmortos <- moran.mc(modeloglmmortenb$residuals,print(neighbors, zero.policy=TRUE)
,999)
  
  
  
  AICs_TOTAL_full <- bind_rows(AICs_TOTAL_full,AICs_TOTAL)
  
  forcasts_temp_full <- bind_rows(forcasts_temp_full,forcasts_temp)
  
  print(paste0(dat_it))
}


saveRDS(forcasts_temp_full,"resultados/previsao.rds")

saveRDS(AICs_TOTAL_full,"resultados/modelos.rds")


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


#### avaliação ajuste
# Loop modelo ----


dat_it <-as.Date((unique(covidmun$date)))[1]
paras_full <- NULL
dat_it_vec <- as.Date((unique(covidmun$date)))
dat_it_vec <- dat_it_vec[month(dat_it_vec)>=4]
#dat_it_vec <- dat_it_vec [-2]
dat_it<-dat_it_vec[1]  
for(dat_it in dat_it_vec){
  base_final <- covidmun %>% #filter(place_type!="state")%>% 
    arrange(ibgeID,date) %>% as.data.table()
  
  base_final[,maxconfirmed:=max(totalCases),by="ibgeID"]
  
  codspcity <- base_final$ibgeID[which.max(base_final$totalCases)]
  
  
  base_final <- shapefilesp %>%  
    mutate(densidade=(npop2019/area),
           dist= (dx^2 + dy^2)^(1/2)) %>%
    dplyr::select(X,Y,code_muni,tem_rodo,tem_rodo_1,tem_rodo_2,dist,densidade,npop2019,area)  %>%
    left_join(base_final %>%
                filter(date==dat_it) %>%
                dplyr::select(ibgeID,deaths,totalCases,maxconfirmed),
              by = c("code_muni"="ibgeID")) %>%
    mutate(capital = ifelse(code_muni==codspcity,1,0),
           totalCases = ifelse(is.na(totalCases),0,totalCases),
           deaths = ifelse(is.na(deaths),0,deaths)) %>%
    filter(code_muni!=cod.capital)
  
  
  
  base_final %>% data.frame() %>%
    group_by(tem_rodo,tem_rodo_1,tem_rodo_2) %>%
    summarise(N=length(code_muni))
  
  modeloglmmortenb <-glm.nb(data= base_final, 
                            deaths ~ dist + tem_rodo_2+offset(log(npop2019)))
  countreg::rootogram( modeloglmmortenb,main=paste0("Mortes-",as.Date(dat_it)))
  distplot(base_final$deaths, type="nbinomial",main=paste0("Mortes-",as.Date(dat_it)))
  deviance(modeloglmmortenb)
  #hnp(modeloglmmortenb,main=paste0("Mortes-",as.Date(dat_it)))
  #moranmortes <- moran.mc(modeloglmmortenb$residuals,neighbors,999)
  
  modeloglmconfirmadonb <-glm.nb(data= base_final, 
                                 totalCases ~ dist+ tem_rodo_2 +  offset(log(npop2019)))
  
  countreg::rootogram(modeloglmconfirmadonb,main=paste0("Casos-",as.Date(dat_it)))
  distplot(base_final$totalCases, type="nbinomial",main=paste0("Casos-",as.Date(dat_it)))
  deviance(modeloglmconfirmadonb)
  #hnp(modeloglmconfirmadonb,main=paste0("Casos-",as.Date(dat_it)))
  #morancasos <- moran.mc(modeloglmconfirmadonb$residuals,neighbors,999)
  
  
  a<-summary(modeloglmconfirmadonb)
  paras <- a$coefficients %>% data.frame()
  paras$tipo_var <- rownames(paras)
  paras$aic <- a$aic
  paras$theta <- a$theta
  paras$deviance <- a$deviance
  paras$nulldeviance <- a$null.deviance
  paras$twologlik <- a$twologlik
  
  paras <- bind_cols(paras,
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
  
  paras <- bind_cols(paras,
                     confint(modeloglmmortenb,level = c(.95))%>% data.frame(),
                     confint(modeloglmmortenb,level = c(.90))%>% data.frame()) 
  
  paras$tipo <-  'morte'
  parastemp <- bind_rows(parastemp,paras)
  parastemp$data_ref <- dat_it
 #parastemp$moranmortos <- moranmortos$p.value
  paras_full <- bind_rows(paras_full,parastemp)
  
}

# Analise output parÂmetros ----
paras_full_t <-paras_full %>% unique()


names(paras_full_t) <- c("estimativa","sd","z","pvalor",
                         "tipo_var","aic","theta","deviance","nulldeviance","twologlik",
                         "q025","q975","q050","q950","tipo","data_ref")#"moran")

saveRDS(paras_full_t,paste0("resultados/estimativas",state.char,".rds"))


supp.labs <- c("Distância","Rodovia Primária","Rodovia secundária","Rodovia")
names(supp.labs) <-c("dist","tem_rodo_2","tem_rodo_1","tem_rodo")

unique(paras_full_t$tipo_var)

names(paras_full_t)


paras_full_t <- paras_full_t %>% filter(data_ref<as.Date("2020-06-02"))
ggplot()+
  # geom_ribbon(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
  #             aes(x=as.Date(data_ref),
  #                 fill=tipo,
  #                 col= tipo,
  #                 ymin=exp(q025),
  #                 ymax=exp(q050)),alpha=0.1)+
  # geom_ribbon(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
  #             aes(x=as.Date(data_ref),
  #                 fill=tipo,
  #                 col= tipo,
  #                 ymin=exp(q950),
  #                 ymax=exp(q975)),alpha=0.1)+
  # geom_ribbon(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
  #             aes(x=as.Date(data_ref),
  #                 fill=tipo,
  #                 col= tipo,
  #                 ymin=exp(q050),
  #                 ymax=exp(q950)),alpha=0.12)+
  # geom_ribbon(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
  #             aes(x=as.Date(data_ref),
  #                 fill=tipo,
  #                 col= tipo,
  #                 ymin=exp(q050),
  #                 ymax=exp(q950)),alpha=0.12)+
  geom_ribbon(data=paras_full_t %>% filter(tipo_var!="(Intercept)") ,
              aes(x=as.Date(data_ref),
                  fill=tipo,
                  ymin=exp(q025),
                  ymax=exp(q975)),
              alpha=0.12)+
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




ggsave(paste0("resultados/BETAS_",state.char,".pdf"),height = 8,width = 12)


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


ggsave(paste0("resultados/TAXA_",state.char,".pdf"),
       height = 8,
       width = 12)


tab1 <- paras_full_t %>%
  mutate(estimativap=ifelse(tipo_var=="(Intercept)",round(estimativa,1),
                            round(exp(estimativa),2))) %>%
  mutate(valor_tabela = ifelse(pvalor<0.01,paste0(estimativap,"***"),
                               ifelse(pvalor<0.05,paste0(estimativap,"**"),
                                      ifelse(pvalor<0.1,paste0(estimativap,"*"),
                                             paste0(estimativap))))) %>%
  #mutate(valor_tabela = ifelse(moran<0.05,valor_tabela,paste0(valor_tabela,"+"))) %>%
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

saveRDS(tab1,paste0("resultados/coeficientes_",state.char,".rds"))



# Analise output forecast ----

dat_it_vec <- as.Date((unique(covidmun$date)))
dat_it_vec <- dat_it_vec[month(dat_it_vec)==4]
dat_it <- dat_it_vec[1]


  base_final <- covidmun %>% filter(place_type!="state")%>% 
    arrange(city_ibge_code,date) %>% as.data.table()
  
  codspcity <- base_final$city_ibge_code[which.max(base_final$confirmed)]
  
  
  base_final <- shapefilesp %>%  
    mutate(densidade=(npop2019/area),
           dist= (dx^2 + dy^2)^(1/2)) %>%
    dplyr::select(X,Y,code_muni,tem_rodo_2,dist,densidade,npop2019,area)  %>%
    left_join(base_final %>%
                dplyr::select(city_ibge_code,deaths,confirmed,date),
              by = c("code_muni"="city_ibge_code")) %>%
    mutate(capital = ifelse(code_muni==codspcity,1,0),
           confirmed = ifelse(is.na(confirmed),0,confirmed),
           deaths = ifelse(is.na(deaths),0,deaths)) %>%
    filter(code_muni!=cod.capital)
  
  nrow(forcasts_temp_full)/643
  
  
  names(forcasts_temp_full)

a <- forcasts_temp_full %>%
    mutate(data_ref=as.Date(data_ref)) %>%
    left_join(base_final %>%
                mutate(date=as.Date(date)),
              by=c("codi_mun"="code_muni",
                   "data_ref"="date"))


a <- a %>%
  gather(key="tipovarfor",value="valor",previsao_modelo,previsao_ajustada,confirmed) %>%
  group_by(tipovarfor,data_ref) %>%
  summarise(N = sum(valor,na.rm = T))


a %>%
  spread(key="tipovarfor",value="N")

ggplot(a)+
    geom_line(aes(x=data_ref,
                  y=N,
                  col=tipovarfor))




# Indentificando polos ----

a <- read_excel("bases/MUNICI.xlsx",sheet = "MUNICI_14") %>% as.data.table()
a[,dt_polo:=min(date,na.rm = T),by = c("state","place_type")] 
a[,max_confirmed:=max(confirmed,na.rm = T),by = c("state","place_type")]
a[,max_pop:=max(confirmed_per_100k_inhabitants,na.rm = T),by = c("state","place_type")]

a <- a %>% filter( place_type=="city" & (dt_polo==date |
                                           max_confirmed==confirmed | confirmed_per_100k_inhabitants == max_pop)) %>%
  mutate(flag_data = ifelse(dt_polo==date,1,0),
         flag_confirmed = ifelse(max_confirmed==confirmed,1,0),
         flag_pop = ifelse(confirmed_per_100k_inhabitants == max_pop,1,0)) %>%
  select(flag_data,flag_confirmed,flag_pop,city_ibge_code,state) %>% unique() %>%
  group_by(city_ibge_code,state) %>%
  summarise(flag_data=max(flag_data),
            flag_confirmed=max(flag_confirmed),
            flag_pop=max(flag_pop))
