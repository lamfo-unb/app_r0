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
                        is_last = ifelse(date==max(date)-1,T,F))
base.last <- base %>% filter(is_last==1)
#"2020-04-14"
pop <- read_excel("bases/estimativa_dou_2019.xls", sheet = "Municípios",skip = 1)


# Identificação estado e capital
#  cod.state <- 33
#  state.char <- "RJ"
#  cod.capital <- 3304557
 cod.state <- 35
 state.char <- "SP"
 cod.capital <- 3550308
# cod.state <- 26
#state.char <- "PE"
#cod.capital <- 	2611606
# cod.state <- 23
# state.char <- "CE"
# cod.capital <- 	2304400



covidmun <-  base %>%
  filter(state==state.char)%>% filter(ibgeID!=cod.state) %>% as.data.table()

covidmun[deaths>0, day1_death := as.numeric(min(date)),by=c("ibgeID")] 
covidmun[deaths>9, day_10death  := as.numeric(min(date))- day1_death,by=c("ibgeID")] 
covidmun[deaths>4, day_5death  := as.numeric(min(date))- day1_death,by=c("ibgeID")] 


covidmun[totalCases>0, day1_case := as.numeric(min(date)),by=c("ibgeID")] 
covidmun[totalCases>=100, day_100cases := as.numeric(min(date))-day1_case,by=c("ibgeID")] 
covidmun[totalCases>=10, day_10cases := as.numeric(min(date))-day1_case,by=c("ibgeID")] 
covidmun[totalCases>=25, day_25cases := as.numeric(min(date))-day1_case,by=c("ibgeID")] 
covidmun[totalCases>=50, day_50cases := as.numeric(min(date))-day1_case,by=c("ibgeID")] 
covidmun[totalCases>=75, day_75cases := as.numeric(min(date))-day1_case,by=c("ibgeID")] 

covidmun[is.na(covidmun),] = 0

covidmun <- covidmun %>% filter(is_last==1)


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
  left_join(covidmun %>% filter(date=="30-04-2020") %>%
              dplyr::rename(confirmed=totalCases),
            by = c("code_muni"='ibgeID'))


teste <- shapefile %>%
  anti_join(covidmun %>% filter(date=="30-04-2020") %>%
              dplyr::rename(confirmed=totalCases),
            by = c("code_muni"='ibgeID'))




# shapefilesp <- shapefilesp %>% 
#   left_join(pop %>% mutate(code_muni=paste0(`COD. UF`,`COD. MUNIC`) %>% as.numeric,
#                            npop2019 = `POPULAÇÃO ESTIMADA`) %>%dplyr::select(code_muni,npop2019),
#             by = "code_muni")

shapefilesp <- shapefilesp %>%
  left_join(pop %>% dplyr::mutate(code_muni=paste0(`COD. UF`,`COD. MUNIC`) %>% as.numeric,
                           npop2019 = `POPULAÇÃO ESTIMADA`) %>%dplyr::select(code_muni,npop2019),
            by = "code_muni") #%>% left_join(hospit %>% select(-NOME_MUN),by=c("code_muni2"="COD_MUN"))



shapefilesp <- shapefilesp %>%
  dplyr::mutate(confirmed=ifelse(is.na(confirmed),0,confirmed),
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

covidmun<- covidmun %>% right_join(data.frame(baseunica),
                                  by = c("ibgeID"="code_muni")) 

write.csv2(covidmun,"SP_10_04.csv",row.names = F,na = "0")

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
          aes(fill=(deaths/npop2019)*10^5),size=.05,
          color = ifelse(shapefilesp$code_muni %in% munbrsp$code_muni,
                         "orange","gray85"))+
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
