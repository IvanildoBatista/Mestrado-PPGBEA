#IMPORTANDO OS PACOTES
install.packages(c('SPEI','readxl','sf','tidyverse','geobr','tmap','vtable','forecast','ggrepel',
                   'trend','heatwaveR','dplyr','plyr','Metrics','writexl','gridExtra','pals'))
library(pals)
library(gridExtra) 
library(SPEI)
library(readxl)
library(SPEI)
library(sf)
library(tidyverse)
library(geobr)
library(tmap)
library(vtable)
library(forecast)
library(trend)
library(heatwaveR)
library(dplyr) 
library(plyr)
library(Metrics)
library(writexl)
library(ggrepel)
#setwd("C:/Users/junio/OneDrive/Área de Trabalho/Material do segundo período/Projeto")

##### IMPORTANDO A FUNÇÃO DE CÁLCULO DO SPI###############################################
source('SPI_function.R')

###########################################################################################
##########################################################################################
##########################################################################################

dados <- read.delim2("join_prec_full_region.txt", sep="\t", header = TRUE)
dados <- dados[c('cod','year','month','prec')]
dados$prec <- as.numeric(dados$prec)
#dados%>% view()

#Convertendo a base de dados para calcular o SPI para cada estação####
data2 <- data.frame()
for (i in unique(dados$cod)){
  for (j in c(1,3,6,12)){
    spi <- SPI(as.data.frame(dados[dados$cod==i,][,-1]), 
               scale=j,distr="gamma") %>%tibble::as_tibble() %>% dplyr::select(3) %>% pull()
    data2<-dplyr::bind_rows(data2,bind_cols(cod=i,ano=dados[dados$cod==i,]$year,
                                            mes=dados[dados$cod==i,]$month,
                                            scale=j,spi=spi))}}
#########################################################################
#http://old.apac.pe.gov.br/meteorologia/estacoes-do-ano.php?estacao=verao
#Inserindo estações do ano
#PRIMAVERA: 22 de setembro até dia 21 de dezembro. (10-12)
#VERÃO: 21 de dezembro até 20 de março. (1-3)
#OUTONO: 20 de março a 20 de junho (4-6)
#INVERNO: 20 de junho até 22 de setembro. (7-9)

data2$season <-c()
data2$season[data2$mes==1] <- "VERAO"
data2$season[data2$mes==2] <- "VERAO"
data2$season[data2$mes==3] <- "VERAO"
data2$season[data2$mes==4] <- "OUTONO"
data2$season[data2$mes==5] <- "OUTONO"
data2$season[data2$mes==6] <- "OUTONO"
data2$season[data2$mes==7] <- "INVERNO"
data2$season[data2$mes==8] <- "INVERNO"
data2$season[data2$mes==9] <- "INVERNO"
data2$season[data2$mes==10] <- "PRIMAVERA"
data2$season[data2$mes==11] <- "PRIMAVERA"
data2$season[data2$mes==12] <- "PRIMAVERA"

data2 %>% view()
#########################################################
dados3 <- read.delim2("join_prec_full_region.txt", sep="\t", header = TRUE)
dados4 <- dados3 %>% filter(year==1962,month==1)%>%arrange(cod)%>% select(cod,lon,lat,region)

#organizando por código, ano e mês
#dados %>% arrange(cod,year,month) %>% view()
################################################################################################
##########################CALCULANDO A FREQUÊNCIA DE SECA######################################
################################################################################################
frequencia <- data2 %>% na.omit() %>% dplyr::group_by(cod,scale,ano)%>%
  dplyr::summarise(freq = sum(spi<(-0.5)))%>% # calcula para valores menores que 0 ou menor que -0.5
  mutate(freq1 = replace(freq, freq >0, 1))
#inserindo as estações
frequencia2<-left_join(frequencia,dados4,by=c('cod'='cod'))
#calculando os valores percentuais de seca por ano e por mês
freq <-frequencia %>% dplyr::group_by(cod,scale) %>% 
  dplyr::summarise(freq_ano = sum(freq1>0,na.rm=TRUE)/51, 
                   freq_mes = sum(freq,na.rm=TRUE)/612)
#inserindo as estações
freq2 <- left_join(freq,dados4,by=c('cod'='cod'))
freq2<-freq2 %>% select(cod,lon,lat,region,scale,freq_ano,freq_mes)

############################################################################################
###################FREQUÊNCIA SAZONAL#######################################################
############################################################################################

frequencia3 <- data2 %>%
  na.omit() %>%  dplyr::group_by(cod,scale,season)%>%
  dplyr::summarise(freq = sum(spi<(-0.5)))%>% # calcula para valores menores que 0 ou menor que -0.5
  mutate(freq1 = replace(freq, freq >0, 1))
frequencia4<-left_join(frequencia3,dados4,by=c('cod'='cod'))

freq3 <-frequencia3 %>% dplyr::group_by(cod,scale,season) %>% 
  dplyr::summarise(freq_ano = sum(freq1>0,na.rm=TRUE),freq_mes = sum(freq,na.rm=TRUE)/153)
#inserindo as estações
freq4 <- left_join(freq3,dados4,by=c('cod'='cod'))
freq4<-freq4 %>% select(cod,lon,lat,season,scale,freq_ano,freq_mes)

##############################################################################################
#########################CALCULANDO A INTENSIDADE#############################################
##############################################################################################
intensidade <- data2 %>%
  na.omit()%>% group_by(cod,scale,ano)%>% 
  dplyr::summarise(int=mean(spi[spi<0],na.rm=TRUE)) %>% abs()%>% 
  mutate(category=cut(int, breaks=c(0.5, 1, 1.5, 2, Inf),
                      labels=c("Seca leve","Seca moderada","Seca pesada","Seca extrema")))

intensidade2 <- left_join(intensidade,dados4,by=c('cod'='cod'))
intensidade2<-intensidade2 %>% select(cod,lon,lat,region,scale,int,category)

#####################################################################################################
########################################INTENSIDADE SAZONAL##########################################
#####################################################################################################

intensidade3 <- data2 %>%
  na.omit()%>% group_by(cod,scale,season)%>% 
  dplyr::summarise(int=abs(mean(spi[spi<0],na.rm=TRUE))) %>% 
  mutate(category=cut(int, breaks=c(0.5, 1, 1.5, 2, Inf),
                      labels=c("Seca leve","Seca moderada","Seca pesada","Seca extrema")))

intensidade4 <- left_join(intensidade3,dados4,by=c('cod'='cod'))
intensidade4<-intensidade4 %>% select(cod,lon,lat,region,scale,int,category)

######################################################################################################
######################################################################################################
######################################################################################################
p1<-freq2 %>% filter(scale==1) %>% ggplot()+geom_point(aes(x=lon,y=lat,color=freq_ano),size=3.5)+
  geom_sf(data=macroreg, fill='transparent',show.legend = TRUE)+
  scale_color_gradientn(colours = jet(10))

p2<-freq2 %>% filter(scale==3) %>% 
  ggplot()+geom_point(aes(x=lon,y=lat,color=freq_ano),size=3.5)+
  geom_sf(data=macroreg, fill='transparent',show.legend = TRUE)+
  scale_color_gradientn(colours = jet(10))

p3<-freq2 %>% filter(scale==6) %>% 
  ggplot()+geom_point(aes(x=lon,y=lat,color=freq_ano),size=3.5)+
  geom_sf(data=macroreg, fill='transparent',show.legend = TRUE)+
  scale_color_gradientn(colours = jet(10))

p4<-freq2 %>% filter(scale==12) %>% 
  ggplot()+geom_point(aes(x=lon,y=lat,color=freq_ano),size=3.5)+
  geom_sf(data=macroreg, fill='transparent',show.legend = TRUE)+
  scale_color_gradientn(colours = jet(10))

grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)
#facet_wrap(~scale)


freq2 %>% filter(cod==30)


#########################################################################################
#########################################################################################
data3 <- left_join(data2,dados4,by=c('cod'='cod'))
data3<-data3 %>% select(cod,lon,lat,region,ano,mes,scale,spi)
#write_xlsx(data3,"data_spi.xlsx")
#agrupando os dados por ano, código e escala
#contando os valores que são menores que -0.5
#filtrando pela escala 1
data4<-data3 %>% group_by(ano,cod,scale) %>% 
  dplyr::summarise(soma = sum(spi<=(-0.5),na.rm = TRUE)) %>% filter(scale==1)
#criando uma nova coluna que serão os perídos de começo, meio e fim
#cada período terá um intervalo de 17 anos, pois assim cada período tem o mesmo 
#número de anos
data5 <- data4 %>% mutate(periodo=cut(ano, breaks=c(1961,1979,1996,2012),
                              labels=c("começo","meio","fim")))
#agrpando os valores por código e período
#e calculando onde se agrupam a maioria dos valores menores que -0.5
data6 <- data5 %>% group_by(cod,scale,periodo) %>% 
  dplyr::summarise(soma_freq = sum(soma,na.rm=TRUE))
data7 <- left_join(data6,dados4,by=c('cod'='cod'))
data7 %>% filter(region=='SertÃ£o') %>% view()

#####################################################################################################
#######################ÁREA AFETADA PELA SECA########################################################
######################################################################################################
#PARA COLOCAR ANUAL, TROCA freq POR freq1
#area <- frequencia %>% group_by(ano,cod,scale) %>% dplyr::summarise(area=sum(freq,na.rm = TRUE))
#area%>%filter(scale==1)%>%view()

freqqq<- frequencia %>% group_by(ano,cod,scale) %>% dplyr::summarise(area=sum(freq,na.rm = TRUE))%>% 
  mutate(area1 = replace(area, area >0, 1))

freqqq2<-left_join(freqqq,dados4,by=c('cod'='cod'))
area2<- freqqq %>%group_by(ano,scale) %>% dplyr::summarise(area_total=sum(area1,na.rm = TRUE)/133)%>% 
  mutate(category=cut(area_total, breaks=c(0,0.1, 0.25, 0.33, 0.5, 1),
                      labels=c("Sem seca aparente","Seca local","Seca regional parcial",
                               "Seca regional","Seca global"))) 

freqqq<-left_join(freqqq,dados4,by=c('cod'='cod'))
area<- freqqq %>%group_by(ano,scale,region) %>% dplyr::summarise(area_total=sum(area1,na.rm = TRUE)/133)%>% 
  mutate(category=cut(area_total, breaks=c(0,0.1, 0.25, 0.33, 0.5, 1),
                      labels=c("Sem seca aparente","Seca local","Seca regional parcial",
                               "Seca regional","Seca global"))) 

area2%>%  ggplot()+geom_point(aes(y=area_total,x=ano),size=3.5)+ 
  geom_line(aes(y=area_total,x=ano),size=.5)+
  geom_hline(yintercept=0.1, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=0.25, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=0.33, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=0.5, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red",size=.8)+
  facet_wrap(~scale)

###################################################################################################
################################### ÁREA AFETADA SAZONAL##########################################

freqqq3<- frequencia3 %>% group_by(season,cod,scale) %>% 
  dplyr::summarise(area=sum(freq,na.rm = TRUE))%>% 
  mutate(area1 = replace(area, area >0, 1))

freqqq4<-left_join(freqqq3,dados4,by=c('cod'='cod'))
area3<- freqqq3 %>%group_by(season,scale) %>% dplyr::summarise(area_total=sum(area1,na.rm = TRUE)/133)%>% 
  mutate(category=cut(area_total, breaks=c(0,0.1, 0.25, 0.33, 0.5, 1),
                      labels=c("Sem seca aparente","Seca local","Seca regional parcial",
                               "Seca regional","Seca global"))) 

freqqq3<-left_join(freqqq3,dados4,by=c('cod'='cod'))
area4<- freqqq3 %>%group_by(season,scale,region) %>% dplyr::summarise(area_total=sum(area1,na.rm = TRUE)/133)%>% 
  mutate(category=cut(area_total, breaks=c(0,0.1, 0.25, 0.33, 0.5, 1),
                      labels=c("Sem seca aparente","Seca local","Seca regional parcial",
                               "Seca regional","Seca global"))) 

area4%>%  ggplot()+geom_bar(aes(y=area_total,x=season),size=3.5)+ 
  geom_line(aes(y=area_total,x=season),size=.5)+
  geom_hline(yintercept=0.1, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=0.25, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=0.33, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=0.5, linetype="dashed", color = "red",size=.8)+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red",size=.8)+
  facet_wrap(~scale)
#ou area e 1596,filter(scale==1)%>%

#PARA COLOCAR ANUAL TROCA 1596 por 133
#area2<-area%>% group_by(ano,scale) %>%dplyr::summarise(area_total=sum(area,na.rm = TRUE)/1596)%>% 
 # filter(scale==1)%>% mutate(category=cut(area_total, breaks=c(0,0.1, 0.25, 0.33, 0.5, 1),
  #                                        labels=c("Sem seca aparente","Seca local",
   #                                                "Seca regional parcial","Seca regional",
    #                                               "Seca global")))#

area %>% filter(cod==1,scale==1) %>% view()
ggplot(frequencia %>% filter(scale==1),aes(y=freq,x=ano)) + 
  geom_bar(position="dodge", stat='identity')

###################################################################################################
###################################################################################################

prec_acumulada2 %>% view()
frequencia2%>% view()
frequencia2$ano <- factor(frequencia2$ano,levels = c(1962:2012))

###Gráfico com a frequência mensal para as estações
p1<-ggplot(frequencia2%>% filter(cod==1,scale==1), aes(y=freq,x=ano, fill=region)) +
  geom_bar(position="dodge", stat='identity')+ geom_line(color='black')+  geom_point()+ 
  ylab('Frequência 1')
p2<-ggplot(frequencia2%>% filter(cod==2,scale==1), aes(y=freq,x=ano, fill=region)) +
  geom_bar(position="dodge", stat='identity') + geom_line(color='black')+  geom_point()+
  ylab('Frequência 2')
p3<-ggplot(frequencia2%>% filter(cod==4,scale==1), aes(y=freq,x=ano, fill=region)) +
  geom_bar(position="dodge", stat='identity')+ ylab('Frequência 4')
p4<-ggplot(frequencia2%>% filter(cod==5,scale==1), aes(y=freq,x=ano, fill=region)) +
  geom_bar(position="dodge", stat='identity')+ ylab('Frequência 5')
grid.arrange(p1, p2, p3,p4, ncol=2) 

#https://www.apac.pe.gov.br/agrometeorologia/196-agrometeorologia/544-fracao-de-cobertura-vegetal
#####################################################################################################
####################################################################################################

freq4$lon<- as.numeric(freq4$lon)
freq4$lat<- as.numeric(freq4$lat)
frequencia2$lon<- as.numeric(frequencia2$lon)
frequencia2$lat<- as.numeric(frequencia2$lat)
intensidade2$lon<- as.numeric(intensidade2$lon)
intensidade2$lat<- as.numeric(intensidade2$lat)
#area$lon<- as.numeric(area$lon)
#area$lat<- as.numeric(area$lat)
macroreg <- read_sf('macroreg.shp') #lendo o shapefile das macrorregiões de pernambuco

#MAPA COM AS FREQUÊNCIAS MENSAIS
freq4 %>% 
  #filter(scale==1) %>% 
  ggplot()+geom_point(aes(x=lon,y=lat,color=freq_mes),size=2.5)+
  geom_sf(data=macroreg, fill='transparent',show.legend = TRUE)+
  facet_wrap(~scale)+scale_color_gradientn(colours = jet(10))
#geom_text(aes(x=lon,y=lat,label=cod))

#MAPAS COM AS FREQUÊNCIAS ANUAIS
frequencia2 %>% 
  filter(scale==1,region=='SertÃ£o') %>% 
  ggplot()+geom_point(aes(x=lon,y=lat,color=freq),size=2.5)+
  geom_sf(data=sertao, fill='transparent',show.legend = TRUE)+
  facet_wrap(~ano)+ scale_color_gradientn(colours = jet(10))
#+  geom_text(aes(x=lon,y=lat,label=cod))

#MAPA DA INTENSIDADE

intensidade<-left_join(intensidade,dados4,by=c('cod'='cod'))
intensidade$lon<- as.numeric(intensidade$lon)
intensidade$lat<- as.numeric(intensidade$lat)
intensidade%>% filter(scale==12,region=='SertÃ£o') %>% ggplot()+geom_point(aes(x=lon,y=lat,color=int),size=2.5)+
  geom_sf(data=sertao, fill='transparent',show.legend = TRUE)+facet_wrap(~ano)+ scale_color_gradientn(colours = jet(10))
  #geom_text(aes(x=lon,y=lat,label=cod))

intensidade%>% filter(scale==12,region=='Agreste') %>% ggplot()+geom_point(aes(x=lon,y=lat,color=int),size=2.5)+
  geom_sf(data=agreste, fill='transparent',show.legend = TRUE)+facet_wrap(~ano)+ scale_color_gradientn(colours = jet(10))

intensidade%>% filter(scale==12,region=='Zona da Mata') %>% ggplot()+geom_point(aes(x=lon,y=lat,color=int),size=2.5)+
  geom_sf(data=zona, fill='transparent',show.legend = TRUE)+facet_wrap(~ano)+ scale_color_gradientn(colours = jet(10))

area3 %>% 
  filter(scale==6,region=='Zona da Mata') %>% #
  ggplot()+geom_point(aes(x=lon,y=lat,color=area),size=2.5)+
  geom_sf(data=zona, fill='transparent',show.legend = TRUE)+
  facet_wrap(~ano)+ scale_color_gradientn(colours = jet(10))
#unique(area3$region)"SertÃ£o"      "Agreste"      "Zona da Mata"

sertao<-macroreg[macroreg$MACRO=="SERTAO",]
zona<-macroreg[macroreg$MACRO=="MATA",]
agreste<-macroreg[macroreg$MACRO=="AGRESTE",]

frequencia3 <- data2 %>%
  na.omit() %>%
  dplyr::group_by(cod,scale,mes)%>%
  dplyr::summarise(freq = sum(spi<(-0.5)))%>% # calcula para valores menores que 0 ou menor que -0.5
  mutate(freq1 = replace(freq, freq >0, 1))


