install.packages('readxl')
install.packages('tidyverse')
install.packages('geobr')
install.packages('sf')
install.packages('GISTools')
install.packages('raster')
install.packages('cowplot')
install.packages('rcartocolor')
install.packages('mapproj')
install.packages('ggpubr')
install.packages('gridExtra')
install.packages('grid')
library(readxl)
library(tidyverse)
library(geobr)
library(sf)
library(GISTools)
library(raster)
library(cowplot)
library(rcartocolor)
library(mapproj)
library(gridExtra)
library(grid)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
#library(ggpubr)

pernambuco <- read_state('PE')#Estado de Pernambuco
pe <- geobr::read_municipality("PE", year = 2020) %>% 
  filter(name_muni!="Fernando De Noronha") #Estado de Pernambuco sem Fernando de Noronha

br <- geobr::read_country(year = 2020) #Brasil
micro <- read_sf('BR_Microrregioes_2019.shp') #micro regiões do Brasil
macroreg <- read_sf('macroreg.shp') #Macro regiões
regioes <- read_sf('regioes_2010.shp') #Regiões
reg <- read_region(year=2020)#Regiões
a <- read_semiarid(year=2017) #região do semiárido
nordeste <- regioes[regioes$nome =='Nordeste',][1] #nordeste

nordeste <- br[br$code_region==2,][1]

plot(read_sf('PE_2600609_RIOS_SIMPLES.shp'))
estacoes = read_xlsx('Estacoes meteorologicas.xlsx')

estacoes$Latitude <- as.numeric(estacoes$Latitude)
estacoes$Longitude <- as.numeric(estacoes$Longitude)

dt <- data.frame(lon=c(estacoes$Longitude),
                 lat=c(estacoes$Latitude))

#Brasil (estados)+ Pernambuco destacado
ggplot(dt) +
  geom_point(aes(x=lon,y=lat),size=1,color="red") + geom_sf(data=br) +
  geom_sf(data=pe,fill="transparent") +
  theme_bw()

ggplot(dt) +
  geom_point(aes(x=lon,y=lat),size=1,color="red") + geom_sf(data=br) +
  geom_sf(data=pernambuco,fill="red") +
  theme_bw()


#+
  #borders("world", xlim = c(-130, -60), ylim = c(20, 50))

#theme_update(plot.title = element_text(hjust = 0.5))
pe_cortado <- ggplot(dt) +
  geom_point(aes(x=lon,y=lat),size=1,color="black") + geom_sf(data=br, fill='white')+ 
  geom_sf(data=a, fill='gray', color='transparent', alpha=0.8) +
  geom_sf(data=macroreg, fill='red', alpha = 0.6, size = 0.5) +
  #borders(xlim = c(-130, -60), ylim = c(20, 50))+
  labs(x = "", y = "", color = "Legend") + xlim(-42,-34.25)+ylim(9.5,7)+
  #ggtitle(label = "Regiões do Brasil",
          #subtitle = "Em destaque Pernambuco e o Semiárido nordestino")+
  theme_test()

mapa_completo <- ggplot(dt) +
  geom_point(aes(x=lon,y=lat),size=1,color="red") + geom_sf(data=regioes, fill='white')+ 
  #geom_sf(data=a, fill='gray', color='transparent', alpha=0.8) +
  geom_sf(data=macroreg, fill='red', alpha = 0.6, size = 0.5) +
  #borders(xlim = c(-130, -60), ylim = c(20, 50))+
  labs(x = "", y = "", color = "Legend") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
  #ggtitle(label = "Regiões do Brasil",
  #subtitle = "Em destaque Pernambuco e o Semiárido nordestino")+
  theme_bw()

nordeste2 <- ggplot(dt) + 
  geom_point(aes(x=lon,y=lat),size=1,color="red") + 
  geom_sf(data=nordeste) + geom_sf(data=macroreg, fill = 'red') +
  labs(x = "", y = "", color = "Legend") + theme_bw()

pernambuco_completo <- ggplot(dt) + geom_sf(data=macroreg, fill='green') + theme_bw()


tres_mapas = ggdraw() +
  draw_plot(pe_cortado) +
  draw_plot(mapa_completo,  x = -0.01, y = 0.57, width = 0.25, height = 0.25)+
  draw_plot(nordeste2,  x = -0.01, y = 0.25, width = 0.25, height = 0.25)
tres_mapas

g3 <- pernambuco_completo +
  annotation_custom(grob = ggplotGrob(mapa_completo)) 

help("annotation_custom")

pe_cortado +
  annotation_custom(grob = ggplotGrob(mapa_completo)) 


bacias <- read_sf('GEOFT_BHO_CURSO_DAGUA.shp')
plot(bacias)


pe_cortado2 <- ggplot(dt) +
  geom_point(aes(x=lon,y=lat),size=1,color="red") + geom_sf(data=br, fill='white')+ 
  geom_sf(data=bacias, color='blue', alpha = 1.3, size=1.5) +
  geom_sf(data=a, fill='gray', color='transparent', alpha=0.8) +
  geom_sf(data=macroreg, fill=NA, alpha = 0.6, size = 0.5) +
  #borders(xlim = c(-130, -60), ylim = c(20, 50))+
  labs(x = "Longitude", y = "Latitude", color = "Legend") + xlim(-42,-34.5)+ylim(9.5,7)+
  #ggtitle(label = "Regiões do Brasil",
  #subtitle = "Em destaque Pernambuco e o Semiárido nordestino")+
  theme_bw()


tres_mapas = ggdraw() +
  draw_plot(pe_cortado) +
  draw_plot(mapa_completo,  x = -0.01, y = 0.57, width = 0.25, height = 0.25)+
  draw_plot(nordeste2,  x = -0.01, y = 0.25, width = 0.25, height = 0.25)
tres_mapas

grid.arrange(pe_cortado, mapa_completo, nordeste2, layout_matrix = rbind(c(1,1),c(2,3)))

