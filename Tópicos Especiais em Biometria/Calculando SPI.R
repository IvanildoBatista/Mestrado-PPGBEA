install.packages("SPEI")
install.packages('readxl')
install.packages('sf')
install.packages('tidyverse')
install.packages('geobr')
install.packages('tmap')
install.packages('vtable')
install.packages('forecast')
install.packages('trend')
install.packages('heatwaveR')
install.packages('dplyr')
install.packages('plyr')
install.packages('Metrics')
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
#### new function
#### run_theory() 
#### applied the run theory to a time series ()
## time_serie: a numeric vector with no NA values
## threshold: a numeric value in which the features (below) of run theory is measured

####################################################################################
####################################################################################
###########IMPORTANDO A BASE DE DADOS ##############################################
####################################################################################
####################################################################################

chuva = read_xlsx('precipitacao.xlsx')
head(chuva$prec)

meus_dados <- read.delim2("station_1_GAMMA.txt", sep="\t", header = FALSE)

meus_dados <- meus_dados %>%
  filter(!row_number() %in% c(1, 2))
colnames(meus_dados) <- c("ano","mes","spi1","spi3","spi6","spi12")
View(meus_dados)

#CALCULANDO O VALOR DO SPI
View(spi1)
spi1 <- spi(na.omit(chuva$prec), 1) 
spi3 <- spi(na.omit(chuva$prec), 3)
spi6 <- spi(na.omit(chuva$prec), 6)
spi9 <- spi(na.omit(chuva$prec), 9)
spi12 <- spi(na.omit(chuva$prec), 12)
spi24 <- spi(na.omit(chuva$prec), 24)
spi48 <- spi(na.omit(chuva$prec), 48)

meus_dados$spi1.2 <- spi1$fitted
meus_dados$spi3.2 <- spi3$fitted
meus_dados$spi6.2 <- spi6$fitted
meus_dados$spi12.2 <- spi12$fitted

count(is.na(meus_dados$spi12))
#plot(meus_dados$, type = "l")

plot(meus_dados$ano, meus_dados$spi3, type = "l", col = 2,
     xlab = "Anos", ylab = "Frequências por ano")
lines(meus_dados$ano, meus_dados$spi3.2,type = "l", col = 3)

meus_dados[is.na(meus_dados)] <- " "

#correlação entre o que é calculado no R e o que foi calculado pelo professor
meus_dados1 <- meus_dados %>% filter(!row_number() %in% c(1,2))
meus_dados2 <- meus_dados %>% filter(!row_number() %in% c(1,2,3,4,5))
meus_dados3 <- meus_dados %>% filter(!row_number() %in% c(1,2,3,4,5,6,7,8,9,10,11))

mse(as.numeric(meus_dados$spi1),as.numeric(meus_dados$spi1.2))
mse(as.numeric(meus_dados1$spi3),as.numeric(meus_dados1$spi3.2))
mse(as.numeric(meus_dados2$spi6),as.numeric(meus_dados2$spi6.2))
mse(as.numeric(meus_dados3$spi12),as.numeric(meus_dados3$spi12.2))
#também calcular o rmse, mse, mae.

#sumário estatístico de cada série de SPI
summary(spi1$fitted)
summary(na.omit(spi3$fitted))
summary(na.omit(spi6$fitted))
summary(na.omit(spi9$fitted))
summary(na.omit(spi12$fitted))
summary(na.omit(spi24$fitted))
summary(na.omit(spi48$fitted))

#Base de dados com a série de precipitação e as 
#séries dos índices de precipitação para diferentes escalas de tempo
spi_tabela <- data.frame(ano = chuva$year,
                         mes = chuva$month,
                         #data = chuva$date,
                         chuva = chuva$prec,
                         var_spi1 = spi1$fitted,
                         var_spi3 = spi3$fitted,
                         var_spi6 = spi6$fitted,
                         var_spi9 = spi9$fitted,
                         var_spi12 = spi12$fitted,
                         var_spi24 = spi24$fitted,
                         var_spi48 = spi48$fitted)
View(spi_tabela)

####################################################################################
####################################################################################
#################################FREQUENCIA##########################################
#####################################################################################
############

#proporção de frequência de seca total para cada SPI
nrow(spi_tabela[spi_tabela$Series.1<0,])/dim(spi_tabela)[1] #SPI1
nrow(spi_tabela[spi_tabela$Series.1.1<0,])/dim(spi_tabela)[1] #SPI3
nrow(spi_tabela[spi_tabela$Series.1.2<0,])/dim(spi_tabela)[1] #SPI6
nrow(spi_tabela[spi_tabela$Series.1.3<0,])/dim(spi_tabela)[1] #SPI9
nrow(spi_tabela[spi_tabela$Series.1.4<0,])/dim(spi_tabela)[1] #SPI12
nrow(spi_tabela[spi_tabela$Series.1.5<0,])/dim(spi_tabela)[1] #SPI24
nrow(spi_tabela[spi_tabela$Series.1.6<0,])/dim(spi_tabela)[1] #SPI48

#Frequência de seca por ano 
#(Contando os valores de SPI negativos apenas)

#criando um dataframe com apenas os anos. Estou fazendo isso, pois ao calcular a
#frequência por ano, podem haver anos valores de spi negativos e os anos não serão
#contabilizados. Quando houver um ano sem valores negativos, esse ano será preenchido
#com NAN values e depois substituirei esses valores por zero.

spi_tab <- spi_tabela%>% group_by(ano)%>% dplyr::summarise(freq=n())
spi_tab <- data.frame(spi_tab$ano)
colnames(spi_tab) <- "ano"
View(spi_tab)

#Calculando a frequência de valores negativos por SPI
tabela1<- spi_tabela %>% filter(Series.1<0)%>%group_by(ano)%>%
  dplyr::summarise(freq=n())

tabela2<- spi_tabela%>% filter(Series.1.1<0)%>%
  group_by(ano)%>% dplyr::summarise(freq=n())

tabela3<- spi_tabela%>% filter(Series.1.2<0)%>%
  group_by(ano)%>% dplyr::summarise(freq=n())

tabela4<- spi_tabela%>% filter(Series.1.3<0)%>%
  group_by(ano)%>% dplyr::summarise(freq=n())

tabela5<- spi_tabela%>% filter(Series.1.4<0)%>%
  group_by(ano)%>% dplyr::summarise(freq=n())

tabela6<- spi_tabela%>% filter(Series.1.5<0)%>%
  group_by(ano)%>% dplyr::summarise(freq=n())

tabela7<- spi_tabela%>% filter(Series.1.6<0)%>%
  group_by(ano)%>% dplyr::summarise(freq=n())

#combinando essas frequências em um único dataframe
spi_tab1 <- merge(spi_tab, tabela1, by='ano', all=TRUE)
spi_tab2 <- merge(spi_tab1, tabela2, by='ano', all=TRUE)
spi_tab3 <- merge(spi_tab2, tabela3, by='ano', all=TRUE)
spi_tab4 <- merge(spi_tab3, tabela4, by='ano', all=TRUE)
spi_tab5 <- merge(spi_tab4, tabela5, by='ano', all=TRUE)
spi_tab6 <- merge(spi_tab5, tabela6, by='ano', all=TRUE)
spi_tab7 <- merge(spi_tab6, tabela7, by='ano', all=TRUE)

#renomeando as colunas
colnames(spi_tab7) <-c("ano","freq_1","freq_3","freq_6",
                       "freq_9","freq_12","freq_24","freq_48")
View(spi_tab7)
#Onde tiver um valor faltantes NAN será substituído por zero
spi_tab7[is.na(spi_tab7)] <- 0

View(spi_tab7)
#teste de inclinação
sens.slope(na.omit(spi_tab7$freq_12), conf.level = 0.95)
#teste de mann-kendall
mk.test(spi_tab7$freq_1)

ggplot(data=spi_tab7, aes(x=ano, y=freq_1)) +geom_bar(stat = "identity")#+geom_point()
ggplot(data=spi_tab7, aes(x=ano, y=freq_1)) + geom_line(color='red')+  geom_point()
ggplot(data=spi_tab7, aes(x=ano, y=freq_3)) + geom_line(color='blue')+  geom_point()

#Gráfico das séries de frequência de seca mensal de cada ano
plot(spi_tab7$ano, spi_tab7$freq_1, type = "l", col = 2, ylim = c(0, 13),
     xlab = "Anos", ylab = "Frequências por ano")
lines(spi_tab7$ano, spi_tab7$freq_3,type = "l", col = 3)
lines(spi_tab7$ano, spi_tab7$freq_12, type = "l", col = 4)
legend("topright", c("ts1", "ts2", "ts3"), lty = 1, col = 2:4)

#realizando a frequência por década
spi_decada <- spi_tab7 %>% mutate(decade = floor(ano/10)*10) %>% 
  group_by(decade) %>% 
  summarize_all(sum) %>% 
  select(-ano)
spi_decada

#Gráficos
ggplot(data=spi_decada, aes(x=decade, y=freq_6)) +
  geom_bar(stat = "identity")

#correlação entre as frequências
cor(spi_tab7[c("freq_1","freq_3","freq_6","freq_12")])

####################################################################################
###################################INTENSIDADE######################################
####################################################################################
####################################################################################

#intensidade por ano
#todos os valores negativos foram somados e gerada a sua média para
#cada índice de precipitação padronizado
abs(mean(spi_tabela$Series.1[spi_tabela$Series.1 < 0]))
abs(mean(na.omit(spi_tabela$Series.1.1)[na.omit(spi_tabela$Series.1.1) < 0]))
abs(mean(na.omit(spi_tabela$Series.1.2)[na.omit(spi_tabela$Series.1.2) < 0]))
abs(mean(na.omit(spi_tabela$Series.1.3)[na.omit(spi_tabela$Series.1.3) < 0]))
abs(mean(na.omit(spi_tabela$Series.1.4)[na.omit(spi_tabela$Series.1.4) < 0]))
abs(mean(na.omit(spi_tabela$Series.1.5)[na.omit(spi_tabela$Series.1.5) < 0]))
abs(mean(na.omit(spi_tabela$Series.1.6)[na.omit(spi_tabela$Series.1.6) < 0]))


###Calculando a intensidade de seca para cada SPI
for (i in lst("Series.1","Series.1.1","Series.1.2",
              "Series.1.3","Series.1.4","Series.1.5",
              "Series.1.6")){
  print(mean(na.omit(spi_tabela[[i]][spi_tabela[[i]]<0])))}


spi_tab <- spi_tabela%>% group_by(ano)%>% dplyr::summarise(freq=n())
spi_tab <- data.frame(spi_tab$ano)
colnames(spi_tab) <- "ano"
View(spi_tab)

spi_tab8 <- spi_tabela[c("Series.1","Series.1.1","Series.1.2","Series.1.3","Series.1.4",
                         "Series.1.5", "Series.1.6")]

spi_tab8[spi_tab8>0] <- NA
#spi_tab8[spi_tab8>0] <- 0
View(spi_tab8)


spi_tab8$ano <- spi_tabela$ano
spi_tab8 <- spi_tab8[c("ano","Series.1","Series.1.1","Series.1.2","Series.1.3","Series.1.4",
                       "Series.1.5", "Series.1.6")]
View(spi_tab8)

tabela8<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1,na.rm=TRUE))

tabela9<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1.1,na.rm=TRUE))

tabela10<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1.2,na.rm=TRUE))

tabela11<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1.3,na.rm=TRUE))

tabela12<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1.4,na.rm=TRUE))

tabela13<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1.5,na.rm=TRUE))

tabela14<- spi_tab8 %>% group_by(ano)%>% 
  dplyr::summarise(soma=mean(x=Series.1.6,na.rm=TRUE))


spi_tab9 <- merge(spi_tab, tabela8, by='ano', all=TRUE)
spi_tab10 <- merge(spi_tab9, tabela9, by='ano', all=TRUE)
spi_tab11 <- merge(spi_tab10, tabela10, by='ano', all=TRUE)
spi_tab12 <- merge(spi_tab11, tabela11, by='ano', all=TRUE)
spi_tab13 <- merge(spi_tab12, tabela12, by='ano', all=TRUE)
spi_tab14 <- merge(spi_tab13, tabela13, by='ano', all=TRUE)
spi_tab15 <- merge(spi_tab14, tabela14, by='ano', all=TRUE)

View(spi_tab15)

colnames(spi_tab15) <-c("ano","Int_1","Int_3","Int_6",
                       "Int_9","Int_12","Int_24","Int_48")

#Onde tiver um valor faltantes NAN será substituído por zero
spi_tab15[is.na(spi_tab15)] <- 0

View(spi_tab15)
#Intensidade por década
#Aqui eu somo ou faço a média ?
spi_decada2 <- spi_tab15 %>% mutate(decade = floor(ano/10)*10) %>% 
  group_by(decade) %>% 
  summarize_all(mean) %>% 
  abs()%>% 
  select(-ano)
spi_decada2[c('decade',"Int_1","Int_3","Int_6","Int_12")]

mk.test(spi_tab15$Int_48)

plot(spi_tab15$Int_1, type='l')

#for (i in lst("Series.1","Series.1.1","Series.1.2",
#           "Series.1.3","Series.1.4","Series.1.5",
#          "Series.1.6")){
#print(mean((na.omit(spi_tabela)%>% filter(i<0))[[i]]))}
#spi_tab <- spi_tabela[c("ano","mes","Series.1", "Series.1.1",
 #                        "Series.1.2","Series.1.3","Series.1.4","Series.1.5",
  #                       "Series.1.6")]

#a <-merge(spi_tab,tabela1, by='ano')

#for (i in spi_tabela$Series.1){
 # if(i<0){i<-1} else {i<-0}}
#View(spi_tabela2)

#sumário com as estatísticas descritivas da série de precipitação e
#dos índices de precipitação padronizados
sumtable(spi_tabela, vars = c('chuva','Series.1','Series.1.1',
                              'Series.1.2', 'Series.1.3','Series.1.4',
                              'Series.1.5','Series.1.6'),
         labels=c('Chuva','SPI 1','SPI 3','SPI 6', 'SPI 9',
                  'SPI 12','SPI 24', 'SPI 48'))#, )

head(spi6$fitted, 12)

#Gráfico das séries de SPI para diferentes escalas de tempo
par(mfrow=c(2,4))
plot(spi1, 'SPI-1', type = "l")
plot(spi3, 'SPI-3')
plot(spi6, 'SPI-6')
plot(spi9, 'SPI9')
plot(spi12, 'SPI-12')
plot(spi24, 'SPI-24')
plot(spi48, 'SPI-48')

#Gráfico de histograma para as séries de SPI para diferentes 
#escalas de tempo
par(mfrow=c(2,4))
hist(spi1$fitted, main="Histograma do SPI 1",
     xlab=" ", ylab="Frequência", breaks=30)
hist(spi3$fitted, main="Histograma do SPI 3",
     xlab=" ", ylab="Frequência", breaks=30)
hist(spi6$fitted, main="Histograma do SPI 6",
     xlab=" ", ylab="Frequência", breaks=30)
hist(spi9$fitted, main="Histograma do SPI 9",
     xlab=" ", ylab="Frequência", breaks=30)
hist(spi12$fitted, main="Histograma do SPI 12",
     xlab=" ", ylab="Frequência", breaks=30)
hist(spi24$fitted, main="Histograma do SPI 24",
     xlab=" ", ylab="Frequência", breaks=30)
hist(spi48$fitted, main="Histograma do SPI 48",
     xlab=" ", ylab="Frequência", breaks=30)


chuva2 = chuva[c('date','prec')]
chuva2$date = as.Date(chuva2$date)
View(chuva2)

#convertendo a base para o tipo TS
chuva2$prec <- ts(chuva2$prec, frequency = 12,
              start = c(1950,1), end = c(2012,12))
#gráfico Polar
ggseasonplot(chuva2$prec, polar = TRUE)

#estatísticas descritivas por mês 
data.frame(aggregate(prec ~ month, chuva, FUN = function(x) c(media=mean(x),desv=sd(x),
                                                              maxi=max(x), 
                                                              perc = quantile(x,c(.25,.5,.75)),
                                                              iqr = IQR(x))))

spi_tabela2 = spi_tabela
spi_tabela2$Series.1 <- ts(spi_tabela2$Series.1, frequency = 12,
                           start = c(1950,1), end = c(2012,12))


colnames(spi_tabela2)[4:8][1]

#transformando todas as colunas dos SPI para séries temporais.
for (i in colnames(spi_tabela2)[4:8]) {
  spi_tabela2[[i]] <- ts(spi_tabela2[[i]], frequency = 12,
                             start = c(1950,1), end = c(2012,12))
}

#teste Sen's Slope

#Este teste calcula a inclinação (ou seja, taxa de mudança linear)
#e os níveis de confiança de acordo com o método de Sen

sens.slope(spi1$fitted, conf.level = 0.95)
#z = 0.60468, n = 756, p-value = 0.5454, Sen's slope = 0
sens.slope(na.omit(spi3$fitted), conf.level = 0.95)
#z = 1.4869, n = 754, p-value = 0.137, Sen's slope = 0.0001961018 
sens.slope(na.omit(spi6$fitted), conf.level = 0.95)
#z = 3.0627, n = 751, p-value = 0.002193, Sen's slope = 0.0004539784
sens.slope(na.omit(spi9$fitted), conf.level = 0.95)
#z = 4.184, n = 748, p-value = 2.864e-05, Sen's slope = 0.0006427167 
sens.slope(na.omit(spi12$fitted), conf.level = 0.95)
#z = 5.1164, n = 745, p-value = 3.113e-07, Sen's slope = 0.0007611849
sens.slope(na.omit(spi24$fitted), conf.level = 0.95)
#z = 6.3467, n = 733, p-value = 2.199e-10, Sen's slope = 0.0009327544 
sens.slope(na.omit(spi48$fitted), conf.level = 0.95)
#z = 5.4827, n = 709, p-value = 4.19e-08, Sen's slope = 0.001063238 


#teste Mann Kendall

#p-valor abaixo de 0.05 significa que o resultado é estatísticamente significativo
#valor da estatística positivo =  tendência positiva
#valor da estatística negativo =  tendência negativa

mk.test(spi1$fitted)
#z = 0.60468, n = 756, p-value = 0.5454,
#S  = 4.193000e+03, VarS = 4.806156e+07 , tau =  1.481000e-02
mk.test(na.omit(spi3$fitted))
#z = 1.4869, n = 754, p-value = 0.137
# S = 1.027200e+04,varS = 4.771592e+07, tau = 3.626276e-02
mk.test(na.omit(spi6$fitted))
#z = 3.0627, n = 751, p-value = 0.002193
# S = 2.103300e+04,varS = 4.715646e+07, tau = 7.468946e-02
mk.test(na.omit(spi9$fitted))
#z = 4.184, n = 748, p-value = 2.864e-05
# S = 2.856100e+04,varS = 4.659404e+07, tau = 1.022309e-01
mk.test(na.omit(spi12$fitted))
#z = 5.1164, n = 745, p-value = 3.113e-07
# S = 3.471600e+04,varS = 4.603603e+07, tau = 1.252657e-01
mk.test(na.omit(spi24$fitted))
#z = 6.3467, n = 733, p-value = 2.199e-10
# S = 4.202800e+04,varS = 4.384855e+07, tau = 1.566584e-01
mk.test(na.omit(spi48$fitted))
#z = 5.4827, n = 709, p-value = 4.19e-08
# S = 3.453900e+04, varS = 3.968367e+07, tau = 1.376135e-01


#Teste de mudança de ponto
#H0: não há mudança
#h1: há mudança

pettitt.test(chuva$prec)
# U* = 14756, p-value = 0.09764, probable change point at time K =  496 
pettitt.test(spi1$fitted)
# U* = 15156, p-value = 0.08271, probable change point at time K =  222
pettitt.test(na.omit(spi3$fitted))
# U* = 28458, p-value = 2.424e-05, probable change point at time K =  223
pettitt.test(na.omit(spi6$fitted))
# U* = 41597, p-value = 4.681e-11, probable change point at time K =  244
pettitt.test(na.omit(spi9$fitted))
# U* = 51014, p-value < 2.2e-16, probable change point at time K =  244
pettitt.test(na.omit(spi12$fitted))
# U* = 57718, p-value < 2.2e-16 , probable change point at time K =  244
pettitt.test(na.omit(spi24$fitted))
# U* = 77914, p-value < 2.2e-16 , probable change point at time K = 206 
pettitt.test(na.omit(spi48$fitted))
# U* = 89746, p-value < 2.2e-16 , probable change point at time K =  210