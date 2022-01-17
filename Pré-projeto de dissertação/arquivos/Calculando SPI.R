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
#### new function
#### run_theory() 
#### applied the run theory to a time series ()
## time_serie: a numeric vector with no NA values
## threshold: a numeric value in which the features (below) of run theory is measured
run_theory <- function(time_serie,
                       threshold = -.5)
{
  
  dataBase <- data.frame(time_serie = time_serie) %>%
    transform(masked = ifelse(time_serie >= threshold, 1, 0)) %>%
    transform(index = cumsum(masked), index_rev = cumsum(abs(masked-1)))
  
  ####  Duration, Severity, Intesity ####  
  
  dataBase[dataBase$masked == 0, ] %>%
    by(., .$index, function(z){
      
      data.frame(D = dim(z)[1],
                 S = abs(sum(z$time_serie)),
                 I = abs(sum(z$time_serie))/dim(z)[1],
                 date_ini = row.names(z)[1],
                 date_fin = row.names(z)[nrow(z)])
      
    }) %>% do.call(rbind, .) -> df1
  
  
  ####  Interarrival ####  
  
  dataBase[dataBase$masked != 0, ] %>%
    by(., .$index_rev, function(z){
      
      data.frame(Int = dim(z)[1])
      
    }) %>% unlist() -> Int
  
  # first condition 
  
  if ((dataBase$masked[1]) == 1){
    
    Int <- Int[-1] 
    
  } else {
    
    Int <- Int
    
  }
  
  # second contidition
  
  if (dataBase$masked[length(dataBase$masked)] == 1) {
    
    Int <- Int + df1$D
    
  } else { 
    
    n <- c(Int, 0 ) + df1$D
    Int <- n[-length(n)]
    
  }
  
  
  return(list(Duration = as.numeric(df1$D),
              Severity = as.numeric(df1$S),
              Intesity = as.numeric(df1$I),
              Date_Ini_Ev = as.character(df1$date_ini),
              Date_Fin_Ev = as.character(df1$date_fin),
              Interarrival = as.numeric(Int)))
  
}

#### old function
## SPI: SCI output
## spi_escala: SPI scale (used to omit NA values)
## umbral: threshold in which is computed the characteristic of SPI

drought.index<-function(SPI, 
                        spi_escala, 
                        umbral)
{
  #Calculo de parametros de sequia: Intensidad, Duraci?n, Severidad e Interarrival.
  #SPI: serie de tiempo
  #spi_escala: escala a en la que se calculo el spi
  #umbral: valor minimo para calcular los parametros
  #
  #Adrian Huerta
  SPI<-SPI[spi_escala:length(SPI)]
  #------------------------ severidad, duracion e intensidad ------------------------
  sdi<-SPI
  sdi[sdi >= umbral]<-NA
  idx<- 1 + cumsum(is.na(sdi))
  no.NA<-!is.na(sdi)
  D.S<-split(sdi[no.NA],idx[no.NA])
  
  D<-matrix(nrow=length(D.S),ncol=1)
  S<-matrix(nrow=length(D.S),ncol=1)
  for (i in 1:length(D.S)){
    D[i]<- length(D.S[[i]])
    S[i]<- -1*sum(D.S[[i]])
  }
  I<-S/D
  rm(idx,no.NA)
  
  #---------------------------------- Interarrival ----------------------------------
  int<-SPI
  int[int < umbral]<-NA
  idx<- 1 + cumsum(is.na(int))
  no.NA<-!is.na(int)
  I.n<-split(int[no.NA],idx[no.NA])
  
  intera.raw<-matrix(nrow=length(I.n),ncol=1)
  for (i in 1:length(I.n)){
    intera.raw[i]<- length(I.n[[i]])
  }
  
  #1era condici?n.
  if (is.na(int[1])==FALSE){
    intera.y<-intera.raw[c(-1),]} else
    {intera.y<-intera.raw }
  
  #2da condicion
  if (is.na(int[length(int)])==FALSE){
    interarrival<-intera.y+D} else
    { n<-c(intera.y,0)+D
    interarrival<-matrix(n[-length(n)],ncol=1)}
  
  return(list(Duracion=as.numeric(D),Severidad=as.numeric(S),Intensidad=as.numeric(I),Interarrival=as.numeric(interarrival)))
}

chuva = read_xlsx('precipitacao.xlsx')

#head(chuva)

#plot(chuva$prec, type = "l")


spi1 <- spi(na.omit(chuva$prec), 1)
spi3 <- spi(na.omit(chuva$prec), 3)
spi6 <- spi(na.omit(chuva$prec), 6)
spi9 <- spi(na.omit(chuva$prec), 9)
spi12 <- spi(na.omit(chuva$prec), 12)
spi24 <- spi(na.omit(chuva$prec), 24)
spi48 <- spi(na.omit(chuva$prec), 48)


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
                         data = chuva$date,
                         chuva = chuva$prec,
                         var_spi1 = spi1$fitted,
                         var_spi3 = spi3$fitted,
                         var_spi6 = spi6$fitted,
                         var_spi9 = spi9$fitted,
                         var_spi12 = spi12$fitted,
                         var_spi24 = spi24$fitted,
                         var_spi48 = spi48$fitted)
#View(chuva)
#View(spi_tabela)
#st(chuva)

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