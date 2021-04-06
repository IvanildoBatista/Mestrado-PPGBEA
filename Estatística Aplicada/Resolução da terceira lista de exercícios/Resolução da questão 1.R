install.packages('stringr')
library(stringr)
#Questão 1 

lampadas<-rep(c("B", "D"), times = c(6, 2))
lampadas
L <-100000

#casos em que tem 2 lampadas defeituosas 

experimento <- sample(lampadas,4)
lampdefeituosa <- replicate(L,{experimento <- sample(lampadas,4)
(experimento[4]=="D") 
n <- str_count(experimento, "D")
length(which(n==1))==2})

s<-mean(lampdefeituosa)
#solução 
round(s/2,3)