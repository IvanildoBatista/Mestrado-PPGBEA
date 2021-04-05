#Resolução da questão 2 e 3

#Questão 2 

p1<- 1/2 
p2<-3/4
p3<- 1/3

#letra a)
a<- p1*p2
a
#letra b)
p4 = (p1*p2)+((1-p1)*p3)
p4
b<-(p1*(1-p2))+((1 - p1)*p3)
b
#letra c)
c<-1 - (p1+p4 - a)
c


#Questão 3
#dados da questão 
w1<- 0.5 
f1<-0.3
x1<- 0.2

w2w1<-0.5 #carro2 ser w sendo que o carro1 foi w. 
w2f1<-0.15
w2x1<-0.3

f2w1<-0.25
f2f1<-0.7
f2x1<-0.3

x2w1<-0.25
x2f1<- 0.15
x2x1<-0.4

w3w2<-0.5
w3f2<-0.15
w3x2<- 0.30


#letra a
w2<- (w1*w2w1)+(f1*w2f1)+(x1*w2x1)
w2
f2<-(w1*f2w1)+(f1*f2f1)+(x1*f2x1)
f2
x2<-(w1*x2w1)+(f1*x2f1)+(x1*x2x1)
x2
#então
w3<-(w2*w3w2)+(f2*w3f2)+(x2*w3x2)
w3

#letra b 

w3w1<-(w3w2*w2w1)+(w3f2*f2w1)+(w3x2*x2w1)
w3w1
w1w3<-((w1*w3w1)/(w3))
w1w3


#Questão 5

#dados
c1<-0.9
c2<-0.8
c3<-0.7

c2c3<-c2*c3 #prob da interseção - eventos independentes 
c2c3

#então a confiabilidade do sistema será 
S <-c1*(c2+c3-c2c3)
S

