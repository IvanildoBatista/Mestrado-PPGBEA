#Resolução da questão 4 

#O seguinte conjunto de dados representa as "vidas" de 40 baterias de carro da mesma marca e mesmas características com aproximação até décimos do ano. As baterias tinham garantia para 3 anos.

#2.2, 4.1, 3.5, 4.5, 3.2, 3.7, 3.0, 2.6, 3.4, 1.6, 3.1, 3.3, 3.8, 3.1, 4.7, 3.7, 2.5,
#4.3, 3.4, 3.6, 2.9, 3.3, 3.9, 3.1, 3.3, 3.1, 3.7, 4.4, 3.2, 4.1, 1.9, 3.4, 4.7, 3.8,
#3.2, 2.6, 3.9, 3.0, 4.2, 3.5

# Instalando o pacote
install.packages('fdth')
library(fdth)

#Criando um vetor
x <- c(2.2, 4.1, 3.5, 4.5, 3.2, 3.7, 3.0, 2.6, 3.4, 1.6, 3.1,
       3.3, 3.8, 3.1, 4.7, 3.7, 2.5, 4.3, 3.4, 3.6, 2.9, 3.3,
       3.9, 3.1, 3.3, 3.1, 3.7, 4.4, 3.2, 4.1, 1.9, 3.4, 4.7,
       3.8, 3.2, 2.6, 3.9, 3.0, 4.2, 3.5)

#imprimindo o vetor
x

#(a) Construa a distribuição de frequência e o histograma;

d <- fdt(x ,start = 1.5 , end = 5 , h = 0.5)
print(d ,format = TRUE, col = 1:4 , pattern = "%.2f")

plot(d) #histograma

#(b) Faça o gráfico da distribuição de frequências relativas acumuladas.
plot(d, type='cfp')


#(c) Calcule a média aritmética dos dados originais;
mean(x)

#(d) Usando a distribuição de frequência conforme obtido em a calcule a média novamente. 
#Para tal, considere os pontos médios de cada classe (média entre os dois limites de cada classe)
#para serem os valores da variável no cálculo da média.
mean(d)

#(e) Obtenha a variância para os dados originais conforme feito para a média em c.
var(x)

#(f) Obtenha a variância a partir da distribuição de frequência conforme feito para 
#a média no item d.
var(d)