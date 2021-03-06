#Resolu��o da quest�o 4 

#O seguinte conjunto de dados representa as "vidas" de 40 baterias de carro da mesma marca e mesmas caracter�sticas com aproxima��o at� d�cimos do ano. As baterias tinham garantia para 3 anos.

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

#(a) Construa a distribui��o de frequ�ncia e o histograma;

d <- fdt(x ,start = 1.5 , end = 5 , h = 0.5)
print(d ,format = TRUE, col = 1:4 , pattern = "%.2f")

plot(d) #histograma

#(b) Fa�a o gr�fico da distribui��o de frequ�ncias relativas acumuladas.
plot(d, type='cfp')


#(c) Calcule a m�dia aritm�tica dos dados originais;
mean(x)

#(d) Usando a distribui��o de frequ�ncia conforme obtido em a calcule a m�dia novamente. 
#Para tal, considere os pontos m�dios de cada classe (m�dia entre os dois limites de cada classe)
#para serem os valores da vari�vel no c�lculo da m�dia.
mean(d)

#(e) Obtenha a vari�ncia para os dados originais conforme feito para a m�dia em c.
var(x)

#(f) Obtenha a vari�ncia a partir da distribui��o de frequ�ncia conforme feito para 
#a m�dia no item d.
var(d)