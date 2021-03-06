#LISTA 7

#####################################################################
#####################################################################
###QUEST�O 1

#LETRA A)

IC <- function(x, sd, n, nc){
  c(round(x - qnorm((1 - nc/2), mean=0, sd=1)*sd/(sqrt(n)),3), 
    round(x + qnorm((1 - nc/2), mean=0, sd=1)*sd/(sqrt(n)),3))
}

IC(800,100,400,0.01)

#LETRA B)

#� necess�rio inverter a fun��o

qnormal = 0.98
nc = function(qnormal,n,sd){
  qnormal*(sqrt(n))/sd
}

# e inserir o resultado em na fun��o pnorm
#como o resultado � bicaudal, temos que multiplic�-lo por 2
#e por fim o resultado ser� subtra�do de 1
round(1-(pnorm(nc(qnormal, 400,100), lower.tail=FALSE)*2),4)

#LETRA C)

#encontrar o valor da amostra � necess�rio criar uma fun��o

tamanho_amostral <- function(nc,sd, ic){
  ((qnorm(1 - nc/2)*sd)/ic)^2
}

round(tamanho_amostral(0.05, 100,7.84),1)

#####################################################################
#####################################################################
###QUEST�O 2

intervalo_proporcao = function(cc,p,n){
  c(p - qnorm(1-((1-cc)/2))*sqrt(p*(1-p)/n),
  p + qnorm(1-((1-cc)/2))*sqrt(p*(1-p)/n))
}

round(intervalo_proporcao(0.9,0.7,625),2)


intervalo_conservador = function(cc,p,n){
  c(p - qnorm(1-((1-cc)/2))*sqrt(1/(4*n)),
    p + qnorm(1-((1-cc)/2))*sqrt(1/(4*n)))
}

round(intervalo_conservador(0.9,0.7,625),3)

#####################################################################
#####################################################################
###QUEST�O 3

#LETRA A)
tamanho_amostral2 <- function(nc,erro,p){
  ((qnorm(1-((1-nc)/2))/erro)^2)*p*(1-p)
}

tamanho_amostral2(0.8,0.01,0.6)

#LETRA B)

n=tamanho_amostral2(0.8,0.01,0.6)

round(intervalo_proporcao(0.95, 0.55,n),3)

#####################################################################
#####################################################################
###QUEST�O 4

#letra a)
round(intervalo_proporcao(0.95,(100/300),300),3)

round(intervalo_conservador(0.95,(100/300),300),3)

#Interpreta��o: Se pud�ssemos construir um grande n�mero de intervalos aleat�rios
#para p, todos baseados em amostras de tamanho n, em 95% desses intervalos
#encontrar�amos o valor do par�metro p.

#letra b)

#arredondando para um valor inteiro
round(tamanho_amostral2(0.95,0.02,(100/300)),0)

#calculando para um n�mero de indv�duos igual a 150
#temos o valor m�ximo de p, que � 0.25 (150/300)*(150/300)

round(tamanho_amostral2(0.95,0.02,(150/300)),0)

#Com tamanho da amostra, em 95% das vezes que estimarmos um intervalo de confian�a
#propor��o amostral ter� uma diferen�a do verdadeiro valor de p por menos que 2%.

#####################################################################
#####################################################################
### QUEST�O 5

#LETRA A)

#intervalo de confan�a
round(intervalo_proporcao(0.90,(180/300),300),3)

#intervalo de confan�a conservador
round(intervalo_conservador(0.90,(180/300),300),3)

#LETRA B)

z_proporcao = function(e,p,n){
  e/(sqrt((p*(1-p))/n))
}

z_score = z_proporcao(0.001,(180/300),300)

round(1-(pnorm(z_score, lower.tail=FALSE)*2),4)

#LETRA C)

round(tamanho_amostral2(0.95,0.0005,(180/300)),0)