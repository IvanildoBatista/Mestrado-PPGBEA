
#2.	Uma empresa de cristais finos sabe por experi�ncia que 10% de suas
#ta�as possuem defeitos cosm�ticos e devem ser classificadas como 
#"de segunda linha".


# a) Entre seis ta�as selecionadas aleatoriamente,
#qual � a probabilidade de uma ser de segunda linha? 

dbinom (1, size = 6, prob = 0.1)

# b) Entre seis ta�as selecionadas aleatoriamente, 
#qual � a probabilidade de no m�nimo duas serem de segunda linha ?

dbinom (2, size = 6, prob = 0.1) + 
  + dbinom (3, size = 6, prob = 0.1) + 
  + dbinom (4, size = 6, prob = 0.1) + 
  + dbinom (5, size = 6, prob = 0.1) + 
  + dbinom (6, size = 6, prob = 0.1)

#c) Se as ta�as forem examinadas uma a uma, qual ser� a probabilidade 
#de no m�ximo cinco terem de ser selecionadas 	para encontrar quatro 
#que n�o sejam de segunda linha? 

#Para encontrar que 4 n�o sejam defeituosas, entre as 5 ta�as � preciso
#que tenha ou nenhuma com defeito ou pelo menos uma com defeito, assim
#sendo:

dbinom (0, size = 5, prob = 0.1) + 
  + dbinom (1, size = 5, prob = 0.1)


#5.	Uma limusine de aeroporto pode acomodar at� quatro passageiros 
#em qualquer corrida. A empresa aceitar� um m�ximo de seis reservas 
#e os passageiros devem ter reservas. Pelos registros anteriores, 
#20% de todos os que fazem reservas n�o aparecem para a corrida. 
#Responda as seguintes perguntas, assumindo independ�ncia quando 
#apropriado.


#a. Se forem feitas seis reservas, qual � a probabilidade de ao menos 
#um indiv�duo com reserva n�o poder ser acomodado na corrida?

dbinom (5, size = 6, prob = 0.8) + 
  + dbinom (6, size = 6, prob = 0.8)

#b. Se forem feitas seis reservas, qual � o n�mero esperado de lugares 
#dispon�veis quando a limusine parte?

dbinom (3, size = 6, prob = 0.2)*1 + 
  + dbinom (4, size = 6, prob = 0.2)*2 +
  + dbinom (5, size = 6, prob = 0.2)*3 +
  + dbinom (6, size = 6, prob = 0.2)*4


#c. Suponha que a distribui��o de probabilidade do n�mero de reservas 
#feitas seja dada na tabela a seguir.

# para x= 0
p0 = dbinom (0, size = 3, prob = 0.8)*0.1 + 
  + dbinom (0, size = 4, prob = 0.8)*0.2 +
  + dbinom (0, size = 5, prob = 0.8)*0.3 +
  + dbinom (0, size = 6, prob = 0.8)*0.4
p0

# para x= 1
p1 = dbinom (1, size = 3, prob = 0.8)*0.1 + 
  + dbinom (1, size = 4, prob = 0.8)*0.2 +
  + dbinom (1, size = 5, prob = 0.8)*0.3 +
  + dbinom (1, size = 6, prob = 0.8)*0.4
p1

# para x= 2
p2 = dbinom (2, size = 3, prob = 0.8)*0.1 + 
  + dbinom (2, size = 4, prob = 0.8)*0.2 +
  + dbinom (2, size = 5, prob = 0.8)*0.3 +
  + dbinom (2, size = 6, prob = 0.8)*0.4
p2

# para x= 3
p3 = dbinom (3, size = 3, prob = 0.8)*0.1 + 
  + dbinom (3, size = 4, prob = 0.8)*0.2 +
  + dbinom (3, size = 5, prob = 0.8)*0.3 +
  + dbinom (3, size = 6, prob = 0.8)*0.4
p3

# para x= 4 (n�mero m�ximo de pessoas na limusine)
p4 = 1-(p0+p1+p2+p3)
p4


