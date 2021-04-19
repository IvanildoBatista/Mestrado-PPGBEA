
#2.	Uma empresa de cristais finos sabe por experiência que 10% de suas
#taças possuem defeitos cosméticos e devem ser classificadas como 
#"de segunda linha".


# a) Entre seis taças selecionadas aleatoriamente,
#qual é a probabilidade de uma ser de segunda linha? 

dbinom (1, size = 6, prob = 0.1)

# b) Entre seis taças selecionadas aleatoriamente, 
#qual é a probabilidade de no mínimo duas serem de segunda linha ?

dbinom (2, size = 6, prob = 0.1) + 
  + dbinom (3, size = 6, prob = 0.1) + 
  + dbinom (4, size = 6, prob = 0.1) + 
  + dbinom (5, size = 6, prob = 0.1) + 
  + dbinom (6, size = 6, prob = 0.1)

#c) Se as taças forem examinadas uma a uma, qual será a probabilidade 
#de no máximo cinco terem de ser selecionadas 	para encontrar quatro 
#que não sejam de segunda linha? 

#Para encontrar que 4 não sejam defeituosas, entre as 5 taças é preciso
#que tenha ou nenhuma com defeito ou pelo menos uma com defeito, assim
#sendo:

dbinom (0, size = 5, prob = 0.1) + 
  + dbinom (1, size = 5, prob = 0.1)


#5.	Uma limusine de aeroporto pode acomodar até quatro passageiros 
#em qualquer corrida. A empresa aceitará um máximo de seis reservas 
#e os passageiros devem ter reservas. Pelos registros anteriores, 
#20% de todos os que fazem reservas não aparecem para a corrida. 
#Responda as seguintes perguntas, assumindo independência quando 
#apropriado.


#a. Se forem feitas seis reservas, qual é a probabilidade de ao menos 
#um indivíduo com reserva não poder ser acomodado na corrida?

dbinom (5, size = 6, prob = 0.8) + 
  + dbinom (6, size = 6, prob = 0.8)

#b. Se forem feitas seis reservas, qual é o número esperado de lugares 
#disponíveis quando a limusine parte?

dbinom (3, size = 6, prob = 0.2)*1 + 
  + dbinom (4, size = 6, prob = 0.2)*2 +
  + dbinom (5, size = 6, prob = 0.2)*3 +
  + dbinom (6, size = 6, prob = 0.2)*4


#c. Suponha que a distribuição de probabilidade do número de reservas 
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

# para x= 4 (número máximo de pessoas na limusine)
p4 = 1-(p0+p1+p2+p3)
p4


