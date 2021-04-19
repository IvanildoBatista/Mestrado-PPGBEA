# -*- coding: utf-8 -*-
"""Resolução da quarta lista de Estatística Aplicada.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1svxd4hKz9ZJuCbyrdiTcZUc9ZBJ5UUTm

O presidente da *Martin Corporation* está considerando duas alternativas de investimento $X$ e $Y$. Se cada uma das alternativas for levada a diante há 4 possibilidades de resultado. O valor presente líquido e sua respectiva probabilidade de ocorrência são mostrados abaixo:

a) Qual é o valor esperado do valor presente do lucro para os investimentos $X$ e $Y$? E qual das oportunidades é a mais interessante (maior valor esperado do VPLucro)? 
    
b) Qual a variância do valor presente do lucro para os investimentos $X$ e $Y$? E qual das oportunidades é a mais arriscada (maior variância do VPLucro)?
"""

import pandas as pd
import numpy as np

d = {'VP Lucro X' : [20,8,10,3], 
     'Probabilidade X' : [0.2,0.3,0.4,0.1],
     'VP Lucro Y' : [12,9,16,11], 
     'Probabilidade Y' : [0.1,0.3,0.1,0.5]}
resultado = pd.DataFrame(d)
resultado

"""letra a)"""

valor_x  = (resultado['VP Lucro X']*resultado['Probabilidade X']).sum()
valor_y = (resultado['VP Lucro Y']*resultado['Probabilidade Y']).sum()

print('Valor esperado do investimento X:',round(valor_x,2))
print('Valor esperado do investimento Y:',valor_y)

"""letra b)"""

print('A variância do investimento X:',
      (((resultado['VP Lucro X']-valor_x)**2)*resultado['Probabilidade X']).sum())
print('A variância do investimento Y:',
      (((resultado['VP Lucro Y']-valor_y)**2)*resultado['Probabilidade Y']).sum())