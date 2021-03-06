# -*- coding: utf-8 -*-
"""Resolução da terceira lista de exercícios.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1ifiCAeYm1i_jP425Oi1L8Eh4X_BQjWRp

# Resolução da terceira lista de exercícios

Importação das bibliotecas.
"""

!pip install -q tabula-py
import seaborn as sns
import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import tabula
from scipy.stats import chi2_contingency
plt.style.use('ggplot')

"""# QUESTÃO 1

Duas lâmpadas queimadas foram acidentalmente misturadas com seis lâmpadas boas. Se vamos testando as lâmpadas, uma por uma, até encontrar duas defeituosas, qual é a probabilidade de que a última defeituosa seja encontrada no quarto teste?


**Resposta**
"""



"""# QUESTÃO 2

Um empreiteiro apresentou orçamentos separados para a execução da parte elétrica e da parte de encanamento de um edifício. Ele acha que a probabilidade de ganhar a concorrência da parte elétrica é de 1/2. Caso ele ganhe a parte elétrica, a chance de ganhar a parte de encanamento é de 3/4; caso contrário, essa probabilidade é de 1/3. Qual a probabilidade de ele:

(a) ganhar os dois contratos?
    
(b) ganhar apenas um?
    
(c) não ganhar nada?
"""



"""# QUESTÃO 3

Para estudar o comportamento do mercado automobilístico, as marcas foram divididas em três categorias: marca F, marca W, e as demais reunidas como marca X. Um estudo sobre o hábito de mudança de marca mostrou o seguinte quadro de probabilidade:

compra do primeiro carro é feita segundo as seguintes probabilidades: marca W com 50%, marca F com 30% e marca X com 20%.

(a) Qual a probabilidade de um indivíduo comprar o terceiro carro da marca W?
    
(b) Se o terceiro carro é da marca W, qual a probabilidade de o primeiro também ter sido W?
"""



"""# QUESTÃO 4

Um inspetor da seção de controle de qualidade de uma firma examina os artigos de um lote que tem m peças de primeira qualidade e n peças de segunda qualidade. Uma verificação dos $b$ primeiros artigos selecionados ao acaso do lote mostrou que todos eram de segunda qualidade $(b < n$ –$ 1)$. Qual a probabilidade de que entre os dois próximos artigos selecionados, ao acaso, dos restantes, pelo menos um seja de segunda qualidade?
"""



"""# QUESTÃO 5

Um sistema é composto de três componentes 1, 2 e 3, com confiabilidade 0.9, 0.8 e 0.7,respectivamente. O componente 1 é indispensável ao funcionamento do sistema; se 2 ou 3 não funcionam, o sistema funciona, mas com um rendimento inferior. A falha simultânea de 2 e 3 implica o não-funcionamento do sistema. Supondo que os componentes funcionem independentemente, calcular a confiabilidade do sistema.
"""



"""# QUESTÃO 6

Num mercado, três corretoras $A$, $B$ e $C$ são responsáveis por 20%, 50% e 30% do volume total de contratos negociados, respectivamente. Do volume de cada corretora, 20%, 5% e 2%, respectivamente, são contratos futuros em dólares. Um contrato é escolhido ao acaso e este é futuro em dólares. Qual é a probabilidade de ter sido negociado pela corretora $A$? E pela corretora $C$?
"""



"""# QUESTÃO 7

Um inspetor de controle de qualidade pesquisa defeitos em itens produzidos. O inspetor pesquisa as falhas do item numa série de fixações independentes, cada uma com duração fixa. Dado que existe uma falha, seja $p$ a probabilidade de a falha ser detectada durante qualquer fixação (esse modelo é discutido em *“Human Performance in Sampling Inspection,” Human Factors*, 1979, p. 99-105).

a. Assumindo que um item tenha uma falha, qual é a probabilidade de ele ser detectado até o final da segunda fixação (depois da detecção de uma falha, as fixações são interrompidas)?

b. Forneça uma expressão da probabilidade de que uma falha será detectada até o final da enésima fixação.

c. Se, quando uma falha não for detectada em três fixações, o item passar, qual será a probabilidade de que um item com falha passe na inspeção?
    
d. Suponha que 10\% de todos os itens contenham uma falha \[P(item escolhido aleatoriamente apresenta falha) = 0,1]. Com a suposição da parte (c), qual é a probabilidade de um item com falha passar na inspeção (ele passará automaticamente se não tiver falha, mas também pode passar se tiver)?
    
e. Dado que um item passou na inspeção (nenhuma falha em três fixações), qual é a probabilidade de ele possuir uma falha? Calcule para $p$ = 0.5.

"""



"""# QUESTÃO 8

Um sistema consiste em dois componentes. A probabilidade de o segundo componente funcionar de forma satisfatória durante a vida útil do projeto é 0.9, a probabilidade de pelo menos um dos dois componentes funcionar é de 0.96 e a de ambos os componentes funcionarem é de 0.75. Dado que o primeiro componente funciona de forma satisfatória por toda a vida útil do projeto, qual é a probabilidade de o segundo também funcionar?

Vamos definir o valores dados:

P(segundo sistema funcionar) = 0.9

P(ambos funcionarem) = 0.75 (essa é a probabilidade da interseção)

P(Pelo menos um funcionar) = 0.96 (essa a probabilidade da união)

Quero resolver o problema : Qual a probabilidade do segundo sistema funcionar dado que o primeiro sistema funcionou ?
"""

P_A_inter_B = 0.75
P_A_uni_B = 0.96
P_B = 0.9

"""É necessário encontrar a probabilidade do primeiro sistema funciona.

Pela forma $P(A \cup B) = P(A) + P(B) - P(A \cap B)$, faremos então

$0.96 = P(1 sistema) + 0.9 - 0.75$

Isolando $P(1 sistema)$ temos:

$P(1 sistema) = 0.96 - 0.9 + 0.75 = 0.81$
"""

P_A = P_A_uni_B - P_B + P_A_inter_B
P_A

"""Agora temos que calcular a probabilidade condicional:

$P(2 sistema| 1 sistema) = \frac{P(2 sistema \cap 1 sistema)}{P(1 sistema)}$,

Ora $P(2 sistema \cap 1 sistema)$ = $P(1 sistema \cap 2 sistema)$ = 0.75,

Logo teremos que 

$P(2 sistema| 1 sistema) = \frac{0.75}{0.81}$ = 0.926
"""

P_B_inter_A = P_A_inter_B

P_B_cond_A = P_B_inter_A/P_A

round(P_B_cond_A,3)

"""# QUESTÃO 9

Um sistema de computadores usa senhas que são exatamente sete caracteres e cada caracter é uma das 26 letras (a-z) ou 10 inteiros (0-9). Você mantém uma senha para esse sistema de computadores. Seja $A$ o subconjunto de senhas qu começam com a vogal $(a,e,i,o,u)$ e seja $B$ o subconjunto de senhas que terminam com um número par $(0,2,4,6$ ou $8)$.

(a) Suponha que um invasor selecione uma senha aoa acaso. Qual a probabilidade de sua senha ser relacionada?

(b) Suponha que um invasor saiba que sua senha está no evento $A$ e selecione uma senha ao acaso para esse subconjunto. Qual a probabilidade de sua senha ser selecionada?

(c) Suponha que um invasor saiba que sua senha está em $A$ e em $B$ e selecione uma senha ao acaso para esse subconjunto. Qual a probabilidade de sua senha ser selecionada ?
"""



"""# QUESTÃO 10

Um artigo no British Medical Journal \["Comparison of treatment of renal calculi by operative surgery,percutaneous nephrolithotomy, and extracorporeal shock wave lithotripsy" (1986, Vol. 82, pp. 879-892)] fornece a seguinte discussão sobre as taxas de sucesso em remoções de pedras nos rins.
A cirurgia aberta teve uma taxa de sucesso de 78% (273/350) e um método mais recente, a nefrolitotomia percutânea (NP), teve uma taxa desucesso de 83% (289/350). 

Esse novo método pareceu melhor, mas os resultados mudaram quando o diâmetro da pedra foi considerado. 

Para pedras com diâmetros inferiores a dois centímetros, 93% (81/87) dos casos de cirurgia aberta obtiveram sucesso em comparação com apenas 83% (234/270) dos casos de NP. Para pedras maiores ou iguais a dois centímetros, as taxas de sucesso foram 73% (192/263) e 69% (55/80) para cirurgia aberta e NP, respectivamente. Cirurgia aberta é melhor para ambos os tamanhos de pedra, mas tem menos sucesso no total. Em 1951, E.H. Simpson apontou essa aparente contradição (conhecida como **paradoxo de Simpson**), porém o risco ainda hoje persiste. **Explique como a cirurgia aberta pode ser melhor para ambos ostamanhos de cálculos, mas pior no total.**

"""











