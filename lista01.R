#Exercício 1. Crie os seguintes vetores:
#
#(a) (10, 11, 12,…, 30)
a <- 10:30
a

#(b) (30, 29, 28,…, 10)
b <- 30:10
b

#(c) (10, 11, 12,…, 30, 29, 28,…, 10)
c <- c(10:30, 29:10)
c

#Exercício 2. Use a função help do R para descobrir o funcionamento das funções
#rep e seq. Em seguida, utilize estas funções para resolver os seguintes itens:

?rep
?seq

#(a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8), em que há dez ocorrências
#do número 2

a <- rep(seq(2,8, by=2), times=10)
a

#(b) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8, 2), em que há onze
#ocorrências do número 2 e dez ocorrências dos números 4, 6 e 8.

b <- rep(seq(2,8, by=2), times=10, length.out=length(a)+1)
b

#Exercício 3. Utilize a estrutura de vetores do R para realizar as seguintes
#somas:

#(a)
a <- seq(20,30)
seq_a <- a**2 + 4*a
seq_a
soma_a <- sum(seq_a)
soma_a

#(b)
b <- seq(10,20)
seq_b <- 3**b/b + 2**b/b**2
seq_b
soma_b <- sum(seq_b)
soma_b

#Exercício 4. Numa urna há bolas idênticas numeradas de 1 até 100. Serão
#extraídas 40 bolas com reposição desta urna. Simule este experimento e guarde
#o resultado dos sorteios em um vetor.

sorteio <- sample(1:100, 40, replace=TRUE)
sorteio

#(a) Quantas bolas pares foram sorteadas?

a <- sum(sorteio%%2 == 0)

#(b) Quantas bolas maiores do que 70 foram sorteadas?

b <- sum(sorteio > 70)

#(c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?

c <- which(sorteio %% 2 != 0)

#Exercício 5. Crie um função no R que irá simular sucessivos lançamentos de um
#dado até que o número 4 seja obtido pela segunda vez. A função deverá retornar
#o número de lançamentos que foram necessários até o 4 ser obtido pela segunda
#vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7

sorteio_4 <- function(){
  n <- 0
  q <- 0
  while(q < 2){
    q <- q + ( sample(1:6,1) == 4)
    n <- n + 1
  }
  return(n)
}

#Exercício 6. Utilize a função do exercício anterior para replicar o experimento
#dez mil vezes. Para cada replicação, guarde o número de lançamentos num vetor
#chamado quantidades. Por fim, calcule a média de quantidades. Interprete o
#resultado obtido.

quantidades <- vector()

for(i in 1:10000){
  quantidades[i] <- sorteio_4()
}
mean(quantidades)

#Interpretação, seja X: a lancamento de um dado até que caia 4 pela segunda vez.
#X~binomialNegativa(n=2, p=1/6), logo, E[X]=n*1/p = 2*6 = 12. Pela lei dos
#grandes números e pelo fato da media de X ser E[X], a media do vetor quantidades
#tende a 12.


#Exercício 7. Os dois primeiros termos da sequência de Fibonacci são iguais a 1.
#Os termos subsequentes da sequência são encontrados somando os dois termos
#imediatamente anteriores. Escreva uma função com parâmetro de entrada n chamada
#fibonacci que retornará os primeiros n termos da sequência de Fibonacci para
#qualquer 𝑛 ≥ 3.

fibonacci <- function(n){
  sequencia <- c(1,1)
  for(i in 3:n){
    sequencia[i] <- sequencia[i-2] + sequencia[i-1]
  }
  return(sequencia)
}

#Exercício 8. Michael Scott é gerente regional da empresa Dunder Mufflin. Para
#as festividades de fim de ano, Michael propôs aos funcionários Dwight Schrute,
#Jim Halpert, Kevin Malone e Creed Bratton a realização de um amigo oculto entre
#eles. Consideraremos que o sorteio do amigo oculto deu errado quando uma pessoa
#sortear ela mesma (Michael tira Michael, por exemplo). Simule o sorteio do
#amigo oculto. Se ele deu certo, atribua o valor 1; caso contrário, atribua o
#valor 0 (zero). Em seguida, replique este experimento cem mil vezes e calcule a
#proporção de vezes que o amigo oculto deu errado

participantes <- c("Michael Scott", "Dwight Schrute", "Jim Halpert",
                   "Kevin Malone", "Creed Bratton")
n <- 10**5
sucesso <- 0

for(i in 1:n){
  sorteio <- sample(participantes, length(participantes))
  sucesso <- sucesso + prod(participantes != sorteio)
  
}

prop_fracasso <- 1 - sucesso/n
prop_fracasso

#Exercício 9. Luke Skywalker realizará o seguinte passeio aleatório na reta: a
#reta do passeio é formada pelos números inteiros de zero até 𝑁; Luke está em
#um ponto 𝐿 que é maior do que zero e menor do que 𝑁;Luke lança uma moeda
#honesta; se sair coroa, ele dá um passo para a esquerda (e termina na posição
#𝐿 − 1 da reta); se sair cara, ele dá um passo para a direita (e termina na
#posição 𝐿 + 1 da reta). Luke continuará a lançar a moeda e se deslocará até
#que ele chegue em sua casa (e lá ele vai dormir e o passeio acaba) ou até que
#ele chegue (caia) no precipício (e, óbvio, o passeio também acaba nesse caso).

#(a) Para 𝑁 = 20, crie uma função cuja entrada seja 𝐿 (um número maior do que
#zero e menor do que 20) e que retorne 1 se Luke terminou um passeio em sua casa
#ou retorne zero se Luke caiu no precipício.


retorno <- function(L){
  N <- 20
  posicacao <- L
  while(posicacao > 0 & posicacao < N){
    posicacao <- posicacao + sample(c(-1,1),size=1,replace=TRUE)
  }
  if(posicacao == N){
    return(1)
  }
  return(0)
}

#(b) Crie uma função cuja entrada seja 𝐿; esta função deverá replicar o passeio
#da letra (a) 10 mil vezes e retornar a proporção de vezes que Luke chegou em
#sua casa. Sugestão: crie um vetor que, para cada replicação, guardará o
#resultado de um passeio; cada entrada deste vetor será zero ou 1; zero se Luke
#caiu no precipício e 1 se Luke chegou em casa.

retornos <- function(L){
  sucessos <- vector()
  for(i in 1:10000){
    sucessos[i] <- retorno(L)
  }
  return(mean(sucessos))
}

retornos(5)

#(c) Use a função criada em (b) para 𝐿 = 1, 2, … , 19 e, em seguida, use esses
#valores para plotar um gráfico de 𝑥 = 1 ∶ 19 por 𝑦, em que 𝑦 são a
# proporções retornadas pela função criada em (b) para cada 𝑥.

#install.packages("ggplot2")
library(ggplot2)

valor_l <- vector()
alcance <- 1:19

for(i in alcance){
  valor_l[i] <- retornos(i)
}


df <- data.frame(L<-alcance, prop<-valor_l)

ggplot(df, aes(x=L, y=prop))+
  geom_point()






