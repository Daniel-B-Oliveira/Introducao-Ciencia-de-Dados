#Exercício 1. Crie os seguintes vetores:

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

b <- c(rep(seq(2,8, by=2), times=10), 2)
#ou
b <- rep(seq(2,8, by=2), times=10, length.out = 41)
b

#Exercício 3. Utilize a estrutura de vetores do R para realizar as seguintes
#somas:

#(a)
a <- seq(20,30)
seq_a <- a**2 + 4*a
soma_a <- sum(seq_a)
soma_a

#(b)
b <- seq(10,20)
seq_b <- 3**b/b + 2**b/b**2
soma_b <- sum(seq_b)
soma_b

#Exercício 4. Numa urna há bolas idênticas numeradas de 1 até 100. Serão
#extraídas 40 bolas com reposição desta urna. Simule este experimento e guarde
#o resultado dos sorteios em um vetor.

sorteio <- sample(1:100, 40, replace=TRUE)
sorteio

#(a) Quantas bolas pares foram sorteadas?

a <- sum(sorteio%%2 == 0)
a

#(b) Quantas bolas maiores do que 70 foram sorteadas?

b <- sum(sorteio > 70)
b

#(c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?

c <- which(sorteio %% 2 != 0)
c

#Exercício 5. Crie um função no R que irá simular sucessivos lançamentos de um
#dado até que o número 4 seja obtido pela segunda vez. A função deverá retornar
#o número de lançamentos que foram necessários até o 4 ser obtido pela segunda
#vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7

sorteio_4 <- function(){
  n <- 0
  q <- 0
  while(q < 2){
    q <- q + (sample(1:6,1) == 4)
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
  posicao <- L
  while(posicao > 0 & posicao < N){
    posicao <- posicao + sample(c(-1,1),1)
  }
  return(posicao/N)
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

#(c) Use a função criada em (b) para 𝐿 = 1, 2, … , 19 e, em seguida, use esses
#valores para plotar um gráfico de 𝑥 = 1 ∶ 19 por 𝑦, em que 𝑦 são a
# proporções retornadas pela função criada em (b) para cada 𝑥.

library(ggplot2)

valor_retorno <- vector()
valor_L <- 1:19

for(i in valor_L){
  valor_retorno[i] <- retornos(i)
}

df <- data.frame(L<-valor_L, prop<-valor_retorno)

ggplot(df, aes(x=L, y=prop))+
  geom_point()

#Exercício 10. Harold Frederick Shipman (Nottingham, 14 de janeiro de 1946 —
#Wakefield, 13 de janeiro de 2004), conhecido como “Doutor Morte”, foi um médico
#e assassino em série britânico condenado pela morte de muitos pacientes entre as
#décadas de 1970 e 1990. Dr. Shipman é, talvez, o assassino em série mais
#prolífico da História Moderna. O arquivo dados.txt contém informações sobre o
#sexo, a idade, o local da morte (casa do paciente; hospital; casa de repouso)
#e o ano da morte das vítimas de Shipman. Antes de responder as questões abaixo,
#abra o arquivo dados.txt e compreenda sua estrutura. Importe o arquivo para o
#R e utilize-o para responder os seguintes itens.

dados <- read.table("dados.txt", sep=";", header=TRUE)
str(dados)

dados$LocalDaMorte <- as.factor(dados$LocalDaMorte)
dados$Genero <- as.factor(dados$Genero)

summary(dados)

#(a) Escolha um gráfico apropriado para representar as frequências das
#categorias da variável sexo. Comente os resultados encontrados.

ggplot(dados, mapping=aes(Genero))+
  geom_bar()+
  theme_minimal()

#Vítimas mulheres foram bem mais frequentes do que vítimas homens. 

#(b) Apresente o histograma da variável idade em 8 (argumento bins na geometria
#do histograma) intervalos. Comente os resultados obtidos. Analise este gráfico
#para cada gênero.

ggplot(dados, aes(x=Idade))+
  geom_histogram(bins = 8)+
  theme_minimal()+
  facet_wrap(~Genero)

#Em relação às vítimas mulheres, possuiam, em sua maioria, entre 70 e 90 anos, 
#no geral estavam entre 40 e 100 anos. Em relação às vítimas homens, assim como
#as mulheres, possuiam em média entre 70 e 90 anos, mas com uma dispersão menor
#estando entre 40 e 90 anos.

#(c) Apresente o boxplot da variável idade. Comente os resultados obtidos.

ggplot(dados, aes(y=Idade))+
  geom_boxplot()+
  theme_minimal()

#Em relação à idade, 75% das vítimas tinham mais de 70 anos, sendo a média entre
#70 e 83 anos, com 50% das vítimas acima de 75 anos. 

#(d) Apresente um gráfico para representar o local da morte. Comente os
#resultados obtidos.

ggplot(dados, aes(x=LocalDaMorte))+
  geom_bar()+
  theme_minimal()

#Grande parto dos assassinatos foram cometidos nas residências das vítimas,
#sendo os hospitais e casas de repouso locais menos frequentes.

#(e) Analise graficamente o ano da morte das vítimas de Harold Shipman.

ggplot(dados, aes(AnoDaMorte))+
  geom_histogram(bins=14)+
  theme_minimal()

#Exercício 11. O conjunto primatas.txt apresenta informações sobre tamanho
# (centímetros), peso (libras) e gênero de bonobos e de chimpanzés. Abra o
# arquivo e veja como ele está organizado.

#(a) Importe o arquivo para o ambiente do R. Conheça sua estrutura e peça um
#resumo dos dados com alguma função. (1 ponto)

primatas <- read.table("primatas.txt", sep=":", header=TRUE)

str(primatas)

primatas$especie <- as.factor(primatas$especie)
primatas$genero <- as.factor(primatas$genero)

summary(primatas)

#(b) Construa um gráfico de barras contando quantas espécies de bonobos e
#chimpanzés há no conjunto. Construa também um gráfico de barras mostrando a
#frequência de machos e fêmeas de cada espécie. (4 pontos)

ggplot(primatas, aes(x=especie))+
  geom_bar()

ggplot(primatas, aes(x=genero))+
  geom_bar()+
  facet_wrap(~especie)

#(c) Construa um gráfico para comparar as fêmeas e os machos dos bonobos.
#Em seguida, construa, também, um gráfico para comparar as fêmeas e os machos
#dos chimpanzés. (4 pontos)

library(dplyr)
summary(primatas)

primatas |>
  filter(primatas$especie == "bonobo") |>
  ggplot(aes(altura, peso, col=genero))+
  geom_point()+
  geom_hline(yintercept=38.5)+
  labs(title = "Macacos da espécie bonobo")+
  theme_minimal()

primatas |>
  filter(primatas$especie == "chimpanze") |>
  ggplot(aes(altura, peso,col=genero))+
  geom_point()+
  geom_vline(xintercept=129)+
  labs(title= "Macacos da espécie chimpanze")
  theme_minimal()
  
#(d) Construa um gráfico para comparar as fêmeas dos bonobos e dos chimpanzés.
#Em seguida, construa também um gráfico para comparar os machos dos bonobos e
#dos chimpanzés. (4 pontos)

primatas |>
  filter(primatas$genero == "femea") |>
  ggplot(aes(altura, peso, col=especie))+
  geom_point()+
  labs(title= "Fêmeas de bonobos e chimpanzés")+
  theme_minimal()

primatas |>
  filter(primatas$genero == "macho") |>
  ggplot(aes(altura, peso, col=especie))+
  geom_point()+
  labs(title="Machos de bonobos e chimpanzés")+
  theme_minimal()

#(e) A partir das análises dos itens anteriores, escreva um pequeno texto
#contendo informações sobre os bonobos e os chimpanzés, como exemplo: diferenças
#entre os gêneros de cada espécie e diferenças entre as espécies. (5 pontos)

#Nos bonobos, os machos são mais pesados que as fêmeas, com pesos variando de
#38,5 a 51 kg para os machos e de 27 a 38,5 kg para as fêmeas. Quanto à altura,
#há uma sobreposição entre os sexos: os machos medem entre 127 e 136 cm,
#enquanto as fêmeas ficam entre 120 e 130 cm. Já nos chimpanzés, não há
#sobreposição nem nos pesos nem nas alturas, com os machos sendo mais pesados e
#mais altos. Os pesos dos machos variam de 50 a 65 kg e os das fêmeas de 36 a
#48 kg; em altura, os machos medem entre 130 e 138 cm e as fêmeas entre 120 e
#138 cm. Comparando as duas espécies, percebe-se que os chimpanzés, tanto machos
#quanto fêmeas, tendem a ser mais pesados que os bonobos, embora as alturas
#possam se sobrepor entre fêmeas e machos de ambas as espécies. 

#(f) A partir das variáveis tamanho, peso e genero, construa um modelo de árvore
#de decisão utilizando estruturas condicionais que seja capaz de prever a
#espécie de uma observação. Calcule a acurácia do modelo. (10 pontos)

n <- round(nrow(primatas)*0.8)
id_treino <- sample(1:nrow(primatas),n,FALSE)

treino <- primatas[id_treino,]
teste <- primatas[-id_treino,]

treino |>
  filter(treino$genero == "femea")|>
  ggplot(aes(altura, peso, col=especie))+
  geom_hline(yintercept = 37.45)+
  geom_point()

treino |>
  filter(treino$genero != "femea")|>
  ggplot(aes(altura, peso, col=especie))+
  geom_hline(yintercept=52.7)+
  geom_point()


previsao <- vector()

for(i in 1:nrow(teste)){
  if(teste$genero[i] == 'femea'){
    if(teste$peso[i] > 37.45){
      previsao[i] <- "chimpanze"
    }else{
      previsao[i] <- "bonobo"
    }
  }else{
    if(teste$peso[i] > 52.7){
      previsao[i] <- "chimpanze"
    }else{
      previsao[i] <- "bonobo"
    }
  }
}

mean(previsao == teste$especie)



