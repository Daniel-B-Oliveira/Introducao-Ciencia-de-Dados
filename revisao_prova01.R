#1

dados <- read.csv("chicago.csv", header = TRUE)
str(dados)

dados <- dados[,-1]
str(dados)

dados$season <- as.factor(dados$season)
str(dados)

summary(dados)

#(a)

mortes_cadio <- sum(dados$cvd)
mortes_cadio

#(b)

autumn <- dados[dados$season == "Autumn",]
spring <- dados[dados$season == "Spring",]
summer <- dados[dados$season == "Summer",]
winter <- dados[dados$season == "Winter",]

#(c)
mean(winter$temp)
mean(winter$rhum, na.rm = TRUE)

ggplot(dados, aes(season, temp))+
  geom_boxplot()

#(d)

#existe mapeamento para forma: shape=categoria
ggplot(dados, aes(time, temp, col=season))+
  geom_point(alpha=0.7)

#Questão 2. Papagaio-do-mar  ́e o nome comum dado as aves charadriiformes da
#familia dos alcideos, pertencentes ao genero Fratercula. Existem tres especies
#de papagaios-do-mar conhecidas: arctica, corniculata e cirrhata. O conjunto
#papagaio.txt apresenta informações sobre o peso (em gramas), o tamanho
#(em centímetros), a envergadura da asa (em centímetros) e a espécie de 500
#papagaios-do-mar.

#(a)
papagaio <- read.table("papagaio.txt", sep=",", header=TRUE)
str(papagaio)
papagaio$especie <- as.factor(papagaio$especie)

str(papagaio)

#(b)

ggplot(papagaio, aes(especie))+
  geom_bar()

#(c)

ggplot(papagaio, aes(especie,tamanho))+
  geom_boxplot()
ggplot(papagaio, aes(especie,peso))+
  geom_boxplot()
ggplot(papagaio, aes(especie,envergadura))+
  geom_boxplot()

ggplot(papagaio, aes(tamanho, peso, col=especie))+
  geom_point()
ggplot(papagaio, aes(tamanho, envergadura, col=especie))+
  geom_point()
ggplot(papagaio, aes(peso, envergadura, col=especie))+
  geom_point()


#teste

n <- round(0.8*nrow(papagaio))

id_treino <- sample(1:nrow(papagaio), n, replace=FALSE)

treino <- papagaio[id_treino,]
teste <- papagaio[-id_treino,]


library(rpart)
library(rpart.plot)

arvore <- rpart(formula=especie~., data=treino, method="class")
#rpart.plot(arvore)

?predict

previsao_arvore <- predict(arvore, newdata = teste, type="class")
mean(previsao_arvore==teste$especie)

library(class)


treino_padronizado <- scale(treino[,-4])
teste_padronizado <- scale(teste[,-4])

modelo_knn <- knn(treino_padronizado, teste_padronizado, treino$especie, k=5)
modelo_knn

mean(modelo_knn==teste$especie)
mean(previsao_arvore==teste$especie)









