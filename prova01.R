#Questão 1

baleia <- read.table("baleias.txt", sep=",", header = TRUE)
str(baleia)

baleia$especie <- as.factor(baleia$especie)

sum(is.na(baleia))

library(ggplot2)

ggplot(baleia, aes(especie))+
  geom_bar()

ggplot(baleia, aes(especie, comprimento))+
  geom_boxplot()

ggplot(baleia, aes(especie, peso))+
  geom_boxplot()

ggplot(baleia, aes(especie, profundidade_maxima))+
  geom_boxplot()

ggplot(baleia, aes(especie, volume_cranio))+
  geom_boxplot()

ggplot(baleia, aes(comprimento, peso, col=especie))+
  geom_point()

ggplot(baleia, aes(peso, volume_cranio, col=especie))+
  geom_point()

#Resposta:
#
#As baleias azuis possuem maior comprimento, estando entre 26m e 35m, enquanto as
#espécies cachalote e jubarte são as menores em média. Em relação ao peso e ao
#volume do crânio, as baleias azuis possuem as maiores medidas, com o peso 
#entre 16.000 kg e 25.000 kg e volume do crânio entre 10.000 m^3 e 20.000 m^3,
#seguinda respectivamente da Fin, Cachalote e Jubarte, sendo essa última a de 
#menores pesos e volume do crânio. A respeito da profundidade, a baleia azul
#é a que pode alcançar regiões mais profundas, sendo em média a maior, seguida
#da baleia Fin, Jubarte e por último a espécie Cachalote é a que alcança
#menores profundidades.

#Questão 2

#(a)
clientes <- read.table("churn.txt", sep=";", header = TRUE)

sum(is.na(clientes))

str(clientes)

clientes <- clientes[,-c(1,2,3)]
str(clientes)

clientes$Geography <- as.factor(clientes$Geography)
clientes$Gender <- as.factor(clientes$Gender)
clientes$HasCrCard <- as.factor(clientes$HasCrCard)
clientes$IsActiveMember <- as.factor(clientes$IsActiveMember)
clientes$Exited <- as.factor(clientes$Exited)

str(clientes)

#(b)

library(rpart)
library(rpart.plot)

n <- round(nrow(clientes)*0.75)
id_treino <- sample(1:nrow(clientes), n, FALSE)

treino <- clientes[id_treino,]
teste <- clientes[-id_treino,]

arvore <- rpart(Exited~., treino, method = "class")
rpart.plot(arvore)

previsao_arvore <- predict(arvore, newdata = teste, type = "class")

mean(previsao_arvore == teste$Exited)
table(teste$Exited, previsao_arvore)

95/(1893+85)
281/(281+239)

#Resposta:
#
#A acuracia do modelo é de 84%. Em relação a matriz de confusão, 4% dos clientes
#que ficaram foram classificados erroneamente, e 54% dos clientes que sairam o
#modelo classificou erroneamente.

#(c)
levels(clientes$Geography)

franca <- clientes[clientes$Geography == "France",]
alemanha <- clientes[clientes$Geography == "Germany",]
espanha <- clientes[clientes$Geography == "Spain",]

franca <- franca[,-2]
alemanha <- alemanha[,-2]
espanha <- espanha[,-2]

#França
n_franca <- round(nrow(franca)*0.75)
id_treino_franca <- sample(1:nrow(franca), n_franca, FALSE)

treino_franca <- franca[id_treino_franca,]
teste_franca <- franca[-id_treino_franca,]

arvore_franca <- rpart(Exited~., treino_franca, method = "class")
rpart.plot(arvore_franca)

previsao_arvore_franca <- predict(arvore_franca, newdata = teste_franca, type = "class")

mean(previsao_arvore_franca == teste_franca$Exited)
table(teste_franca$Exited, previsao_arvore_franca)

27/(27+1030)
125/(125+72)
#A acuracia é de 87%. Com 2,5% dos clientes que ficaram sendo classificados
#erroneamente e 63% dos clientes que sairam sendo classificados erroneamente.

#Alemanha
n_alemanha <- round(nrow(alemanha)*0.75)
id_treino_alemanha <- sample(1:nrow(alemanha), n_alemanha, FALSE)

treino_alemanha <- alemanha[id_treino_alemanha,]
teste_alemanha <- alemanha[-id_treino_alemanha,]

arvore_alemanha <- rpart(Exited~., treino_alemanha, method = "class")
rpart.plot(arvore_alemanha)

previsao_arvore_alemanha <- predict(arvore_alemanha, newdata = teste_alemanha, type = "class")

mean(previsao_arvore_alemanha == teste_alemanha$Exited)
table(teste_alemanha$Exited, previsao_arvore_alemanha)

42/(42+385)
76/(76+124)

#A acurácia é de 81%. Com 9% dos clientes que ficaram sendo classificados
#erroneamente e 38% dos clientes que sairam sendo classificados erroneamente.

#Espanha
n_espanha <- round(nrow(espanha)*0.75)
id_treino_espanha <- sample(1:nrow(espanha), n_espanha, FALSE)

treino_espanha <- espanha[id_treino_espanha,]
teste_espanha <- espanha[-id_treino_espanha,]

arvore_espanha <- rpart(Exited~., treino_espanha, method = "class")
rpart.plot(arvore_espanha)

previsao_arvore_espanha <- predict(arvore_espanha, newdata = teste_espanha, type = "class")

mean(previsao_arvore_espanha == teste_espanha$Exited)
table(teste_espanha$Exited, previsao_arvore_espanha)

20/(512+20)
43/(43+44)

#A acuracia é de 89%. Com 3,7% dis clientes que ficaram sendo classificados
#erroneamente e 50% dos clientes que sairam sendo classificados erroneamente.

#Resposta:
#
#As previsões da Alemanha possuem menos erros de classificação em relação
#aos individuos que sairam. E a classificação em (c) não depende da variàvel
#país pois todos os indivíduos de cada conjunto pertencem ao mesmo país.


