library(rpart)
library(rpart.plot)
library(randomForest)

dados <- read.table("Carseats.txt", header = TRUE, sep = ";")
?read.table

str(dados)
summary(dados)

dados$ShelveLoc <- as.factor(dados$ShelveLoc)
dados$Urban <- as.factor(dados$Urban)
dados$US <- as.factor(dados$US)

#Queremos modelar sales
#Tranformar sales para categoria, pois sabemos prever categorias

dados$vendas <- as.factor(ifelse(dados$Sales < 8, "baixo", "alto"))
dados <- dados[,-1]

summary(dados)

n <- round(0.8*nrow(dados))

set.seed(123)

id_treino <- sample(1:nrow(dados), size = n, replace = FALSE)

treino <- dados[id_treino,]
teste <- dados[-id_treino,]

arvore <- rpart(vendas~., data = treino, method = "class")
rpart.plot(arvore, extra = 101)

previsao_arvore <- predict(arvore, newdata = teste, type = "class")
mean(previsao_arvore == teste$vendas)

#Over feeding
previsao_arvore_treino <- predict(arvore, newdata = treino, type = "class")
mean(previsao_arvore_treino == treino$vendas)

names(arvore)
arvore$frame
nrow(arvore$frame)    #Queremos podar a arvore

floresta <- randomForest(formula = vendas~., data = treino)

previsao_floresta <- predict(floresta, newdata = teste, type = "Class")
mean(previsao_floresta == teste$vendas)

importance(floresta)

#ajustando floresta
floresta1 <- randomForest(formula = vendas~. - Urban - US - Education, data = treino)
previsao_floresta1 <- predict(floresta1, newdata = teste, type = "Class")
mean(previsao_floresta1 == teste$vendas)

#voltando a arvore
arvore$cptable

arvore_podada <- prune(arvore, cp = 0.02205882)
rpart.plot(arvore_podada, extra = 101)

nrow(arvore_podada$frame)

previsao_podada <- predict(arvore_podada, newdata = teste, type = "class")
mean(previsao_podada == teste$vendas)

arvore_podada$cptable


