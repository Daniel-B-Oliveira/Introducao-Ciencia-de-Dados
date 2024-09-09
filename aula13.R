library(ggplot2)
library(dplyr)

dados <- read.csv("cancer.csv", header = TRUE)
str(dados)

dados$diagnosis <- as.factor(dados$diagnosis)

n <- round(nrow(dados)*0.8)
indices_treinamento <- sample(1:nrow(dados), size = n, replace = FALSE)

treinamento <- dados[indices_treinamento,]
teste <- dados[-indices_treinamento,]

?scale
treinamento_padronizado <- scale(treinamento[,-1])
teste_padronizado <- scale(teste[,-1])

library(class)
?knn

modelo_knn <- knn(train = treinamento_padronizado, test = teste_padronizado, cl = treinamento$diagnosis, k = 1)

acuracia_knn <- mean(teste$diagnosis == modelo_knn)

acuracia_n_vizinho <- function(n_vizinho){
  modelo_knn <- knn(train = treinamento_padronizado, test = teste_padronizado, cl = treinamento$diagnosis, k = n_vizinho)
  return(mean(teste$diagnosis == modelo_knn))
}

table(modelo_knn, teste$diagnosis)

#explicação Gini
#G = P_a*G_a + P_b*G_b
#G_a = P_x*(P_x - 1) + P_y(P_y - 1)

library(rpart)
library(rpart.plot)

?rpart

modelo_arvore <- rpart(formula = diagnosis ~ ., data = treinamento, method = "class")
rpart.plot(modelo_arvore, extra = 101)
