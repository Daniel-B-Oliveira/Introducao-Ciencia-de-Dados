library(rpart)
library(rpart.plot)
library(randomForest)

library(ggplot2)

dados <- read.csv("Heart.csv", header = TRUE)

dados$ChestPain <- as.factor(dados$ChestPain)
dados$Sex <- as.factor(dados$Sex)
dados$Fbs <- as.factor(dados$Fbs)
dados$RestECG <- as.factor(dados$RestECG)
dados$ExAng <- as.factor(dados$ExAng)
dados$Slope <- as.factor(dados$Slope)
dados$Thal <- as.factor(dados$Thal)

dados$AHD <- as.factor(dados$AHD)

dados <- dados[,-1]

summary(dados)

dados <- na.omit(dados)

ggplot(data = dados, aes(x = AHD, y = Chol))+
         geom_boxplot()

ggplot(data = dados, aes(x = AHD, y = MaxHR))+
         geom_boxplot()

table(dados$ChestPain, dados$AHD)

ggplot(data = dados, aes(x = ChestPain, fill = AHD))+
  geom_bar()

n <- round(0.8*nrow(dados))

set.seed(1909)

id_treino <- sample(1:nrow(dados), n, replace = FALSE)
treinamento <- dados[id_treino, ]
teste <- dados[-id_treino,]



modelo_arvore <- rpart(formula = AHD ~., data = treinamento, method = "class")
rpart.plot(modelo_arvore, extra = 101)

previsao <- predict(modelo_arvore, newdata = teste, type = "class")
mean(previsao == teste$AHD)
table(teste$AHD, previsao)

floresta_dados <- randomForest(formula = AHD~., data = treinamento)
floresta_dados

previsao_floresta <- predict(floresta_dados, newdata = teste, type = "class")
mean(previsao_floresta == teste$AHD)

importance(floresta_dados)  #Quanto maior melhor

floresta_dados2 <- randomForest(formula = AHD ~ . - Fbs - RestECG - Sex - Slope, data = treinamento)
previso_floresta2 <- predict(floresta_dados2, newdata = teste, type = "class")
mean(previso_floresta2 == teste$AHD)

varImpPlot(floresta_dados)

floresta_dados3 <- randomForest(formula = AHD ~. + ChestPain + Thal + Ca + Oldpeak + Age, data = treinamento)
previso_floresta3 <- predict(floresta_dados3, newdata = teste, type = "class")
mean(previso_floresta3 == teste$AHD)

importacia <- as.data.frame(importance(floresta_dados))
importacia$MeanDecreaseGini

importacia$variaveis <- rownames(importacia)
importacia

ggplot(data = importacia, aes(y = variaveis, x = MeanDecreaseGini))+
  geom_col()

library(dplyr)
importacia |>
  mutate(variaveis = reorder(variaveis,MeanDecreaseGini )) |>
  ggplot(aes(y = variaveis, x = MeanDecreaseGini))+
  geom_col()
