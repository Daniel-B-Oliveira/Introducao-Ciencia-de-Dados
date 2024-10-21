library(ggplot2)
library(caret)
library(smotefamily)
library(ROSE)
library(rpart)
library(rpart.plot)

#Problema: desbalanceamento da variavel resposta

dados <- read.csv("creditcard.csv", header = TRUE)

str(dados)
summary(dados)

unique(dados$Class)
dados$Class <- as.factor(dados$Class)

dados$Time

table(dados$Class)
prop.table(table(dados$Class))

n <- round(0.75*nrow(dados))

id <- sample(1:nrow(dados), n, FALSE)

treino <- dados[id,]
teste <- dados[-id,]

?rpart

arvore1 <- rpart(Class ~ ., dados, method = "class")
rpart.plot(arvore1)

previsao1 <- predict(arvore1, newdata = teste, type = "class")
mean(previsao1 == teste$Class)

table(teste$Class, previsao)

ggplot(treino, aes(x = V14, y=V17, col=Class))+
  geom_point(alpha = 0.5, show.legend = FALSE)+
  theme_minimal()

?confusionMatrix
confusionMatrix(previsao, teste$Class)

#Métodos: oversample, undersample e over & under sample

?ovun.sample
table(treino$Class)
24105*2

#Over
N <- nrow(treino[treino$Class == "0",]) * 2
dataOver <- ovun.sample(Class~., treino, "over", N)

dadosOver <- dataOver$data
table(dadosOver$Class)

arvore2 <- rpart(Class~., dadosOver, method="class")
rpart.plot(arvore2)

previsao2 <- predict(arvore2, newdata = teste, type="class")
confusionMatrix(previsao2, teste$Class)

#Under
N <- nrow(treino[treino$Class == "1",]) * 2
dataUnder <- ovun.sample(Class~., treino, "under", N)

dadosUnder <- dataUnder$data
table(dadosUnder$Class)

arvore3 <- rpart(Class~., dadosUnder, method="class")
previsao3 <- predict(arvore3, newdata = teste, type = "class")

confusionMatrix(previsao3, teste$Class)

#Over & Under
dataAmbos <- ovun.sample(Class~., treino, "both", p = 0.5)
dadosAmbos <- dataAmbos$data

table(dadosAmbos$Class)
prop.table(table(dadosAmbos$Class))

arvore4 <- rpart(Class~., dadosAmbos, method="class")
previsao4 <- predict(arvore4, newdata = teste, type="class")

confusionMatrix(previsao4, teste$Class)

#Método 4:Synthetic Minority Oversampling
?SMOTE

resultadoSmote <- SMOTE(treino[,-31], target = treino[,31], 5, 10)
dadosSmote <- resultadoSmote$data

table(dadosSmote$class)   #Class -> class ou classe -> class
prop.table(table(dadosSmote$class))

arvore5 <- rpart(class~., dadosSmote, method="class")
previsao5 <- predict(arvore5, newdata = teste, type="class")

confusionMatrix(previsao5, teste$Class)
confusionMatrix(previsao1, teste$Class)

#Tarefa: pegar o conjunto inteiro desse subconjunto creditcard.csv
