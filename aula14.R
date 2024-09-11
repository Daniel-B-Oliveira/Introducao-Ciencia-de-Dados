library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

dados <- read.csv("cancer.csv", header = TRUE)
str(dados)

dados$diagnosis <- as.factor(dados$diagnosis)

indices_treinamento <- sample(1:nrow(dados), size = 450, replace = FALSE)

treinamento <- dados[indices_treinamento,]
teste <- dados[-indices_treinamento,]

?train
#caret::train()
?trainControl

validacao_cruzada <- trainControl(method = 'cv', number = 10)   #cv: cross validation
valores_k <- data.frame(k=1:20)

modelo <- train(x = treinamento[,-1],
                y=treinamento$diagnosis,
                method = "knn",
                preProcess = c("center", "scale"),
                trControl = validacao_cruzada,
                tuneGrid = valores_k)
plot(modelo)

modelo_knn <- knn(train = scale(treinamento[,-1]),
                  test = scale(teste[,-1]),
                  cl = treinamento$diagnosis,
                  k = 5)

mean(modelo_knn == teste$diagnosis)

resultados <- data.frame(modelo = "knn", acuracia = mean(modelo_knn == teste$diagnosis))
resultados

?rpart
modelo_arvore <- rpart(formula = diagnosis ~.,
                       data = treinamento,
                       method = "class")

rpart.plot(modelo_arvore, extra = 101)


?predict
previsao_arvore <- predict(modelo_arvore,
                           newdata = teste,
                           type = "class")

acuracia_arvore <- mean(previsao_arvore == teste$diagnosis)
resultados <- rbind(resultados, c(modelo = "arvore", acuracia = mean(previsao_arvore == teste$diagnosis)))

resultados

?randomForest

modelo_floresta <- randomForest(formula = diagnosis ~.,data = treinamento)
previsa_floresta <- predict(modelo_floresta, newdata = teste, type = "class")
acuracia_foresta <- mean(previsa_floresta == teste$diagnosis)

resultados <- rbind(resultados, c(modelo = "floresta", acuracia = acuracia_foresta))
resultados
