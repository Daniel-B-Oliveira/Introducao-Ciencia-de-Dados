library(class)
library(caret)

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



