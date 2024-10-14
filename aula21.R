#Modelos Suport Vector Machine(SVM)

#1) Classificador de Margem Máxima

data(iris)
iris$Species <- ifelse(iris$Species == "setosa", "setosa", "outro")
iris$Species

iris$Species <- as.factor(iris$Species)
str(iris)

library(ggplot2)
library(e1071)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species))+
  geom_point()

n <- round(0.8*nrow(iris))
id <- sample(1:nrow(iris), n, FALSE)

treino <- iris[id,]
teste <- iris[-id,]


modelo_linear_svm <- svm(formula = Species ~ Sepal.Width + Sepal.Length, data = treino, kernel = "linear", cost = 1)
?svm

summary(modelo_linear_svm)
plot(modelo_linear_svm, treino[, c(1,2,5)])

previsao_linear_svm <- predict(modelo_linear_svm, teste)
mean(previsao_linear_svm == teste$Species)

tune_out <- tune(svm,
                 train.x = treino[,c(1,2)],
                 train.y = treino$Species,
                 ranges = list(cost = c(0.01, 0.1, 10, 50, 100)),
                 kernel = "linear")
tune_out
summary(tune_out)

melhor_modelo <- tune_out$best.model
previsao_svm_mm <- predict(melhor_modelo, newdata = teste[,1:2])
mean(previsao_svm_mm == teste$Species)
