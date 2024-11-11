library(ggplot2)

olive <- read.table("olive.txt", T, sep=",")

str(olive)
unique(olive$region)

olive$region <- as.factor(olive$region)
summary(olive)

#a)
olivePadronizado <- scale(olive[,-1])
summary(olivePadronizado)

#b)
#k=3
modeloK3Means <- kmeans(olivePadronizado, centers = 3, nstart = 20 )
olive$cluster_k3 <- as.factor(modeloK3Means$cluster)

ggplot(olive, aes(x=cluster_k3, fill=region))+
  geom_bar()

#c)
#k=4
modeloK4means <- kmeans(olivePadronizado, centers = 4, nstart = 20 )
olive$cluster_k4 <- as.factor(modeloK4means$cluster)

ggplot(olive, aes(x=cluster_k4, fill=region))+
  geom_bar()

#k=5
modeloK5means <- kmeans(olivePadronizado, centers = 5, nstart = 20 )
olive$cluster_k5 <- as.factor(modeloK5means$cluster)

ggplot(olive, aes(x=cluster_k5, fill=region))+
  geom_bar()

#Questão 1

library(e1071)

diabetes <- read.table("diabetes.txt", TRUE, ";")
str(diabetes)

diabetes <- diabetes[,-1]
diabetes$Diabetic <- as.factor(diabetes$Diabetic)

n <- round(nrow(diabetes)*0.8)
id <- sample(1:nrow(diabetes), n, FALSE)

treino <- diabetes[id,]
teste <- diabetes[-id,]

?svm
modeloSvmLi <- svm(Diabetic~., data = treino)

previsaoLi <- predict(modeloSvmLi, newdata=teste)
mean(previsaoLi == teste$Diabetic)

?tune
tunePoly <- tune(svm,
                 train.x = treino[,-9],
                 train.y = treino$Diabetic,
                 kernel = "polynomial",
                 rages = list(cost = c(0.01, 0.1, 1 ,10, 100),
                              degre = 1:4))

tunePoly$best.model
previsaoPoly <- predict(tunePoly$best.model, newdata = teste[,-9])
mean(teste$Diabetic == previsaoPoly)


tuneRad <- tune(svm,
                train.x = treino[,-9],
                train.y = treino$Diabetic,
                kernel = "radial",
                ranges = list(cost = c(0.01, 0.1, 1 ,10, 100),
                              gamma = c(0.01, 0.1, 1, 2,3,4)))


