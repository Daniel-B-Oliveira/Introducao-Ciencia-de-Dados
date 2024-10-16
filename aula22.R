library(palmerpenguins)
library(dplyr)
library(e1071)
library(ggplot2)

dados <- penguins
summary(dados)

levels(dados$species)
dados$species <- ifelse(dados$species == "Adelie", "Adelie", "Outro")
dados$species <- as.factor(dados$species)
summary(dados)

dados <- dados |>
  select(species,
         bill_length_mm,
         bill_depth_mm,
         flipper_length_mm,
         body_mass_g)
summary(dados)

dados <- na.omit(dados)

ggplot(dados, aes(x=flipper_length_mm, body_mass_g, col = species))+
  geom_point()
ggplot(dados, aes(x=bill_length_mm, body_mass_g, col = species))+
  geom_point()

dados1 <- dados |>
  select(species,
         bill_length_mm,
         body_mass_g)

n <- round(0.8*nrow(dados1))
id <- sample(1:nrow(dados1), n, FALSE)

treino <- dados1[id,]
teste <- dados1[-id,]

modelo1 <- svm(formula = species ~ .,
                   data = treino,
                   kernel = "linear",
                   cost = 1)

modelo1
plot(modelo1, treino)

previsao1 <- predict(modelo1, newdata = teste)
mean(previsao1 == teste$species)

names(treino)

tune1 <- tune(svm,
                 train.x = treino[,2:3],
                 train.y = treino$species,
                 ranges = list(cost = c(0.01, 0.1, 1 ,10, 50, 100)),
                 kernel = "linear")

tune1$performances
summary(tune1)

modelo1_mm <- tune1$best.model
previsao1_mm <- predict(modelo1_mm, newdata = teste[, 2:3])

mean(previsao1 == teste$species)
mean(previsao1_mm == teste$species)

?svm

tune2 <- tune(svm,
              train.x = treino[,2:3],
              train.y = treino$species,
              ranges = list(cost = c(0.01, 0.1, 1 ,10, 100),
                            gamma = c(0.01, 0.1, 1, 2,3,4)),
              kernel = "radial")
tune2$performances

modelo2 <- svm(formula = species ~ .,
               data = treino,
               kernel = "radial",
               cost = 100,
               gamma = 0.1)
plot(modelo2, treino)

previsao2 <- predict(modelo2, newdata = teste[,2:3])
mean(previsao2 == teste$species)

table(previsao2, teste$species)

tune3 <- tune(svm,
              train.x = treino[,2:3],
              train.y = treino$species,
              ranges = list(cost = c(0.01, 0.1, 1 ,10, 100),
                            degre = 1:4),
              kernel = "polynomial")
tune3

modelo3 <- svm(formula = species ~ .,
               data = treino,
               kernel = "polynomial",
               cost = 1,
               degre = 3)
plot(modelo3, treino)

previsao3 <- predict(modelo3, newdata = teste[,2:3])
mean(previsao3 == teste$species)

# Mais dimensões
treino2 <- dados[id,]
teste2 <- dados[-id,]
tune4 <- tune(svm,
              train.x = treino2[,2:5],
              train.y = treino2$species,
              ranges = list(cost = c(0.01, 0.1, 1 ,10, 100),
                            gamma = c(0.01, 0.1, 1, 2,3,4)),
              kernel = "radial")
tune4

previsao4 <- predict(tune4$best.model, newdata = teste2[,2:5])
mean(previsao4 == teste2$species)

tune5 <- tune(svm,
              train.x = treino2[,2:5],
              train.y = treino2$species,
              ranges = list(cost = c(0.01, 0.1, 1 ,10, 100),
                            degre = 1:4),
              kernel = "polynomial")
tune5

previsao5 <- predict(tune5$best.model, newdata = teste2[,2:5])
mean(previsao5 == teste2$species)