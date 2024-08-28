library(palmerpenguins)
library(ggplot2)
library(dplyr)
#Modelo KNN

pinguins <- penguins

str(pinguins)
summary(pinguins)

pinguins <- na.omit(pinguins)

#Modelagem do treinamento
pinguins <- pinguins |>
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

n <- round(nrow(pinguins) * 0.8)

indices_treinamento <- sample(1:nrow(pinguins), size = n, replace = FALSE)

treinamento <- pinguins[indices_treinamento,]
teste <- pinguins[-indices_treinamento,]


ggplot(treinamento, aes(x = bill_length_mm, y = bill_depth_mm, col = species))+
  geom_point(alpha = 0.8)

ggplot(treinamento, aes(x = bill_length_mm, y = flipper_length_mm, col = species))+
  geom_point(alpha = 0.8)   #Escolhido

#ggplot(treinamento, aes(x = bill_depth_mm, y = flipper_length_mm, col = species))+
  #geom_point(alpha = 0.8)


classificacao <- c()

for(j in 1:nrow(teste)){
  distancias <- c()
  for(i in 1:nrow(treinamento)){
    distancias[i] <- sqrt((treinamento$bill_length_mm[i] - teste$bill_length_mm[j])**2 + (treinamento$flipper_length_mm[i] - teste$flipper_length_mm[j])**2)
  }
  #?order
  classificacao[j] <- as.character(treinamento$species[order(distancias)[1]])
  #teste$species[1]
}

classificacao
mean(classificacao == teste$species)
