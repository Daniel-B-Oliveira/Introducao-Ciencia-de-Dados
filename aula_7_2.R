install.packages("palmerpenguins")
library(palmerpenguins)

dados <- penguins
str(dados)
summary(dados)

niveis1 <- levels(dados$species)
niveis2 <- levels(dados$island)

gentoo_dream <- dados[dados$species == niveis1[3] & dados$island == niveis2[2],]

mean(dados[,3], na.rm = TRUE)
mean(dados$bill_length_mm, na.rm = TRUE)
mean(dados$bill_length_mm[dados$species == "Adelie"], na.rm = TRUE)
