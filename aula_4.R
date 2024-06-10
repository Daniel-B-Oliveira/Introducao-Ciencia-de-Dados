install.packages("palmerpenguins")
library(palmerpenguins)

dados <- penguins
str(dados)

summary(dados)    #Analise estatistica

dados$sex

#Limpeza dos dados faltantes: 

is.na(dados$sex)            #Retorna TRUE para dados faltantes 

dados[is.na(dados$sex),]    #Retorna os pinguins com dados faltantes
dados[!is.na(dados$sex),]   #Retorna os pinguins com dados nao faltantes


dados <- dados[!is.na(dados$sex),]

#Grafico
tabela_sexo <- table(dados$sex)
barplot(tabela_sexo, main = "Frequência de machos e femeas", ylab = "Frequência absoluta")

#Separação por niveis

niveis <- levels(dados$species)                   #Niveis: "Adelie" "Chinstrap" "Gentoo" 

adelie      <- dados[dados$species == niveis[1],]
chinstrap   <- dados[dados$species == niveis[2],]
gentoo      <- dados[dados$species == niveis[3],]

#Storytelling

table(adelie$island)
table(chinstrap$island)
table(gentoo$island)

boxplot(adelie$body_mass_g, chinstrap$body_mass_g, gentoo$body_mass_g)
boxplot(adelie$bill_depth_mm, chinstrap$bill_depth_mm, gentoo$bill_depth_mm, names = c("Adelie", "Chinstrap", "Gentoo"))
