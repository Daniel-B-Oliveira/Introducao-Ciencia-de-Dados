#Dados coletados sobre pinguins

install.packages("palmerpenguins")
library(palmerpenguins)

dados <- penguins

dados[100,]   #Ou dados[100,1:8]
dados[100, c(1,7)]

ilha <- dados[,2]     #Ou dados[1:n,2], n = ultima linha, não recomendado

#Criacao de tabela sobre as ilhas
tabela_ilha <- table(ilha)
barplot(tabela_ilha)

#Maneira altenativa ao indice
dados$island
dados$sex
