#Funcao sample
dado <- sample(x = 1:6, size = 1000000, replace = TRUE)

deu_1 <- dado == 1

sum(deu_1)
mean(deu_1)

#Soma de dois dados, e probabilidade de sair 7

set.seed(2133)

d1 <- sample(x = 1:6, size = 1000000, replace = TRUE)
d2 <- sample(x = 1:6, size = 1000000, replace = TRUE)

soma = d1 + d2
mean(soma)

soma_7 <- soma == 7

sum(soma_7)
mean(soma_7)


#Criacao de tabelas e graficos
tabela_soma <- table(soma)
barplot(tabela_soma)

