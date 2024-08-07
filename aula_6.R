#NA: not available
a <- c(2, 3, 5, NA)
mean(a, na.rm = TRUE)

#Funções
bilhete <- c(10, 20, 30, 40, 50, 60)

length(bilhete)
max(bilhete)
min(bilhete)
mean(bilhete)

#summary(bilhete)

sorteio <- sample(x = 1:60, size = 6, replace = FALSE)

sum(bilhete %in% sorteio)

#Laços
#For
soma <- 0

#sum(1:20)
for(i in 1:20){
  soma <- soma + i
}
soma

medias <- c()
for(i in 1:5000){
  #media <- mean(sample(x = 1:6, size = 1000, replace = TRUE))
  #medias <- c(medias, media)
  
  medias[i] <- mean(sample(x = 1:6, size = 1000, replace = TRUE))
}

hist(medias)
mean(medias)

#While
soma <- 0
i <- 1
while(soma < 1000){
  soma <- soma + i
  i <- i + 1
}
soma
i

#Mega Sena
bilhete <- c(10, 20, 30, 40, 50, 60)
semanas <- 0
acertos <- 0

while(acertos < 4){
  semanas <- semanas + 1
  
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
  acertos <- sum(bilhete %in% sorteio)
}
semanas/52
acertos