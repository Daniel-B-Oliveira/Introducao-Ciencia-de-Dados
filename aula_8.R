#Titanic
dados <- read.table(file = "titanic.txt", header = TRUE, sep = ',')
dados <- dados[,-1]

structure(dados)
summary(dados)


#Arrumando o tipo dos dados
dados$Survived <- as.factor(dados$Survived)
dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)
dados$Embarked <- as.factor(dados$Embarked)

#Filtro passagem 3
pc_c3 <- dados[dados$Pclass == "3", -2]

summary(pc_c3)
tabela_pc_c3 <- table(pc_c3$Survived)
barplot(tabela_pc_c3)

#Filtro homem
males_c3 <- dados[dados$Sex == "male" & dados$Pclass == 3, -c(2,4)]
males_c1 <- dados[dados$Sex == "male" & dados$Pclass == 1, -c(2,4)]
females_c1 <- dados[dados$Sex == "female" & dados$Pclass == 1, -c(2,4)]
bebes <- dados[dados$Age < 1, ]

summary(males_c3)
summary(males_c1)
summary(females_c1)
summary(bebes)
