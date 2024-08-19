#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)

titanic <- read.table("titanic.txt", header = TRUE, sep = ',')
names(titanic)

str(titanic)
titanic <- titanic[,-1]

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

summary(titanic)


#Survived
survived_tab <- table(titanic$Survived)
barplot(survived_tab)

survived_prop <- prop.table(survived_tab)
barplot(survived_prop)

#Pclass
pclass_tab <- table(titanic$Pclass)
barplot(pclass_tab)

pclass_prop <- prop.table(pclass_tab)
barplot(pclass_prop)

#Age
hist(titanic$Age)
mean(titanic$Age, na.rm = TRUE)
sum(is.na(titanic$Age))
boxplot(titanic$Age)

plot(x = titanic$Age, y = titanic$Fare, type = 'p', pch = 16, main = "grafico", xlab = "Idade", ylab = "Preço da passagem")

#Usando o dplyr
select(titanic, Age, Pclass)

titanic |> 
  select(Age, Pclass) |>
  filter(Pclass == 3)

#filter(select(titanic, Age, Pclass), Pclass == 3)

#Aprendendo ggplot

#https://colorbrewer2.org/
#https://www.color-hex.com/

ggplot(data = titanic, mapping = aes(x = Survived, fill = Sex)) +
  geom_bar() +
  theme_minimal()

ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  geom_bar() + 
  #theme_minimal() +
  facet_wrap(~Sex)   # a ~ b : a em funcão de b

titanic |>
  filter(Age <= 1) |>
  select(Age, Survived)
