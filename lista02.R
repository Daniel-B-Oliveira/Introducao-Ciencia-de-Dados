library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)

# Questão 1
pacientes <- read.table("diabetes.txt", TRUE, ";")

str(pacientes)
pacientes <- pacientes[,-1]
pacientes$Diabetic <- as.factor(pacientes$Diabetic)

n <- round(0.8*nrow(pacientes))
id <- sample(1:nrow(pacientes), n, FALSE)

treino <- pacientes[id,]
teste <- pacientes[-id,]

# a)

ggplot(treino, aes(Pregnancies, fill=Diabetic))+
  geom_bar()

# Grande parte dos diabéticos possuem Pregnancies acima de 1

ggplot(treino, aes(Age, fill=Diabetic))+
  geom_histogram(bins = 8)

ggplot(treino, aes(Diabetic, PlasmaGlucose))+
  geom_boxplot()

# 75% dos diabéticos possuem um indice de PlasmaGlucose maior que 50% dos não diabéticos

ggplot(treino, aes(Diabetic, DiastolicBloodPressure))+
  geom_boxplot()

# Os não diabéticos possuem uma maior variação do DiastolicBloodPressure

ggplot(treino, aes(Diabetic, TricepsThickness))+
  geom_boxplot()

# 25% dos não diabéticos possuem menos TricepsThickness que todos os diabéticos

ggplot(treino, aes(Diabetic, SerumInsulin))+  #
  geom_boxplot()

# 25% dos diabéticos possuem mais SerumInsulin que 75% dos não diabéticos

ggplot(treino, aes(Diabetic, BMI))+ #
  geom_boxplot()

# Os não diabéticos possuem maior variação do BMI
# 75% dos diabéticos possuem um BMI maior que 50% dos não diabéticos

ggplot(treino, aes(Diabetic, DiabetesPedigree))+ 
  geom_boxplot()

# 25% dos diabéticos possuem mais DiabetesPedigree que 75% dos diabéticos

# Grande parte dos diabéticos apresenta um número de gestações (Pregnancies)
# acima de 1, e 75% deles possuem um índice de glicose plasmática (PlasmaGlucose)
# maior do que o nível médio de 50% dos não diabéticos. Por outro lado, os não
# diabéticos demonstram maior variação na pressão arterial diastólica
# (DiastolicBloodPressure) e 25% deles apresentam menor espessura de tríceps
# (TricepsThickness) do que todos os diabéticos. Além disso, 25% dos diabéticos
# possuem níveis de insulina sérica (SerumInsulin) superiores aos observados em
# 75% dos não diabéticos. A variação do índice de massa corporal (BMI) é maior
# nos não diabéticos, mas 75% dos diabéticos têm um BMI maior que o nível médio
# de 50% dos não diabéticos. Por fim, 25% dos diabéticos apresentam valores mais
# altos de DiabetesPedigree do que 75% dos próprios diabéticos.

# b)

acuraciaModelos <- data.frame(modelo=vector(), geral=vector(), condicional=vector())

modeloArvore <- rpart(Diabetic~., treino, method = "class")
rpart.plot(modeloArvore, extra=101)

classifica <- function(pregnancies, BMI, serumInsulin, age) {
  if(pregnancies < 2) {
    return(0)
  }else{
    if(BMI < 22){
      return(0)
    }else{
      if(serumInsulin >= 52){
        return(1)
      }else{
        if(age < 36){
          return(0)
        }else{
          return(1)
        }
      }
    }
  }
}

previsaoArvore <- vector()

for(i in 1:nrow(teste)){
  previsaoArvore[i] <- classifica(teste$Pregnancies[i],
                           teste$BMI[i],
                           teste$SerumInsulin[i],
                           teste$Age[i])
}

acuraciaModelos[1,1] <- "Arvore"
acuraciaModelos[1,2] <- mean(previsaoArvore == teste$Diabetic)

# c)

?randomForest
modeloFloresta <- randomForest(Diabetic~., treino)
previsaoFloresta <- predict(modeloFloresta, newdata = teste, type = "class")


acuraciaModelos[2,1] <- "Floresta"
acuraciaModelos[2,2] <- mean(previsaoFloresta == teste$Diabetic)

# d)

tuneLi <- tune(svm,
                 train.x = treino[,-9],
                 train.y = treino$Diabetic,
                 kernel = "linear",
                 rages = list(cost = c(0.01, 0.1, 1 ,2, 5, 10, 100)))

previsaoSVM_li <- predict(tuneLi$best.model, newdata=teste[,-9])

acuraciaModelos[3,1] <- "Svm Linear"
acuraciaModelos[3,2] <- mean(previsaoSVM_li == teste$Diabetic)

tunePoly <- tune(svm,
                 train.x = treino[,-9],
                 train.y = treino$Diabetic,
                 kernel = "polynomial",
                 rages = list(cost = c(0.01, 0.1, 1 ,10, 100),
                              degre = 1:4))

previsaoSVM_poly <- predict(tunePoly$best.model, newdata = teste[,-9])

acuraciaModelos[4,1] <- "Svm Polinomial"
acuraciaModelos[4,2] <- mean(teste$Diabetic == previsaoSVM_poly)

# Tune para núcleo radial não estava tendo êxito ao compilar

modeloSVM_rad <- svm(Diabetic~.,
                     data = treino,
                     kernel = "radial",
                     cost = 5,
                     gamma = 2)

previsaoSVM_rad <- predict(modeloSVM_rad, teste)

acuraciaModelos[5,1] <- "Svm Radial"
acuraciaModelos[5,2] <- mean(previsaoSVM_rad == teste$Diabetic)

# e)

sum(teste$Diabetic == "1")

table(teste$Diabetic, previsaoArvore)
acuraciaModelos[1,3] <- 842 / 1027

table(teste$Diabetic, previsaoFloresta)
acuraciaModelos[2,3] <- 911 / 1027

table(teste$Diabetic, previsaoSVM_li)
acuraciaModelos[3,3] <- 620 / 1027

table(teste$Diabetic, previsaoSVM_poly)
acuraciaModelos[4,3] <- 783 / 1027 

table(teste$Diabetic, previsaoSVM_rad)
acuraciaModelos[5,3] <- 783 / 1027

# f)
acuraciaModelos

# Em relação aos modelos de SVM, o que se sai melhor foi o Radial. Considerando
# todos os modelos, o que possui melhor precisão é o modelo Floresta Aleatória.

# Questão 2
olive <- read.table("olive.txt", T, sep=",")

str(olive)
unique(olive$region)

olive$region <- as.factor(olive$region)
summary(olive)

#a)
dados_padronizado <- scale(olive[,-1])
summary(dados_padronizado)

#b)
#k=3
modeloK3Means <- kmeans(dados_padronizado, centers = 3, nstart = 20 )
olive$cluster_k3 <- as.factor(modeloK3Means$cluster)

ggplot(olive, aes(x=cluster_k3, fill=region))+
  geom_bar()

# Os grupos 1 e 2 dividem bem alguns azeites da Southern Italy e
# Northern Italy, respectivamente. Com relação  ao grupo 3, esse é composto
# pelos três azeites. Dessa forma, por mais que azeites venham de regiões
# diferentes, ainda assim, aguns possuem semelhanças.

#c)
#k=4
modeloK4means <- kmeans(dados_padronizado, centers = 4, nstart = 20 )
olive$cluster_k4 <- as.factor(modeloK4means$cluster)

ggplot(olive, aes(x=cluster_k4, fill=region))+
  geom_bar()

# Os 4 grupos dividem bem os azeites por região, sendo dois desses grupos (1 e 3)
# da mesma região Southern Italy, ou seja, há diferenças entre os azeites desse
# local. 

#k=5
modeloK5means <- kmeans(dados_padronizado, centers = 5, nstart = 20 )
olive$cluster_k5 <- as.factor(modeloK5means$cluster)

ggplot(olive, aes(x=cluster_k5, fill=region))+
  geom_bar()

# Os 5 grupos dividem bem os azeites por região, sendo o grupo 3 o único composto
# pela região da Sardinia, ou seja, é a região que possue maior semelhança entre
# seus azeites.