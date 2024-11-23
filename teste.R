library(dplyr)
library(purrr)
#Tentativa Modelo Naive Bayes

# P(A|B,C) = P(A,B,C)/P(B,C) ~= [P(A).P(B|A).P(C|A)]/P(B,C)

f <- function(x, y){
  #Agrupando a classe com a freqência da ocorrência das palavras(x)
  dados <- as.data.frame(x)
  dados$cl <- y
  dados$prob <- rep(1, times=nrow(dados))
  
  #Sumando todas as linhas que possuem a mesma classe
  dadosPory <- aggregate(.~cl, data = dados, FUN = sum)
  
  #Tranfomando a coluna prob na probabilidade daquela classe ocorrer
  dadosPory$prob <- dadosPory$prob/nrow(x)
  
  # Calcular o total de palavras por classe
  totalPory <- dadosPory |>
    rowwise() |>
    summarise(total = sum(c_across(-c(cl, prob)))) |>
    pull(total)
  
  # Calcular prob condicional
  dadosPory <- dadosPory |>
    mutate(across(-c(cl, prob), ~ . / totalPory[row_number()]))
  
  return(dadosPory)
}

#Gerando um conjunto de dados artificiais para testes
linha <- 12
coluna <- 100

classes <- sample(c("A", "B", "C", "D"), linha, TRUE)
classes <- as.factor(classes)

frequenciaW <- matrix(sample(0:10,linha*coluna,TRUE),
                      linha,
                      coluna,
                      TRUE)

teste <- f(frequenciaW, classes)
teste




