#problema dos aniversários:
#Probabilidade de ao menos duas pessoas em uma sala com 10 fazerem aniversario no mesmo dia 
#Igual a: 1 - probabilidade de niguem fazer aniversario no mesmo dia
# 1 - c(365,10) / 365^10



prob_aniversario <- function(n){
  
  resultados <- c()
  
  for(i in 1:100000){
    aniversarios <- sample(x = 1:365, size = n, replace = TRUE)
    resultados[i] <- any(duplicated(aniversarios))
  }
  return(mean(resultados))  
}

a <- prob_aniversario(366)

getwd()
