library(palmerpenguins)
library(ggplot2)
library(dplyr)

#Arvore de decisão

dados <- penguins
summary(dados)
dados <- na.omit(dados)

n <- round(nrow(dados) * 0.8)
i_treinamento <- sample(1:nrow(dados), n, FALSE)

treinamento <- dados[i_treinamento,]
teste <- dados[-i_treinamento,]

names(treinamento)
ggplot(treinamento, aes(x = flipper_length_mm, y = bill_length_mm, col = species))+
  geom_point(alpha = 0.8)

ggplot(treinamento, aes(x = flipper_length_mm, y = bill_length_mm, col = species))+
  geom_point(alpha = 0.8)+
  facet_wrap(~sex)

ggplot(treinamento, aes(x = body_mass_g, y = bill_length_mm, col = species))+
  geom_point(alpha = 0.8)+
  facet_wrap(~sex)

ggplot(treinamento, aes(x =  body_mass_g, y = bill_length_mm, col = species))+
  geom_point(alpha = 0.8)+
  facet_wrap(~sex)

treinamento |>
  filter(sex == "female")|>
  ggplot(aes(x =  body_mass_g, y = bill_length_mm, col = species))+
  geom_point(alpha = 0.8)+
  geom_vline(xintercept = 4000)+
  geom_hline(yintercept = 42)

treinamento |>
  filter(sex == "male")|>
  ggplot(aes(x =  body_mass_g, y = bill_length_mm, col = species))+
  geom_point(alpha = 0.8)+
  geom_vline(xintercept = 4850)+
  geom_hline(yintercept = 46)

#Sex == Female?
# Sim:
#   Peso > 4000?
#     maior que 4000?
#       sim: Gentoo
#       não:
#         Bico < 42?
#           Sim: Adelie
#           Não: Chinstrap
# Não:
#   peso > 4850?
#     Sim: Gentoo
#     Não:
#       Bico < 46?
#         Sim: Adelie
#         Não: Chinstrap

classificacao <- c()

for(k in 1:nrow(teste)){
  if(teste$sex[k] == "female"){
    if(teste$body_mass_g[k] > 4000){
      classificacao[k] <- "Gentoo"
    }else{
      if(teste$bill_length_mm[k] < 42){
        classificacao[k] <- "Adelie"
      }else{
        classificacao[k] <- "Chinstrap"
      }
    }
  }else{
    if(teste$body_mass_g[k] > 4850){
      classificacao[k] <- "Gentoo"
    }else{
      if(teste$bill_length_mm[k] < 46){
        classificacao[k] <- "Adelie"
      }else{
        classificacao[k] <-"Chinstrap"
      }
    }
  }
}

mean(classificacao == teste$species)
