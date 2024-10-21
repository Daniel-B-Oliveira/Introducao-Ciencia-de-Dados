library(quanteda)   #Analise quantitativa de texto
library(quanteda.textmodels)  #Modelos de classificação de textos
library(ggplot2)
library(dplyr)
library(tidytext)
library(SnowballC)

noticias <- read.csv("noticias.csv")
names(noticias)
str(noticias)

noticias$categorias <- as.factor(noticias$categorias)
unique(noticias$categorias)

#matriz documento frequência (dfm)

noticias |>
  ggplot(aes(x=categorias))+
  geom_bar()

noticias[1,] |>
  unnest_tokens(output = word, input = texto)

stopwords_pt <- data.frame(word = stopwords("pt"))

noticias[1,] |>
  unnest_tokens(output = word, input = texto)|>
  anti_join(stopwords_pt) |>
  count(word, sort = TRUE) |>
  top_n(20)

noticias[1,] |>
  unnest_tokens(output = word, input = texto)|>
  anti_join(stopwords_pt) |>
  mutate(word = wordStem(word, "portuguese"))|>   #Radicaliza as palavras
  count(word, sort = TRUE) |>
  top_n(20)

#Modelo Naive Bayes

# P(A|B,C) = P(A,B,C)/P(B,C) = [P(A).P(B|A).P(C|A,B)]/P(B,C) ~= [P(A).P(B|A).P(C|A)]/P(B,C)


tokens_noticias <- tokens(noticias$texto,
                          remove_punct = TRUE,
                          remove_numbers = TRUE,
                          remove_symbols = TRUE,
                          remove_url = TRUE) |>
  tokens_remove(stopwords("pt")) |>
  tokens_wordstem("portuguese")

tokens_noticias

matriz_frequencia <- dfm(tokens_noticias)
matriz_frequencia

n <- round(0.8*nrow(noticias))






















