library(quanteda)   #Analise quantitativa de texto
library(quanteda.textmodels)  #Modelos de classificação de textos
library(ggplot2)
library(dplyr)
library(tidytext)
library(SnowballC)

noticias <- read.csv("noticias.csv")
noticias$categorias <- as.factor(noticias$categorias)

tokensNoticias <- tokens(noticias$texto,
                          remove_punct = TRUE,
                          remove_numbers = TRUE,
                          remove_symbols = TRUE,
                          remove_url = TRUE) |>
  tokens_remove(stopwords("pt")) |>
  tokens_wordstem("portuguese")

matrizFrequencia <- dfm(tokensNoticias)
matrizFrequencia

str(matrizFrequencia)

matrizFrequencia[matrizFrequencia@Dimnames$docs == "text1",]
matrizFrequencia[,matrizFrequencia@Dimnames$features == "fim"]

matrizFrequencia@docvars$docname_ <- noticias$categorias

sum(matrizFrequencia@Dimnames$docs != "economia")
