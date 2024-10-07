#Continuação raspagem de dados

library(dplyr)
library(rvest)
library(ggplot2)

url <- "https://www.bbc.com/portuguese/articles/c3e9p3dq0vgo"

html <- read_html(url)
html

titulo <- html |>
  html_element("h1")|>
  html_text2()

texto <- html |>
  html_elements("p.bbc-hhl7in") |>
  html_text2()

texto <- paste(texto, collapse = " ")

noticias <- data.frame(titulo, texto)

#Pegando mais de uma notícia

url_bbc <- "https://www.bbc.com/portuguese/topics/cr50y580rjxt?page=1"

html_bbc <- read_html(url_bbc)

links <- html_bbc |>
  html_elements("ul.bbc-k6wdzo") |>
  html_elements("a.bbc-uk8dsi") |>
  html_attr("href")

titulos <- vector()
textos <- vector()

for(j in 1:length(links)){
  url <- links[j]
  html <- read_html(url)
  
  titulos[j] <- html |>
    html_elements("h1") |>
    html_text2()
  
  texto <- html |>
    html_elements("p.bbc-hhl7in")|>
    html_text2()
  
  textos[j] <- paste(texto, collapse = " ")
}

titulos[8]
textos[8]

noticias <- data.frame(titulos, textos)
View(noticias)

noticias <- noticias |>
  mutate(categoria=rep("ciencia", nrow(noticias)))

#trabalho: Coletar noticias das 40 paginas
#verificar se são as mesma notícia

library(tidytext)

noticias[1,] |>
  unnest_tokens(output = "word", input = "textos") |>
  select(word) |>
  count(word, sort=TRUE)|>
  top_n(10)

library(stopwords)
?stopwords
stopwords(language = "pt")

stopwords_pt <- data.frame(word = c(stopwords("pt"), "é"))

noticias[3,] |>
  unnest_tokens(output = "word", input = "textos") |>
  select(word) |>
  anti_join(stopwords_pt) |>
  count(word, sort=TRUE) |>
  top_n(10)|>
  mutate(word = reorder(word, n))|>
  ggplot(aes(y=word, x=n))+
  geom_col()+
  theme_minimal()

?anti_join
