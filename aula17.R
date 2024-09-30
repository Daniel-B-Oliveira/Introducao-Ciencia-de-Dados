#Web Scraping

library(dplyr)
library(rvest)

url <- "https://www.bbc.com/portuguese/articles/c3dv8yy3d8jo"

html <- read_html(url)


#html_text2 mais estavel
html |>
  html_elements("h1") |>
  html_text2()

#Exercicio

url2 <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"

html2 <- read_html(url2)

html2 |>
  html_elements("h1")|>
  html_text2()

html2 |>
  html_elements("span.mw-page-title-main")|>
  html_text2()

#Equivalente
html_text2(html_elements(html2,"span.mw-page-title-main"))

tabela01 <- html2 |>
  html_element("table.wikitable")|>
  html_table()

tabela01

#Caso de errado
tabelas <- html2 |>
  html_elements("table")|>
  html_table()

tabelas
tabela02 <- tabelas[[3]]
tabela02

