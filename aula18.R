#http://www.atlasbrasil.org.br/#home

library(readxl)
library(geobr)
library(dplyr)
library(ggplot2)

anaf <- read_excel("data.xlsx")
View(anaf)

#Anafalbetismo
anaf <- anaf[-c(1,29:31),]
anaf

#Codigo minucipio
geobr::lookup_muni("uberlandia")$code_muni
rio <- read_municipality(code_muni = 3304557)

rio |>
  ggplot()+
  geom_sf()+
  theme_void() #Retirar latitude longitude

geobr::lookup_muni("rio de janeiro")$code_muni
ubrl <- read_municipality(code_muni = 3170206)

ubrl |>
  ggplot()+
  geom_sf()+
  theme_void()

#
MG <- read_state(code_state = "MG")

MG |>
  ggplot()+
  geom_sf()+
  theme_void()
#

estados <- read_state()
estados |>
  ggplot()+
  #geom_sf(fill='white', col='red')+
  geom_sf()+
  theme_void()


#Maneira menos eficiente
corretos <- anaf$Territorialidades %in% estados$name_state
sum(corretos)

estados$name_state[!corretos]
estados$name_state[c(11,18,19,23,24)] <- c("Rio Grande do Norte",
                                           "Espírito Santo",
                                           "Rio de Janeiro",
                                           "Rio Grande do Sul",
                                           "Mato Grosso do Sul")

estados$name_state
anaf$Territorialidades

order(estados$name_state) # Retorna a posicao do n-esimo elemento (menor para maior)

#Reordenando
estados <- estados[order(estados$name_state),]
anaf <- anaf[order(anaf$Territorialidades),]


names(estados) 
estados$taxa_anaf <- anaf$`Taxa de analfabetismo - 25 anos ou mais de idade 2021`

estados |>
  ggplot()+
  geom_sf(aes(fill=taxa_anaf))+
  theme_void()+
  scale_fill_gradient(
    high = "#132B43",
    low = "#56B1F7"
  )

?scale_fill_gradient

#Dados abertos MG (pesquisar)
