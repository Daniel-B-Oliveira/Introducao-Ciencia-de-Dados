dados <- read.table(file = "dados.txt", header = TRUE, sep = ';')
#colnames(dados): "Idade" "LocalDaMorte" "AnoDaMorte" "Genero"

str(dados)
summary(dados)
#Gráficos

tabela_idade    <- table(dados$Idade)         #table(dados[1,0])
tabela_local    <- table(dados$LocalDaMorte)
tabela_ano      <- table(dados$AnoDaMorte)
tabela_genero   <- table(dados$Genero)

#Idade
boxplot(dados$Idade, names = "Vítimas", ylab = "Idade das vítimas")

#Localização: "Hospital" "Nursing home" "Own home"
#             "Hospital" "Casa de repouso" "Casa própria"
barplot(tabela_local, main = "Local da morte", ylab = "Frequência absoluta", names.arg = c("Hospital", "Casa de repouso", "Casa própria"))

#Ano
barplot(tabela_ano, main = "Ano da morte", ylab = "Frequência absoluta")

#Gênero:  "Men" "Women"
#         "Homen" "Mulher"
barplot(tabela_genero, main = "Genero da vítima", ylab = "Frequência absoluta", names.arg = c("Homem", "Mulher"))

#Observações: Maioria das vítimas mulheres, queda dos assassinatos entre 1990 e 1992 e no ano de 1998, maior atuação em casa própria

#/////////////


# --- Dados de acordo com o gênero ---

dados_men     <- dados[dados$Genero == "Men",]
dados_women   <- dados[dados$Genero == "Women",]

#Idade
boxplot(dados_men$Idade, dados_women$Idade, names = c("Homem", "Mulher"), ylab = "Idade das vítimas")

#Localização

local_men     <- table(dados_men$LocalDaMorte,)
local_women   <- table(dados_women$LocalDaMorte)

barplot(local_men, main = "Local da morte", sub = "Vítimas homens", ylab = "Frequência absoluta", names.arg = c("Hospital", "Casa de repouso", "Casa própria"))
barplot(local_women, main = "Local da morte", sub = "Vítimas mulheres", ylab = "Frequência absoluta", names.arg = c("Hospital", "Casa de repouso", "Casa própria"))

#Ano
ano_men       <- table(dados_men$AnoDaMorte)
ano_women     <- table(dados_women$AnoDaMorte)

barplot(ano_men, tabela_ano, main = "Ano da morte", sub = "Vítimas homens",ylab = "Frequência absoluta")
barplot(ano_women, tabela_ano, main = "Ano da morte", sub = "Vítimas mulheres",ylab = "Frequência absoluta")

#Observações: Morte de homens aparente ser por oportunidade

# --- Dados entre 1990 e 1992 ---

dados_90_92 <- dados[dados$AnoDaMorte >= 1989 & dados$AnoDaMorte <= 1993,]
dados_90_92
tabela_ano_90_92 <- table(dados_90_92$AnoDaMorte)
barplot(tabela_ano_90_92)

#Obeservação: Matou 3 homens seguidos, porque?

# --- Sequência das mortes ---

which(dados$Genero == "Men")

#Em relação ao gênero
plot(dados$AnoDaMorte[dados$Genero == "Men"], which(dados$Genero == "Men"), main = "Ano da morte X Ordem da morte", sub = "Vítimas homens", xlab = "", ylab = "")
plot(dados$AnoDaMorte[dados$Genero == "Women"], which(dados$Genero == "Women"),  main = "Ano da morte X Ordem da morte", sub = "Vítimas mulheres", xlab = "", ylab = "")

#Em relacao ao local

which(dados$LocalDaMorte == "Own home")

plot(dados$AnoDaMorte[dados$LocalDaMorte == "Own home"], which(dados$LocalDaMorte == "Own home"), main = "Ano da morte X Ordem da morte", sub = "Vítimas em casa própria", xlab = "", ylab = "")
plot(dados$AnoDaMorte[dados$LocalDaMorte == "Nursing home"], which(dados$LocalDaMorte == "Nursing home"), main = "Ano da morte X Ordem da morte", sub = "Vítimas em casa de repouso", xlab = "", ylab = "")
plot(dados$AnoDaMorte[dados$LocalDaMorte == "Hospital"], which(dados$LocalDaMorte == "Hospital"), main = "Ano da morte X Ordem da morte", sub = "Vítimas em hospital", xlab = "", ylab = "")

# --- Dados em relação aos quartis de idade ---
quantile(dados$Idade)

dados_q1 <- dados[dados$Idade > 41 & dados$Idade <= 72,]
dados_q2 <- dados[dados$Idade > 72 & dados$Idade <= 77,]
dados_q3 <- dados[dados$Idade > 77 & dados$Idade <= 83,]
dados_q4 <- dados[dados$Idade > 83 & dados$Idade <= 93,]

#Localização
tabela_local_q1 <- table(dados_q1$LocalDaMorte)
tabela_local_q2 <- table(dados_q2$LocalDaMorte)
tabela_local_q3 <- table(dados_q3$LocalDaMorte)
tabela_local_q4 <- table(dados_q4$LocalDaMorte)

barplot(tabela_local_q1, main = "Local da morte", sub = "Vitimas entre 42 e 72 anos", ylab = "Frequência absoluta", names.arg = c("Hospital","Casa própria"))
barplot(tabela_local_q2, main = "Local da morte", sub = "Vitimas entre 73 e 77 anos", ylab = "Frequência absoluta", names.arg = c("Hospital", "Casa de repouso", "Casa própria"))
barplot(tabela_local_q3, main = "Local da morte", sub = "Vitimas entre 78 e 83 anos", ylab = "Frequência absoluta", names.arg = c("Hospital", "Casa própria"))
barplot(tabela_local_q4, main = "Local da morte", sub = "Vitimas entre 84 e 93 anos", ylab = "Frequência absoluta", names.arg = c("Hospital", "Casa de repouso", "Casa própria"))

#Ano
tabela_ano_q1 <- table(dados_q1$AnoDaMorte)
tabela_ano_q2 <- table(dados_q2$AnoDaMorte)
tabela_ano_q3 <- table(dados_q3$AnoDaMorte)
tabela_ano_q4 <- table(dados_q4$AnoDaMorte)

barplot(tabela_ano_q1, main = "Ano da morte", ylab = "Frequência absoluta", sub = "Vitimas entre 42 e 72 anos")
barplot(tabela_ano_q2, main = "Ano da morte", ylab = "Frequência absoluta", sub = "Vitimas entre 73 e 77 anos")
barplot(tabela_ano_q3, main = "Ano da morte", ylab = "Frequência absoluta", sub = "Vitimas entre 78 e 83 anos")
barplot(tabela_ano_q4, main = "Ano da morte", ylab = "Frequência absoluta", sub = "Vitimas entre 84 e 93 anos")

# --- Dados em relação aos quartis de ano ---
quantile(dados$AnoDaMorte)

dados_2_q1 <- dados[dados$AnoDaMorte > 1975 & dados$AnoDaMorte <= 1988,]
dados_2_q2 <- dados[dados$AnoDaMorte > 1988 & dados$AnoDaMorte <= 1995,]
dados_2_q3 <- dados[dados$AnoDaMorte > 1995 & dados$AnoDaMorte <= 1997,]
dados_2_q4 <- dados[dados$AnoDaMorte > 1997 & dados$AnoDaMorte <= 1998,]

#Local
tabela_local_2_q1 <- table(dados_2_q1$LocalDaMorte)
tabela_local_2_q2 <- table(dados_2_q2$LocalDaMorte)
tabela_local_2_q3 <- table(dados_2_q3$LocalDaMorte)
tabela_local_2_q4 <- table(dados_2_q4$LocalDaMorte)

barplot(tabela_local_2_q1, main = "Local da morte", sub = "Casos entre 1976 e 1988", names.arg = c("Hospital", "Casa de repouso", "Casa própria"), ylab = "Frequência absoluta")
barplot(tabela_local_2_q2, main = "Local da morte", sub = "Casos entre 1989 e 1995", names.arg = c("Hospital", "Casa de repouso", "Casa própria"), ylab = "Frequência absoluta")
barplot(tabela_local_2_q3, main = "Local da morte", sub = "Casos entre 1996 e 1997", names.arg = c("Hospital", "Casa de repouso", "Casa própria"), ylab = "Frequência absoluta")
barplot(tabela_local_2_q4, main = "Local da morte", sub = "Casos em 1998", names.arg = "Casa própria", ylab = "Frequência absoluta")

#Proporção local
prop_hos_q1 <- length(dados_2_q1$LocalDaMorte[dados_2_q1$LocalDaMorte == "Hospital"]) / length(dados_2_q1$LocalDaMorte)
prop_hos_q2 <- length(dados_2_q2$LocalDaMorte[dados_2_q2$LocalDaMorte == "Hospital"]) / length(dados_2_q2$LocalDaMorte)
prop_hos_q3 <- length(dados_2_q3$LocalDaMorte[dados_2_q3$LocalDaMorte == "Hospital"]) / length(dados_2_q3$LocalDaMorte)
prop_hos_q4 <- length(dados_2_q4$LocalDaMorte[dados_2_q4$LocalDaMorte == "Hospital"]) / length(dados_2_q4$LocalDaMorte)

prop_hos = c(prop_hos_q1, prop_hos_q2, prop_hos_q3, prop_hos_q4)

barplot(prop_hos, main = "Proporção local da morte", sub = "Casos em hospitais",names.arg = c("1976-1988","1989-1995","1996-1997","1998"))

#Idade

boxplot(dados_2_q1$Idade, dados_2_q2$Idade, dados_2_q3$Idade, dados_2_q4$Idade, names = c("1976-1988","1989-1995","1996-1997","1998"), xlab = "Ano da morte", ylab = "Idade da vítima")
#Observação: anormalidade entre 89 e 95

#Genero
tabela_genero_2_q1 <- table(dados_2_q1$Genero)
tabela_genero_2_q2 <- table(dados_2_q2$Genero)
tabela_genero_2_q3 <- table(dados_2_q3$Genero)
tabela_genero_2_q4 <- table(dados_2_q4$Genero)

barplot(tabela_genero_2_q1, main = "Genero da vítima", sub = "Casos entre 1976 e 1988",ylab = "Frequência absoluta", names.arg = c("Homem", "Mulher"))
barplot(tabela_genero_2_q2, main = "Genero da vítima", sub = "Casos entre 1989 e 1995",ylab = "Frequência absoluta", names.arg = c("Homem", "Mulher"))
barplot(tabela_genero_2_q3, main = "Genero da vítima", sub = "Casos entre 1996 e 1997",ylab = "Frequência absoluta", names.arg = c("Homem", "Mulher"))
barplot(tabela_genero_2_q4, main = "Genero da vítima", sub = "Casos em 1998",ylab = "Frequência absoluta", names.arg = c("Homem", "Mulher"))

#Proporção genero
prop_women_q1 <- length(dados_2_q1$Genero[dados_2_q1$Genero == "Women"]) / length(dados_2_q1$Genero)
prop_women_q2 <- length(dados_2_q2$Genero[dados_2_q2$Genero == "Women"]) / length(dados_2_q2$Genero)
prop_women_q3 <- length(dados_2_q3$Genero[dados_2_q3$Genero == "Women"]) / length(dados_2_q3$Genero)
prop_women_q4 <- length(dados_2_q4$Genero[dados_2_q4$Genero == "Women"]) / length(dados_2_q4$Genero)

prop_women = c(prop_women_q1, prop_women_q2, prop_women_q3, prop_women_q4) 

barplot(prop_women, main = "Proporção gênero da vítima", sub = "Vítimas mulheres",names.arg = c("1976-1988","1989-1995","1996-1997","1998"))
 
