#aula 2: Vetores

x <- c(3,12,43,567)
q <- c(1,21,34,123)

x + q
x + 10

x + c(10,20)      #Reciclagem: 
                  #preencher o vetor maior com o menor
                  #Os vetores devem ser multiplos

c(1,2,'3')        #Vetores sempre possuiem elementos da mesma classe
c(1,2,TRUE)

c(21,20,22,NA)    #NA: not avaliable
                  #NoN: not a number

#Acessar elementos de vetores

u <- c(11,22,33,44,55,66)
u[2]
u[c(2,4)]

menor_30 <- u < 30

u[u<30]
# ==
u[c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)]

u[u < 30 & u > 20]           #Seleciona endereços que atendem a condicao

which(u<30)     #Retorna o endereço
which(c(TRUE,FALSE,TRUE))

length(u)       #Retorna o tamanho do vetor
length(which(u<30))

sum(u<30)       #Soma de vetores

#Vetores sequenciais
v1 <- 100:557
