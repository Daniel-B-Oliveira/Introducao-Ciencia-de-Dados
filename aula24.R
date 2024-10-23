#Apredizado não-supervisionado
#O modelo k-means

#Aglomerados hierárqucos (outro modeo) (dendograma)

library(jpeg)
library(ggplot2)

imagem <- readJPEG("imagem.jpg")
str(imagem)
#Não é preciso padronizar pois as imagens são RGB, ou seja, estão na mesma escala

R <- as.vector(imagem[,,1])
G <- as.vector(imagem[,,2])
B <- as.vector(imagem[,,3])

#coordenadas x e y
#imagem é percorrida por todas as linhas da coluna, até a  próxima coluna

X <- rep(1:259, each = 194)
Y <-rep(194:1, times=259)

dadosImagem <- data.frame(X,Y,R,G,B)

clusterizacao <- kmeans(x=dadosImagem[,3:5],
                        centers = 3,
                        nstart = 20)
?kmeans
dadosImagem$cluster <- as.factor(clusterizacao$cluster)

clusterizacao$centers
#grDevices
cores <- rgb(clusterizacao$centers)
cores

ggplot(dadosImagem, aes(X,Y,col=cluster))+
  geom_point(show.legend = FALSE)+
  scale_color_manual(values = cores)+
  theme_void()
