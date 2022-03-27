#Practica 3
#La propiedad de escala libre

library(igraph)
#barabasi game da una red dirigida por eso se especifica si si o no
#como es no dirigida no necesita nada en degree
#hist para ver la distribución de conectividades
# generar una red de escala libre
g <- barabasi.game(1000) # con un exponete de 3 aprox
plot(g,vertex.label=NA)
#calcular el degree
d <- degree(g, mode="in")
hist(d) #histograma del degree
g <- delete.edges(g,c(sample(1:100)))
plot(g)
g
#Ajuste de una función de distribución de ley de potencia a datos discretos
fit1 <- fit_power_law(d+1, 10) #objeto que contiene el degree calculado
#implementation es un caracter escalar que puede ser R.mle o plfit
  #depende de que metodo uses para calcularlo 
fit2 <- fit_power_law(d+1, 10, implementation="R.mle") 
fit1$alpha #que mierda es esto
#alpha es el exponente xd
#la pendiente de la distribución
boxplot(d) #se ve una distribución de cola larga


#EJERCICIOS
#redes para trabajar los ejercicios
#barabasi game son distribuciones de cola larga
g10<-barabasi.game(10,directed = FALSE)
g100<-barabasi.game(100,directed = FALSE)
g1K<-barabasi.game(1000,directed = FALSE)
#los randomgraph son unimodales
g2<-random.graph.game(1000,0.20)
g3<-sample_smallworld(1,1000,p=0.2,nei=3)
#la Zachary es una red de karate y la interaccion de las personas fuera de
#se oberva que habia dos lideres de cada grupo 
#se usa para probar metodos o compararlos
g4<-make_graph("Zachary")

#para graficar como una escala logaritmica o linea letra
#plot(objeto grafica, log ="xy")

######
#ya contiene redes biologicas precargadas
install.packages("igraphdata")
library("igraphdata")

#Encontrar o calcular el degree y hacer un plot de la distribución
dg10 <- degree(g10)
plot(degree.distribution(g10))
dg100 <- degree(g100)
plot(degree.distribution(g100))
dg1k <- degree(g1K)
plot(degree.distribution(g1K))
dg2 <- degree(g2)
plot(degree.distribution(g2))
dg3 <- degree(g3)
plot(degree.distribution(g3))
dg4 <- degree(g4)
plot(degree.distribution(g4))

#Calcular la media y mediana y hacer un boxplot
media10 <- mean(dg10)
media100 <- mean(dg100)
mediak <- mean(dg1k)
media2 <- mean(dg2)
media3 <- mean(dg3)
media4 <- mean(dg4)
medias <- c(media10, media100, mediak, media2, media3, media4)
redes <- c("g10","g100","g1K", "g2","g3","g4")
names(medias)<-redes
medias
boxplot(medias)
boxplot(dg4)

m10 <- median(dg10)
m100 <- median(dg100)
mk <- median(dg1k)
m2 <- median(dg2)
m3 <- median(dg3)
m4 <- median(dg4)
medianas <- c(m10, m100, mk, m2, m3, m4)
names(medianas) <- redes
medianas
boxplot(medianas)

#Ajustar una ley de potencia a esas distribuciones y discutirlo

#Ajuste una distribución de ley de potencia a la classroom network
fit10 <- fit_power_law(dg10+1, 10)



g <- barabasi.game(1000)

g <- delete_vertices(g,sample(1:100,1))
plot(g)
g

delete.edges(g,sample(1:1000,5))

dg <- diameter(delete_vertices(g,sample(1:100,1)))

r1 <- barabasi.game(1000)
tam <- gorder(r1)

for(i in 1:tam/10){
  d1[i]<-diameter(r1)
  d2[i]<-diameter(r2)
  r1 <- delete.vertices(r1, sample(1:gorder(r1),1))
  r2 <- delete.vertices(r2, sample(1:gorder(r2),1))
}
library(igraph)
#Una red free-scale de 1000 nodos no dirigida
tam<-1000
r1<-barabasi.game(tam,directed=FALSE)
#una red aleatoria con el mismo nñumero de nodos y conexiones (no dirigida)
r2<-random.graph.game(tam,gsize(r1), type = "gnm")
#Declaro un vector vacío en odnde voy a guardar las distancia promedio
d1<-c()
d2<-c()
p<-0.20
for(i in 1:(tam*p)){
  d1[i]<-diameter(r1)
  d2[i]<-diameter(r2)
  r1<-delete_vertices(r1,sample(1:gorder(r1),1))
  r2<-delete_vertices(r2,sample(1:gorder(r2),1))
}





