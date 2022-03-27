install.packages("igraph")
library(igraph)
install.packages("igraphdata")
library("igraphdata")

#Crea una red vacia de 10 nodos
red1 <- make_empty_graph(n=10, directed=TRUE)
#agregar caracteristicas a los vectores
V(red1)$color = "purple"
V(red1)$shape = "sphere"

#para visualizar lo que hiciste
plot(red1)

#añadir conexiones entre los nodos
red1 <- add.edges(red1, c(1,10, 1,2, 1,4, 1,5, 1,6, 1,7, 1,8, 1,9))
plot(red1)

#agregar nodos diferentes a los que tienes
red1 <- add.vertices(red1, 1, color="yellow", shape="sphere")
plot(red1)

#agregar o cambiar las conexiones
red1 <- add.edges(red1, c(3,6, 6,5))
plot(red1)

#para saber de que clase es tu objeto
class(red1)

#reemplazar una conexion por otra
red1 <- delete.edges(red1, c(2)) #quitar la conexion que se tenia
red1 <- add.edges(red1, c(4,2))
plot(red1)

#para identificar los nodos con letras en lugar de numeros
V(red1)$name <- LETTERS[1:12]
V(red1)
plot(red1)

#calcular el degree de la red
degree(red1) #numero de conexiones de cada nodo

#para hacer evidente diferencias entre e numero de conexiones de cada nodo
plot(red1, layout=layout_nicely, vertex.size=degree(red1, V(red1), "in")*10+10,
     vertex.label.dist=0.2, edge.arrow.size=0.2)

#para encontrar y graficar la distribución del degree
plot(degree_distribution(red1), main="Degree distribution", xlab="Degree", ylab="Frequency")
#un plot donde en y esta la frecuencia y en x el degree de cada nodo

#se puede representar el degree a manera de histograma
hist(degree(red1),col="purple")

#para la matriz de adyacencia 
adjM<-as.matrix(get.adjacency(red1))
adjM

#________________________
    ### RED DE AMIGOS ###

#leer el archivo

amikos <- read_csv("amikos.csv")
#Ponemos nombres a los renglones
row.names(amikos)<-amikos$...1
amikos<-amikos[,-1]
#Quitamos los datos con NA
amikos<-amikos[-2,]
amikos<-amikos[,-2]

amikos<-as.matrix(amikos)

library(igraph)

#Cargar la matriz de adyacencia

red_amikos<-graph_from_adjacency_matrix(amikos,mode="directed")
amikos_in<-degree(red_amikos,mode="in")
amikos_in
sort(amikos_in, decreasing = TRUE)

amikos_out<-degree(red_amikos,mode="out")
sort(amikos_out, decreasing = TRUE)

#Promedio de conectividades
mean(amikos_in)
mean(amikos_out)

#Distribución del degree
hist(amikos_in)
hist(amikos_out)

V(red_amikos)$size <- degree(red_amikos,mode = "in")*3
plot(red_amikos,edge.arrow.size=.4, edge.curved=.4)

#MIS AMIKOS INCLUYEN
#erick, mitzy, enrique, maria, mafer, ernesto, ceci, rebeca, andrea
amix_names <- c("Erick","Mitzy", "Enrique","Maria","Fernanda","Ernesto","Cecilia","Rebeca","Andrea")
num_amix <- c(7,11,8,10,12,10,9,9,9)
names(num_amix)<- amix_names

mean(num_amix) #promedio de amigos que tienen mis amigos

#WTF
posiciones <- which(amikos["ERNESTO",]==1)
posiciones

wc <- cluster_walktrap(red_amikos)
members <- membership(wc)

plot(wc,red_amikos)
