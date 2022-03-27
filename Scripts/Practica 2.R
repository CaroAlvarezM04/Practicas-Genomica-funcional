library(igraph)

#ejercicio diapositiva
mat <- read.csv("mat.csv") #cargar la matriz
mat <- as.matrix(mat[,-1]) #convertir a matriz y quitar los rownames
#hacer la red que es no dirigida
red_mat <- graph_from_adjacency_matrix(mat, mode = "undirected",weighted = NULL)
transitivity(red_mat) #calcular el coeficiente de clusterización
V(red_mat)$color = "purple" #poner los nodos morados
plot(red_mat) #para ver la red

#PRACTICA 1
#Crea una red random de 100 nodos
#0.5 corresponde a la probabilidad de que dos puntos se unan de manera arbitraria
g1<-random.graph.game(100,0.5)
plot(g1)

#genera una grafico de acuerdo al modelo de Barabasi
#100 nodos y no dirigida
g2<-barabasi.game(100,directed=FALSE)
#power es apego preferencia lineal
g <- barabasi.game(1000, power=1)

layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

#red random de 10 nodos con probabilidad de 0.5
g<-random.graph.game(10,0.5)
plot(g)
#calcular la distancia entre los nodos
distancia<-shortest.paths(g)
distancia #matriz de distancia
class(distancia) #clase del objeto es una matriz
heatmap(distancia) #heatmap de la distancia entre pares de nodos

#shortest_paths() caminos mas cortos entre vertices de redes dirigidas o no dirigidas
plot(g)
road<-shortest_paths(g, 1,3)
road 

#diametro de la red
diameter(g)
#hacer una red de anillo de 10 nodos
g <- make_ring(10)
plot(g)
#calcular las distancias entre nodos
distances(g) #matriz simetrica donde se ve un icremento y decremento en diagonal
#correspondiente a la distancia entre nodos 
shortest_paths(g,5)
#todos los caminos posibles para llegar del nodo 1 al 6 y 8
all_shortest_paths(g,1,6:8)
#media de la distancia
mean_distance(g)

## Redes ponderadas ??????
el <- matrix(nc=3, byrow=TRUE,
             c(1,2,0, 1,3,2, 1,4,1, 2,3,0, 2,5,5, 2,6,2, 3,2,1, 3,4,1,
               3,7,1, 4,3,0, 4,7,2, 5,6,2, 5,8,8, 6,3,2, 6,7,1, 6,9,1,
               6,10,3, 8,6,1, 8,9,1, 9,10,4) )
g3 <- add_edges(make_empty_graph(10), t(el[,1:2]), weight=el[,3])
plot(g3)
distances(g3, mode="out")


#Coeiciente de clusterización
g <- make_ring(10) #red anillo de 10 nodos
plot(g) #plot de red
transitivity(g) #coeficiente de clusterización
#crea un grafico completo?
#creo que se refiere a completamente dirigida
g1<-make_full_graph(5)
plot(g1)
transitivity(g1) #su coeficiente es 1

#Genere gráficos aleatorios según el modelo G(n,p) Erdos-Renyi
g2 <- sample_gnp(1000, 10/1000)
layout <- layout.fruchterman.reingold(g2)
plot(g2, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)
transitivity(g2)
gw <- graph_from_literal(A-B:C:D:E, B-C:D, C-D)
E(gw)$weight <- 1
E(gw)[ V(gw)[name == "A"] %--% V(gw)[name == "E" ] ]$weight <- 5
plot(gw,vertex.label=NA)

#coeficiente local
transitivity(gw, vids="A", type="local")
#coeficiente ponderado
transitivity(gw, vids="A", type="weighted")

# Ponderado se reduce a "local" si los pesos son los mismos
gw2 <- sample_gnp(1000, 10/1000)
E(gw2)$weight <- 1
t1 <- transitivity(gw2, type="local")
t2 <- transitivity(gw2, type="weighted")
layout <- layout.fruchterman.reingold(gw2)
plot(gw2, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)
all(is.na(t1) == is.na(t2))
all(na.omit(t1 == t2))

#EJERCICIOS
#1. Usando las redes calcula las distancias, medias de distancia,
  # el path de distancias y nodos y el coeficiente de clusterización
#2. Usando las siguientes redes calcula lo mismo de arriba btw
g1<-barabasi.game(100,directed = FALSE)
g2<-random.graph.game(100,0.20)
g3<-sample_smallworld(1,100,p=0.2,nei=3)

#

#_______________________________________
