#CENTRALIDAD

library("igraph")
r1 <- make_empty_graph(n=10, directed=FALSE)
r1 <- add.edges(r1, c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 6,8, 6,9, 6,10))
plot(r1)

#medidas de cetralidad
centralization.degree(r1)
centr_degree(r1)
centralization.betweenness(r1)
centr_betw(r1)
centralization.closeness(r1)

#red de proteinas de levaduas
install.packages("igraphdata")
library("igraphdata")
data(package="igraphdata")
data("yeast")

closeness(yeast)
centr_clo(yeast)
centr_betw(yeast)
centr_degree(yeast)
centr_eigen(yeast)
centr_degree_tmax(yeast)
#METODOS DE CLUSTERIZACION
decreasin

data("karate")
#clusterizar
eb <- cluster_edge_betweenness(karate, directed = FALSE)
membership(eb) #para ver que pertenece a cada grupo
table(membership(eb)) #para ver cuantos hay en cada grupo
#red sin clusters
plot(karate)
#plot de red con clusterizacion
plot(eb, karate)

#otro metodo
fg <- cluster_fast_greedy(karate)
membership(fg)
table(membership(fg))
plot(fg, karate)

#comparar metodos
compare(eb, fg, method = "adjusted.rand")

##
info <- cluster_infomap(karate)
membership(info)
plot(info, karate)

lp <- cluster_label_prop(karate)
membership(lp)
plot(lp, karate)

le <- cluster_leading_eigen(karate)
membership(le)
plot(le, karate)

op <- cluster_optimal(karate)
membership(op)
plot(op, karate)

##
uno <- compare(info, lp, method = "adjusted.rand")
dos <- compare(info, le, method = "adjusted.rand")
tres <-compare(info, op, method = "adjusted.rand")
cuatro <- compare(info, eb, method = "adjusted.rand")
cinco <- compare(info, fg, method = "adjusted.rand")
seis <- compare(lp, le, method = "adjusted.rand")
siete <- compare(lp, op, method = "adjusted.rand")
ocho <- compare(lp, eb, method = "adjusted.rand")
nueve <- compare(lp, fg, method = "adjusted.rand")
diez <- compare(le, op, method = "adjusted.rand")
once <- compare(le, eb, method = "adjusted.rand")
doce <- compare(le, fg, method = "adjusted.rand")
trece <- compare(eb, fg, method = "adjusted.rand")

matriz <- c(uno, dos, tres, cuatro, cinco, seis, siete, ocho, nueve, diez, once, doce, trece)
matriz
metodos <- c("info/lp","info/le","info/op",
             "info/eb","info/fg",
             "lp/le","lp/op","lp/eb","lp/fg",
             "le/op","le/eb","le/fg","eb/fg")
names(matriz)<- metodos
as.matrix(matriz)

#mundo ultrapequeño
#calcula el logaritmo de 10000, 100000, y 1000000

#calcula el logaritmo del logaritmo

#en igraph crea una red de 10000 nodos y de 100000 con free-scale, aleatoria y small-worls
red1 <- random.graph.game(10000, )
red2 <- barabasi.game(10000)
red3 <- sample_smallworld(10000)
#calcula el diametro y distancias promedio de cada red

#usa la diapo para calcular 
distances(yeast)
mean_distance(yeast)
shortest.paths(yeast)
distance_table(yeast)
distance(yeast)


direction <- function(){
matriz <- read.csv("matriz.csv")
x <- matriz[2,3]
y <- matriz[3,2]
mul <- y*x
mul1 <- x*y
dirigida <- if(mul== mul1){
  print("Es no dirigida")
}else{print("Es dirigida")}
}

pesored <- function(){
matriz <- read.csv("matriz.csv")
pesada <- if(matriz==1){
  print("No es pesada")
}else{print("Es pesada")}
}

degreeR <- function(){
matriz <- read.csv("matriz.csv")
A <- matriz[1,-1]
B <- matriz[2,-1]
C <- matriz[3,-1]
D <- matriz[4,-1]
E <- matriz[5,-1]
eF <- matriz[6-1]
sumA <- sum(A)
sumB <- sum(B)
sumC <- sum(C)
sumD <- sum(D)
sumE <- sum(E)
sumF <- sum(eF)

deg <- c(sumA, sumB, sumC, sumD, sumE, sumF)
names(deg) <- c("A","B","C","D","E","F")
histdeg <- hist(deg)
}
