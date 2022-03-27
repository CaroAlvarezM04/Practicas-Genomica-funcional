
series <- read.csv("gustos.csv")
row.names(series)<- series[,1]
series<-series[,-1]
matrizcor <- cor(t(series))
matrizad <- (1+matrizcor)/2
diag(matrizad)<-0

#sin pesar
matriz1 <- ifelse(matrizad>0.5,1,0)

#pesada
matriz2 <- (2+matrizcor)/1.5
diag(matriz2)<-0
matriz_2 <- ifelse(matriz2>0,1,2)

#REDES REGULATORIAS
library(BoolNet)

#para cargar una red en boolnet o red booleana 
#Operadores en R
#AND &
#OR |
#NOT !

# A, B, C
# A: A & !C
# B: A & C
# C: B

#escribirlo en R
#generar un archivo de texto plano
  #targets, factors
red_bul <- loadNetwork("mi_red.txt")

plotNetworkWiring(red_bul)
#no aparecen activaciones e inhibiciones 

#encontrar los atractores
atractores <- getAttractors(red_bul)
#te dice e tamaño de la cuenca de atraccion y todos los atractores

#dibujar atractores
plotAttractors(atractores)
#rojo inactivo verde activo
#cada barra para cada atractor 
#por ejemplo 000 y 100 
#te dice cuantos estados llegan a ese atractor
#tambien te da el vector en numeros en la consola
plotStateGraph(atractores)
#grafica rara la neta

#ciclo celular
data("cellcycle")
plotNetworkWiring(cellcycle)
atractores <- getAttractors(cellcycle)
atractores
plotAttractors(atractores)
plotStateGraph(atractores)

#encontrar el siguiente estado
stateTransition(cellcycle, rep(1,10))
#repite el 1 10 veces
getTransitionTable(atractores)

#cuencas de atraccion
getBasinOfAttraction(atractores,1)
#1 es el atrator al que le calculas la cuenca

red <- loadSBML("MODEL1610060000_url.xml")
getAttractors(red)
red

##
sink("~/Desktop/testNet.txt")
cat("targets, factors\n")
cat("Gene1, !Gene2 | !Gene3\n")
cat("Gene2, Gene3 & Gene4\n")
cat("Gene3, Gene2 & !Gene1\n")
cat("Gene4, 1\n")
sink()
##

#exportar redes tipo igraph
save(red1, file = "red_que_voy_mandar.RDS")
remove(red1)
load("red1_que_voy_mandar.RDS")

install.packages("RCy3")
library(RCy3)

#abrir cytoscape y esto lo liga
cytoscapePing()
#lo crea en igraph
createNetworkFromIgraph(red_amikos, "myIgraph")
