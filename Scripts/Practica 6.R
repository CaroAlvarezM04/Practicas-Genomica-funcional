BiocManager::install("affy")
BiocManager::install("mouse4302cdf")
BiocManager::install("pvclust")
BiocManager::install("vsn")
BiocManager::install("limma")
BiocManager::install("mouse4302.db")

library("affy")
library("mouse4301cdf")
library("pvclust")
library("vsn")
library("limma")
library("mouse4302.db")

#_____________________________________________

# PRACTICA 3 
#Análisis de microarreglos Affymetrix

#Cargar las siguientes librerias
library("affy")
library("pvclust")
library("vsn")

#PARTE 1: CARGAR LA INFORMACION DEL EXPERIMENTO 

# leer el archivo pdata.txt que contiene la relación entre el nombre de cada archivo y las características de cada muestra
pd = read.table("pdata.txt", header=TRUE, as.is=TRUE)
pd

#leer los datos contenidos en los archivos CEL usando la función ReadAffy
affyData = ReadAffy(filenames=pd$filename)

#Para agregar al archivo CEL lo que se guardo en el archivo pd
#accesar el PhenoData del archivo pd
pData(affyData) = pd

#Para que las muestras aparezcan con nombres cortos
sampleNames(affyData) = pd$name

#Revisar el objeto
affyData
#contiene información de 6 muestras, 45101 genes y es del chip mouse4302

#PARTE 2: ANALISIS DE CALIDAD

#Graficos para ver la expresion
boxplot(affyData, col=rainbow(6))
hist(affyData, col=rainbow(6))
#El proposito es ver si alguna muestra tiene un comportamiento anomalo respecto a las demas

#Para visualizar las intensidades directamente a como aparecen sobre el microarreglo
#Puede ser pesada, usar separacion de columna para ver cada una
image(affyData[,3])   # Calcula una imagen para la 3ra muestra

#Se espera que las replicas wt se correlacionen mas entre si que con las mutantes
#Para ver la correlacion se hace un heatmap
heatmap(cor(exprs(affyData)), symm=T)
#o con un dendograma
corClust = pvclust(exprs(affyData), nboot=1, method.dist="correlation")
plot(corClust)
#Analisis de componentes principales PCA
pca = princomp(exprs(affyData))
plot(pca$loadings, main="Principal Component Analysis", col=rainbow(6),  pch=19, cex=2)
text(pca$loadings, colnames(exprs(affyData)), pos=3, cex=0.8)

#PARTE 3: NORMALIZACION
#normalizar los microarreglos usando el algoritmo RMA (Robust Microarray Average)
eset = rma(affyData) #contiene los valores normalizados de expresion
eset

#Para ver métodos para normalizar disponibles en Bioconductor que puedes usar
normalize.AffyBatch.methods()

#PARTE 4: CALIDAD POSTNORMALIZACION
#Después de normalizar la distribución de los valores de expresión debe ser muy similar para todas las muestras
par(mfrow=c(1,2))    # Una misma ventana con multiples-figuras por "row", con 1 file y 2 columnas
boxplot(affyData, col=rainbow(6))
boxplot(data.frame(exprs(eset)), col=rainbow(6))
par(mfrow=c(1,1))    # Regresar a una figura por ventana

#Agrupamiento de muestras
par(mfrow=c(1,2))
corClust = pvclust(exprs(affyData), nboot=1, method.dist="correlation")
plot(corClust, main="Agrupamiento de muestras antes de normalizar")
corClustAfter = pvclust(exprs(eset), nboot=1, method.dist="correlation")
plot(corClustAfter, main="Agrupamiento de muestras despues de normalizar")
par(mfrow=c(1,1))

#Para ver si el normalizado puede controlar la variabilidad a lo largo del rango de valores de expresión
meanSdPlot(exprs(eset))

#Revisar los genes de manera individual para ver como se comportan
#es util si se tienen controles
boxplot(data.frame(exprs(eset)), col="grey");
lines(exprs(eset)["1428027_at",], lwd=2, type="b", col="red")
#te muestra el comportamiento esperado para esta sonda

#PARTE 5: GUARDAR LOS RESULTADOS
write.exprs(eset, file="expr_normalizada.txt") #como txt
save(eset,file="eset.Rdata") #como objeto R

#_________________________________________

