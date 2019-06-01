library(gridExtra)#Compare graph
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(vtreat) #'data.frame' statiscally procesor
library(broom) #Convert Statistical Analysis Objects into Tidy Tibbles
library(caret) #Calssificaton and regretion training
library(glmnet) #Lasso & Elastic - NEt Regularized Generalied models
library(Hmisc)#Correlation matrix
library(factoextra)
library(class)
library(corrplot)
library(stats)
#Cargar data
system("ls ../input")
data <- read_csv("/Users/claudiameneses/desktop/winedata.csv")
datos <- data.frame(data[c(2,3,4,5,6,7,8,9,10,11,12,13,14)])
View(datos)

#Resumen de data
summary(datos)

#Correlaciones
cor_matrix <- cor(datos, method = "pearson", use = "complete.obs")
cor_matrix
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#Estandarizar datos
scaled_datos <- scale(datos)

#Dispersión
pairs(scaled_datos,main="Dispersión",col="coral3",upper.panel = NULL)   

#PCA
acp<-prcomp(scaled_datos,center = TRUE, scale. = TRUE)
#proporción de la varianza y varianza acumulada
summary(acp)
acp$rotation
acp$x
CP1<-acp$x[,1]
CP2<-acp$x[,2]
plot(CP1,CP2,main="CP uno y dos",col="blueviolet", xlab = "Componente Principal 1", ylab = "Componente Principal 2")

#Representación bidimensional de las dos primeras componentes.
fviz_pca_biplot(acp, repel = FALSE, col.var = "mediumvioletred", col.ind = "#696969")
fviz_pca_var(acp, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#Gráfica de los individuos, con la clasificación dada
data <- data.frame(data)
fviz_pca_ind(acp, title="Clasificación por tipo", repel = FALSE, col.ind = data$Tipo,  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") , geom = "point")

#Cluster Análisis

#K-means clustering
km <- kmeans(scaled_datos, 3)
fviz_cluster(km, datos, ellipse.type = "norm")
#PCA
kmacp2 <- kmeans(scaled_datos ,2)
k2 <- fviz_cluster(kmacp2, scaled_datos, geom = "point", ellipse = FALSE, ggtitle="k=2")
kmacp3 <- kmeans(scaled_datos , 3)
k3 <- fviz_cluster(kmacp3, scaled_datos, geom = "point", ellipse = FALSE, ggtitle="k=3")
kmacp4 <- kmeans(scaled_datos ,4)
k4 <- fviz_cluster(kmacp4, scaled_datos, geom = "point", ellipse = FALSE, ggtitle="k=4")
kmacp7 <- kmeans(scaled_datos ,7)
k7 <- fviz_cluster(kmacp7, scaled_datos, geom = "point", ellipse = FALSE, ggtitle="k=7")
grid.arrange(k2, k3, k4,k7, nrow = 2)

#Comparo la clasificación dada con la obtenida con k-means clustering
p1 <- fviz_cluster(kmacp3, scaled_datos, geom = "point", ellipse = FALSE,  ggtitle = "k-means clustering", ggtheme = theme_minimal()) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 ) 
p2 <- fviz_pca_ind(acp, ggtitle="Clasificación dada", repel = FALSE, col.ind = data$Tipo, geom = "point", ggtheme = theme_minimal())  + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 ) 
grid.arrange(p1, p2, nrow = 1)

#Uso KNN para predecir la categorias 

#Genero número aleatorios para tomar una muestra de aprox 80%
data <- data.frame(data)
head(data)
ran <- sample(1:nrow(data), 0.8 * nrow(data)) 

#Normalizo los datos
nor <-function(x) { (x - min(x) )/( max(x) - min(x))   }

#Normalizo las variables
data_norm <- as.data.frame(lapply(data[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)], nor))
summary(data_norm)

data_train <- data_norm[ran,] 
data_test <- data_norm[-ran,] 
data_target_category <- data[ran,1]
data_test_category <- data[-ran,1]
pr <- knn(data_train,data_test,cl=data_target_category,k=13)
tab <- table(pr, data_test_category)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)




















