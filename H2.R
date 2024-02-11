
movies<-read.csv("movies.csv")
View(movies)

####
#### Preprocesamiento de datos para el Clustering
####

# Dividir la columna 'actorsPopularity' en columnas separadas
actors_popularity <- strsplit(movies$actorsPopularity, "|", fixed = TRUE)

# Convertir los valores de la lista resultante a numéricos y calcular el promedio
actors_popularity_mean <- sapply(actors_popularity, function(x) mean(as.numeric(x)))

# Reemplazar la columna 'actorsPopularity' con el promedio
movies$actorsPopularity <- actors_popularity_mean

movies_clust_data <- movies[, c("budget", "revenue", "runtime", "actorsPopularity", "popularity", 
                                "voteAvg", "voteCount", "genresAmount", "productionCoAmount", "productionCountriesAmount", 
                                "actorsAmount", "castWomenAmount", "castMenAmount")]

movies_clust_data <- na.omit(movies_clust_data) # Eliminar filas con valores faltantes

#Eliminar los valores no numericos
movies_clust_data <- movies_clust_data[!is.na(as.numeric(movies_clust_data$castWomenAmount)) &
                                         !is.na(as.numeric(movies_clust_data$castMenAmount)), ]
rm(movies_clust_data)
View(movies_clust_data)

# Convertir las columnas a tipo numérico
movies_clust_data$castWomenAmount <- as.numeric(movies_clust_data$castWomenAmount)
movies_clust_data$castMenAmount <- as.numeric(movies_clust_data$castMenAmount)

str(movies_clust_data)

normalized_data_movies <- scale(movies_clust_data)
normalized_data_movies <- na.omit(normalized_data_movies)
View(normalized_data_movies)

#
# Importación de módulos de clúster.
#

install.packages("cluster")
library(cluster)

#
# Obtener el número de Clústeres para poder aplicar Clustering
#

set.seed(123)
k_values <- 1:10
iner <- numeric(length(k_values))

for (k in k_values) {
  model <- kmeans(normalized_data_movies, centers = k)
  iner[k] <- model$tot.withinss
}

# Graficar la curva de codo
plot(k_values, iner, type = "b", main = "Método del Codo", xlab = "Número de Clústeres (k)", ylab = "Inercia")

# Agregar líneas para facilitar la identificación del codo
abline(v = which.min(diff(iner) > 10) + 1, col = "red", lty = 2)


#
# Aplicar Clústering con método K-Means
# 

set.seed(123)
num_clusters <- 2  # Número de clústeres determinado anteriormente

# Aplicar el algoritmo de k-means
kmeans_model <- kmeans(normalized_data_movies, centers = num_clusters)

# Añadir las etiquetas de clúster al conjunto de datos
movies_clust_data$kmeans_cluster <- as.factor(kmeans_model$cluster)

# Visualizar el resultado del clustering
table(movies_clust_data$kmeans_cluster)

#
# Aplicar Clustering con método de clustering jerárquico
#
library(ggplot2)

# Realizar clustering jerárquico
hc_model <- hclust(dist(normalized_data_movies), method = "complete")

# Determinar el número óptimo de clusters
num_clusters <- 2

# Obtener las etiquetas de clúster
clusters_hc <- cutree(hc_model, k = num_clusters)

# Añadir las etiquetas de clúster al conjunto de datos
movies_clust_data$hc_cluster <- as.factor(clusters_hc)

# Realizar Análisis de Componentes Principales (PCA)
pca <- prcomp(normalized_data_movies)
data_pca <- as.data.frame(pca$x[, 1:2])  # Tomamos solo las dos primeras componentes principales

# Crear diagrama de dispersión con colores por clúster
ggplot(data_pca, aes(x = PC1, y = PC2, color = clusters_hc)) +
  geom_point() +
  labs(title = "Clustering Jerárquico - ACP")

#
# Comparación de los algoritmos de clustering, con su calidad de agrupamiento.
#

# Calcular la medida de silueta para k-means
silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(normalized_data_movies))

# Mostrar el resumen de la medida de silueta
summary(silhouette_kmeans)

# Calcular la medida de silueta para clustering jerárquico
silhouette_hc <- silhouette(clusters_hc, dist(normalized_data_movies))

# Mostrar el resumen de la medida de silueta
summary(silhouette_hc)

