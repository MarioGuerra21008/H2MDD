
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
View(normalized_data_movies)

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