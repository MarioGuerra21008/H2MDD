movies<-read.csv("movies.csv")
View(movies)

# Dividir la columna 'actorsPopularity' en columnas separadas
actors_popularity <- strsplit(movies$actorsPopularity, "|", fixed = TRUE)

# Convertir los valores de la lista resultante a numéricos y calcular el promedio
actors_popularity_mean <- sapply(actors_popularity, function(x) mean(as.numeric(x)))

# Reemplazar la columna 'actorsPopularity' con el promedio
movies$actorsPopularity <- actors_popularity_mean

movies_clust_data <- movies[, c("budget", "revenue", "runtime", "actorsPopularity", "popularity", 
                                "voteAvg", "voteCount", "genresAmount", "productionCoAmount", "productionCountriesAmount", 
                                "actorsAmount", "castWomenAmount", "castMenAmount")]

movies_clust_data <- na.omit(movies_clust_data)  # Eliminar filas con valores faltantes

View(movies_clust_data)

normalized_data_movies <- scale(movies_clust_data)
