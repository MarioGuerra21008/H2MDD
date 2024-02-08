movies<-read.csv("movies.csv")
View(movies)

movies_clust_data <- movies[, c("budget", "revenue", "runtime", "actorsPopularity", "popularity", 
                                "voteAvg", "voteCount", "genresAmount", "productionCoAmount", "productionCountriesAmount", 
                                "actorsAmount", "castWomenAmount", "castMenAmount")]

movies_clust_data <- na.omit(movies_clust_data)  # Eliminar filas con valores faltantes

normalized_data_movies <- scale(movies_clust_data)
