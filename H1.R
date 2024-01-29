movies<-read.csv("movies.csv")
View(movies)

#1

nrow(movies)
ncol(movies)

runtimeMovies <- c(movies$runtime)
mostLongMovies <- sort(runtimeMovies, decreasing=TRUE)
head(mostLongMovies,10)

mean(movies$voteAvg)

#2

View(movies$id)
View(movies$budget)
View(movies$genres)
View(movies$homePage)
View(movies$productionCompany)
View(movies$productionCompanyCountry)
View(movies$productionCountry)
View(movies$revenue)
View(movies$runtime)
View(movies$video)
View(movies$director)
View(movies$actors)
View(movies$actorsPopularity)
View(movies$actorsCharacter)
View(movies$originalTitle)
View(movies$title)
View(movies$originalLanguage)
View(movies$popularity)
View(movies$releaseDate)
View(movies$voteAvg)
View(movies$voteCount)
View(movies$genresAmount)
View(movies$productionCoAmount)
View(movies$productionCountriesAmount)
View(movies$actorsAmount)
View(movies$castWomenAmount)
View(movies$castMenAmount)

#3

# Verifica la normalidad de las variables cuantitativas
quantitative_vars <- c("runtime", "rating", "metascore", "votes", "gross")

for (var in quantitative_vars) {
  qqnorm(movies[[var]])
  qqline(movies[[var]])
  title(paste("Q-Q Plot para", var))
}
#
qqnorm(movies[["runtime"]])
qqline(movies[["runtime"]])
title(paste("", "runtime"))

# Tamaño deseado para la muestra (ajusta según tus necesidades)
tamano_muestra <- 500

# Realiza el test de Shapiro-Wilk en una muestra aleatoria
shapiro_test <- shapiro.test(sample(movies$runtime, tamano_muestra))
print(shapiro.test)

variable <- movies$runtime


# Grafico de densidad Runtime
ggplot(data = NULL, aes(x = movies$runtime)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la Variable", x = "Runtime", y = "Frecuencia")

qqnorm(movies$runtime)
qqline(movies$runtime)
title("Gráfico Q-Q de la Variable")


# Histograma Popularity
ggplot(data = NULL, aes(x = movies$popularity)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la Variable", x = "Popularity", y = "Frecuencia")

qqnorm(movies$popularity)
qqline(movies$popularity)
title("Gráfico Q-Q de la Variable")

# Grafico de densidad VoteAVG
ggplot(data = NULL, aes(x = movies$voteAvg)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución de la Variable", x = "VoteAvg", y = "Densidad")

qqnorm(movies$voteAvg)
qqline(movies$voteAvg)
title("Gráfico Q-Q de la Variable")

#Histograma VoteCount
ggplot(data = NULL, aes(x = movies$voteCount)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la Variable", x = "Popularity", y = "Frecuencia")

qqnorm(movies$voteCount)
qqline(movies$voteCount)
title("Gráfico Q-Q de la Variable")

#Grafico de densidad genresAmount
ggplot(data = NULL, aes(x = movies$genresAmount)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución de la Variable", x = "runtime", y = "Densidad")

qqnorm(movies$genresAmount)
qqline(movies$genresAmount)
title("Gráfico Q-Q de la Variable")

#Histograma productioncoamount
ggplot(data = NULL, aes(x = movies$productionCoAmount)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de la Variable", x = "Popularity", y = "Frecuencia")

#Grafico de densidad productioncountr
ggplot(data = NULL, aes(x = movies$productionCountriesAmount)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución de la Variable", x = "runtime", y = "Densidad")

variable_cualitativa1 <- movies$genres
variable_cualitativa2 <- movies$productionCompany

# Crear la tabla de frecuencias con dplyr
tabla_frecuencias <- data.frame(
  Categoría1 = variable_cualitativa1,
  Categoría2 = variable_cualitativa2
) %>%
  group_by(Categoría1, Categoría2) %>%
  summarise(Frecuencia = n())

# Crear la tabla de frecuencias con dplyr
tabla_frecuencias <- table(movies$actors)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)

library(tidyverse)
install.packages(c("tidyverse", "stringr"))
library(dplyr)


lista_actores <- strsplit(movies$actors, "|", fixed = TRUE)

# Convertir la lista en un vector único
vector_actores <- unlist(lista_actores)

tabla_frecuencias <- table(vector_actores)
tabla_frecuencias <- sort(tabla_frecuencias, decreasing=TRUE)

top_50_actores <- head(tabla_frecuencias, 50)

print(top_50_actores)


#4


#4.1 Peliculas con mayor presupuestoS

datos_ordenados <- movies[order(-movies$budget), ]

top_10_presupuesto <- head(datos_ordenados[, c("budget", "title")], 10)

# Imprimir el resultado
print(top_10_presupuesto)

#4.2 Peliculas con mayores ingresos

datos_ordenados_ingresos <- movies[order(-movies$revenue), ]

top_10_ingresos <- head(datos_ordenados_ingresos[, c("revenue", "title")], 10)

print(top_10_ingresos)


#4.3 Pelicula con mas votos

pelicula_mas_votada <- movies[which.max(movies$voteCount), ]

print(pelicula_mas_votada[c("title", "voteCount")])


#4.4 Pelicula con peores votaciones

peor_pelicula <- movies[which.min(movies$voteAvg), ]

print(peor_pelicula[c("title","voteAvg")])

#4.5 Cuantas peliculas se hicieron cada año y en qué año se hicieron mas.

library(ggplot2)
install.packages("ggplot2")

movies$releaseYear <- as.integer(format(as.Date(movies$releaseDate), "%Y"))

conteo_por_anio <- movies %>%group_by(releaseYear) %>%summarise(numero_de_peliculas = n())

anio_mas_peliculas <- conteo_por_anio[which.max(conteo_por_anio$numero_de_peliculas), "releaseYear"]

tabla_conteo <- as.data.frame(conteo_por_anio)

print(tabla_conteo)
print(paste("Año con más películas:", anio_mas_peliculas))

ggplot(conteo_por_anio, aes(x = as.factor(releaseYear), y = numero_de_peliculas)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Número de películas por año", x = "Año", y = "Número de películas") +
  theme_minimal()

#4.6 Género principal de las 20 películas más recientes. Género que predomina en el conjunto de datos.


#Genero mas popular entre las 20 peliculas mas recientes.

peliculas_ordenadas_por_fecha <- movies[order(movies$releaseDate, decreasing = TRUE), ]
top_20_pelisrecientes <- head(peliculas_ordenadas_por_fecha, 20)

genero_principal_top20 <- names(sort(table(unlist(strsplit(tolower(top_20_pelisrecientes$genres), ","))), decreasing = TRUE))[1]

print(genero_principal_top20)

#Genero mas popular en general

genero_principal <- names(sort(table(unlist(strsplit(tolower(movies$genres), ","))), decreasing = TRUE))[1]

print(genero_principal)

#Grafico de generos en el conjunto de datos.

library(ggplot2)

genre_counts <- as.data.frame(table(unlist(strsplit(tolower(movies$genres), ","))))
colnames(genre_counts) <- c("Genre", "Count")

genre_counts <- genre_counts[order(genre_counts$Count, decreasing = TRUE), ]

ggplot(genre_counts[1:10, ], aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Género que predomina en el conjunto de datos completo",
       x = "Género",
       y = "Cantidad") +
  coord_flip()

#4.7 Películas del género drama que obtuvieron mayor ganancia

peliculas_del_genero_principal <- movies[grep(genero_principal, tolower(movies$genres)), ]
pelis_con_mayor_ganancia_por_genero <- peliculas_del_genero_principal[order(peliculas_del_genero_principal$revenue, decreasing = TRUE), ]
View(pelis_con_mayor_ganancia_por_genero[c("title", "genres", "revenue")])

#4.8 La cantidad de actores influye en los ingresos? Se han hecho películas con más actores en los últimos años?

ingresos_sobre_actores <- cor(movies$actorsAmount, movies$revenue)
print(paste("La correlación entre la cantidad de actores y los ingresos por película es de: ", ingresos_sobre_actores))

library(ggplot2)

ggplot(movies, aes(x = actorsAmount, y = revenue)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación entre la cantidad de actores y los ingresos",
       x = "Cantidad de actores",
       y = "Ingresos (Revenue)")

#Las 100 películas y su cantidad de actores.

top_20_pelisantiguas <- tail(peliculas_ordenadas_por_fecha, 20)
print(top_20_pelisrecientes[c("title","actorsAmount","releaseDate")])
print(top_20_pelisantiguas[c("title","actorsAmount","releaseDate")])

#4.9 La cantidad de hombres y mujeres influye en la popularidad e ingresos de las películas?

peliculas_mas_populares_con_ingresos <- movies[order(-movies$popularity, -movies$revenue), ]
head(peliculas_mas_populares_con_ingresos[c("title","popularity","revenue","castMenAmount","castWomenAmount")], 20)
tail(peliculas_mas_populares_con_ingresos[c("title","popularity","revenue","castMenAmount","castWomenAmount")], 20)

#4.10

top_peliscalificadas <- movies[order(-movies$voteAvg), ]
View(head(top_peliscalificadas[c("title","director","voteAvg")], 20))

#4.11

# Muestra las primeras filas de los datos
head(movies)

# Resumen estadístico de las variables numéricas
summary(movies)


install.packages("ggplot2")


library(ggplot2)

# Gráfico de dispersión
ggplot(movies, aes(x = budget, y = revenue)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Presupuestos vs. Ingresos",
       x = "Presupuesto",
       y = "Ingresos")




# Histograma de presupuestos
ggplot(movies, aes(x = budget)) +
  geom_histogram(binwidth = 10000000, fill = "blue", color = "black") +
  labs(title = "Histograma de Presupuestos",
       x = "Presupuesto",
       y = "Frecuencia")

# Histograma de ingresos
ggplot(movies, aes(x = revenue)) +
  geom_histogram(binwidth = 10000000, fill = "green", color = "black") +
  labs(title = "Histograma de Ingresos",
       x = "Ingresos",
       y = "Frecuencia")

#4.12

movies$releaseDate <- as.Date(movies$releaseDate, format = "%Y-%m-%d")

movies$mesLanzamiento <- format(movies$releaseDate, "%m")

# Gráfico de barras
ggplot(movies, aes(x = mesLanzamiento, y = revenue)) +
  stat_summary(fun = "mean", geom = "bar", fill = "blue") +
  labs(title = "Ingresos Promedio por Mes de Lanzamiento",
       x = "Mes",
       y = "Ingresos Promedio")



#4.13

library(dplyr)

# Agrupar por mes y calcular ingreso promedio
ingresos_por_mes <- movies %>%
  group_by(mesLanzamiento) %>%
  summarise(ingreso_promedio = mean(revenue, na.rm = TRUE))


# Encontrar los meses con los mejores ingresos
meses_mejores_ingresos <- ingresos_por_mes %>%
  filter(ingreso_promedio == max(ingreso_promedio))


# Gráfico de barras
ggplot(ingresos_por_mes, aes(x = mesLanzamiento, y = ingreso_promedio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Ingresos Promedio por Mes de Lanzamiento",
       x = "Mes",
       y = "Ingresos Promedio")


# Número promedio de películas por mes
promedio_peliculas_por_mes <- movies %>%
  group_by(mesLanzamiento) %>%
  summarise(promedio_peliculas = n() / n_distinct(title))


# Gráfico de barras
ggplot(promedio_peliculas_por_mes, aes(x = mesLanzamiento, y = promedio_peliculas)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Número Promedio de Películas por Mes de Lanzamiento",
       x = "Mes",
       y = "Número Promedio de Películas")


#4.14


# Gráfico de dispersión con la columna 'voteAvg'
ggplot(movies, aes(x = voteAvg, y = revenue)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Calificaciones vs. Ingresos",
       x = "Calificación",
       y = "Ingresos")


# Mostrar la correlación
correlation_rating_revenue <- cor(movies$voteAvg, movies$revenue, use = "complete.obs")
print(paste("Correlación entre calificaciones e ingresos:", correlation_rating_revenue))



#4.15


# Identificar la película más larga
pelicula_mas_larga <- movies[which.max(movies$runtime), ]

# Obtener el género principal de la película más larga
genero_principal_mas_larga <- pelicula_mas_larga$genres

# Mostrar el género principal
print(paste("El género principal de la película más larga es:", genero_principal_mas_larga))


# Gráfico de barras
ggplot(movies, aes(x = genres, y = runtime)) +
  geom_bar(stat = "summary", fun = "mean", fill = "blue") +
  labs(title = "Duración Promedio de Películas por Género",
       x = "Género",
       y = "Duración Promedio")



#5





