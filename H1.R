movies<-read.csv("C:/Users/Alvar/OneDrive/Documentos/SemestreVII/MineriaDeDatos/Hoja1/movies.csv")
View(movies)

library(readr)
library(dplyr)
library(ggplot2)
library(pastecs)

#1





#2

View(movies$title)
View(movies$year)
View(movies$runtime)
View(movies$certificate)
View(movies$genre)
View(movies$director)
View(movies$stars)
View(movies$rating)
View(movies$metascore)
View(movies$votes)
View(movies$gross)



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


#4.1


#4.2


#4.3


#4.4


#4.5


#4.6


#4.7


#4.8


#4.9


#4.10


#4.11


#4.12


#4.13


#4.14


#4.15





#5





