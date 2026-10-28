#Cargamos libreria tidyverse
library(tidyverse)

# Cargamos la base de datos (backloggd_games)
backloggd_games <-read.csv("backloggd_games.csv", sep=",")
videojuegos <- backloggd_games

# Chequeo
str(videojuegos)
sum(duplicated(videojuegos))
unique(videojuegos$Rating)

# Eliminamos la columna X
videojuegos <- videojuegos %>% select(-X)

# Convertimos las filas de "character" a "numeric" con una funcion
convertir_a_numeric <-
  function(columna) {
    columna <- as.character(columna)
    columna <- trimws(columna)
    columns <- toupper(columna)
    
    tiene_k <- grepl("K", columna)
    
    columna[tiene_k] <- as.character(as.numeric(gsub("K","", columna[tiene_k])) * 1000
    )
    as.numeric(columna)
  }
videojuegos<- videojuegos %>%
  mutate(
    Rating = convertir_a_numeric(Rating),
    Plays = convertir_a_numeric(Plays),
    Playing = convertir_a_numeric(Playing),
    Backlogs = convertir_a_numeric(Backlogs),
    Wishlist = convertir_a_numeric(Wishlist),
    Lists = convertir_a_numeric(Lists),
    Reviews = convertir_a_numeric(Reviews)
  )
# Cambiamos la clase de "Release_Date" a date

videojuegos$Release_Date[videojuegos$Release_Date == ""] <-NA
videojuegos$Release_Date[videojuegos$Release_Date == "TBD"]<- NA

Sys.setlocale("LC_TIME","C")
videojuegos$Release_Date <- as.Date(
  videojuegos$Release_Date,
  format = "%b %d, %Y"
)

#Verificamos si hay filas duplicadas
sum(duplicated(videojuegos))

#eliminamos las filas duplicadas
videojuegos <- unique(videojuegos)

#Chequeo de cuantos na hay en cada columna
colSums(is.na(videojuegos))

#Chequeo de listas vacia en columnas
videojuegos %>% 
  count(Developers, sort = TRUE)

videojuegos %>% 
  count(Platforms, sort = TRUE)

videojuegos %>% 
  count(Genres, sort = TRUE)

#Funcion ya integrada de tidyverse, que convierte cualquier argumento que pongamos en na
videojuegos <- videojuegos %>%
  mutate(
    Developers = na_if(Developers, "[]"),
    Platforms = na_if(Platforms, "[]"),
    Genres = na_if(Genres, "[]"),
    Summary = na_if(Summary, "")
    )

#Analisis descriptivo de las variables: Rating (Cuantitativa)

# 1. Medidas de tendencia central y dispersión
mean(videojuegos$Rating, na.rm = TRUE)    # Media
median(videojuegos$Rating, na.rm = TRUE)  # Mediana
sd(videojuegos$Rating, na.rm = TRUE)      # Desvío estándar
IQR(videojuegos$Rating, na.rm = TRUE)     # Rango intercuartílico

#--Minimos y Maximos, para ver los valores extremo
min(videojuegos$Rating, na.rm = TRUE)     # Minimo
max(videojuegos$Rating, na.rm = TRUE)     # Máximo

# 2. Gráficos simples
hist(videojuegos$Rating, 
     main = "Distribución de Calificaciones (Rating)", 
     xlab = "Calificación", 
     ylab = "Frecuencia",
     col = "skyblue")

boxplot(videojuegos$Rating, 
        main = "Diagrama de Caja de Calificaciones", 
        ylab = "Rating", 
        col = "lightgreen",
        horizontal = TRUE)

#Analisis descriptivo de las variables: Plays (Cuantitativa)

# 0. Contar y mostrar cuántos registros no válidos (ceros, negativos o NA) se descartaron
sum(videojuegos$Plays <= 0, na.rm = TRUE)     # Cantidad de juegos con 0 jugadas o menos

# Creamos el vector limpio (sin ceros, negativos ni NA)
plays_limpios <- videojuegos$Plays[videojuegos$Plays > 0]

# 1. Medidas de posición sobre los datos limpios
mean(plays_limpios)     # Media
median(plays_limpios)   # Mediana
quantile(plays_limpios, probs = c(0.25, 0.5, 0.75, 0.95)) # Cuartiles

# Minimos y Maximos limpios
min(plays_limpios)    # Mínimo
max(plays_limpios)    # Máximo

# 2. Medidas de dispersión sobre los datos limpios
sd(plays_limpios)       # Desvío estándar
IQR(plays_limpios)      # Rango intercuartílico

# 3. Gráficos utilizando el mismo vector limpio
hist(plays_limpios, 
     main = "Distribución de la Cantidad de Jugadores (Plays)", 
     xlab = "Cantidad de Jugadores", 
     ylab = "Frecuencia",
     col = "orange", 
     border = "white",
     breaks = 30)

boxplot(plays_limpios, 
        main = "Diagrama de Caja - Cantidad de veces que lo jugaron", 
        xlab = "Cantidad de Jugadores", 
        col = "tomato",
        horizontal = TRUE,
        outline = TRUE)

# ==========================================
# ANÁLISIS DE GÉNEROS (Frecuencias y Porcentajes)
# ==========================================

# 1. Creamos la tabla de frecuencias y porcentajes para géneros
tabla_genres <- videojuegos %>%
  mutate(
    Genres = gsub("[", "", Genres, fixed = TRUE),
    Genres = gsub("]", "", Genres, fixed = TRUE),
    Genres = gsub("'", "", Genres, fixed = TRUE)
  ) %>%
  separate_rows(Genres, sep = ", ") %>%     # Separa los géneros múltiples por la coma
  mutate(Genres = trimws(Genres)) %>%       # Limpia espacios en blanco extras
  count(Genres, sort = TRUE) %>%            # Cuenta frecuencias absolutas y ordena
  mutate(
    Porcentaje = round((n / sum(n)) * 100, 2) # Porcentaje redondeado a 2 decimales
  )

# Imprimimos la tabla completa en la consola
#print(tabla_genres)

# 2. Gráfico de barras para el Top 10 de géneros
top_generos <- head(tabla_genres, 10)

barplot(top_generos$n, 
        names.arg = top_generos$Genres, 
        las = 2,                          
        col = "steelblue", 
        main = "Top 10 Géneros más frecuentes", 
        ylab = "Frecuencia",
        cex.names = 0.8)                          


# ==========================================
# ANÁLISIS DE PLATAFORMAS (Frecuencias y Porcentajes)
# ==========================================

# 1. Creamos la tabla de frecuencias y porcentajes para plataformas
tabla_platforms <- videojuegos %>%
  mutate(
    Platforms = gsub("[", "", Platforms, fixed = TRUE),
    Platforms = gsub("]", "", Platforms, fixed = TRUE),
    Platforms = gsub("'", "", Platforms, fixed = TRUE)
  ) %>%
  separate_rows(Platforms, sep = ", ") %>%   # Separamos plataformas múltiples
  mutate(Platforms = trimws(Platforms)) %>%  # Limpiamos espacios extra
  count(Platforms, sort = TRUE) %>%          # Frecuencia absoluta (n)
  mutate(
    Porcentaje = round((n / sum(n)) * 100, 2) # Porcentaje redondeado a 2 decimales
  )

# Imprimimos la tabla completa en la consola
#print(tabla_platforms)

# 2. Gráfico de barras para el Top 10 de plataformas
top_platforms <- head(tabla_platforms, 10)

barplot(top_platforms$n, 
        names.arg = top_platforms$Platforms, 
        las = 2, 
        col = "forestgreen", 
        main = "Top 10 Plataformas más frecuentes", 
        ylab = "Frecuencia",
        cex.names = 0.8)# Cambio de prueba
