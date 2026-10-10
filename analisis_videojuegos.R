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

