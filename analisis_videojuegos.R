# Hacemos una muestra de 500 filas de backloggd_games 
backloggd_games <-read.csv("backloggd_games.csv", sep=",")
muestra <-head(backloggd_games, 500)

# Eliminamos la columna X
muestra <- muestra %>% select(-X)

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
muestra<- muestra %>%
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

muestra$Release_Date[muestra$Release_Date == ""] <-NA
muestra$Release_Date[muestra$Release_Date == "TBD"]<- NA

Sys.setlocale("LC_TIME","C")
muestra$Release_Date <- as.Date(
  muestra$Release_Date,
  format = "%b %d, %Y"
  )

                            