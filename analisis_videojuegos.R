# Cargar las librerias (si no estan descargadas usar install.packets)
library(dplyr)   # install.packages("dplyr")
library(tidyr)   # install.packages("tydyr")
library(ggplot2) # install.packages("ggplot2")
library(ggrepel) # install.packages("ggrepel")

# Cargamos la base de datos (backloggd_games)
backloggd_games <-read.csv("backloggd_games.csv", sep=",")
videojuegos <- backloggd_games

# Chequeo
str(videojuegos)
sum(duplicated(videojuegos))
unique(videojuegos$Rating)

# Eliminamos la columna X y Summary
videojuegos <- videojuegos %>% select(-X)
videojuegos <- videojuegos %>% select(-Summary)

# Convertimos acamos los corchetes de las columnas Genrs y Plataforms
videojuegos$Genres <- gsub("\\['", "", videojuegos$Genres)  # Elimina ['
videojuegos$Genres <- gsub("\\']", "", videojuegos$Genres)  # Elimina ']
videojuegos$Genres <- gsub("'", "", videojuegos$Genres)     # Elimina comillas simples restantes

# Hacer lo mismo con Platforms
videojuegos$Platforms <- gsub("\\['", "", videojuegos$Platforms)
videojuegos$Platforms <- gsub("\\']", "", videojuegos$Platforms)
videojuegos$Platforms <- gsub("'", "", videojuegos$Platforms)

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

# filtramos cuales son los mejores videojuegos, teniendo en cuenta el rating y cuantos juegadores lo juegan  
datos <- filter(videojuegos, Rating >= 4.5 & Playing >= 500)

# Ordenar por Rating (mayor a menor) y luego por Playing (mayor a menor)
# Los primeros 10 son utilizados
top10 <- datos %>%
  arrange(desc(Rating), desc(Playing)) %>%
  slice(1:10)

print(top10)

# Grafico Scatter Plot con los 10
ggplot(top10, aes(x = Playing, y = Rating, label = Title)) +
  geom_point(size = 8, color = "steelblue", alpha = 0.7) +
  geom_text_repel(aes(label = paste0(Title,"\n",
                                      sprintf("%.1f", Rating))),
    size = 4,
    color = "black",
    box.padding = 0.7,
    segment.color = "gray"
  ) +
  labs(
    title = "Top 10 Mejores Videojuegos",
    subtitle = "Rating >= 4.5 y más de 500 jugadores",
    x = "Jugadores Actuales (Playing)",
    y = "Rating"
  ) +
  scale_y_continuous(
    limits = c(1,5),breaks = seq(1,5, by=1),
    labels = function(x) sub("\\.", ",", format(x,nsmall = 1))
  )+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )



