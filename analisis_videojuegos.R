# Cargar las librerias (si no estan descargadas usar install.packets)
library(dplyr)   #install.packages("dplyr")
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

# Convertimos y sacamos los corchetes de las columnas Genrs y Plataforms
videojuegos$Genres <- gsub("\\['", "", videojuegos$Genres)  # Elimina ['
videojuegos$Genres <- gsub("\\']", "", videojuegos$Genres)  # Elimina ']
videojuegos$Genres <- gsub("'", "", videojuegos$Genres)     # Elimina comillas simples restantes

# Hacemos lo mismo con Platforms
videojuegos$Platforms <- gsub("\\['", "", videojuegos$Platforms)
videojuegos$Platforms <- gsub("\\']", "", videojuegos$Platforms)
videojuegos$Platforms <- gsub("'", "", videojuegos$Platforms)

# Convertimos las filas de "character" a "numeric" con una función
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

#======================= Duplicados y NA =======================

# Verificamos si hay filas duplicadas
sum(duplicated(videojuegos))

# Eliminamos las filas duplicadas
videojuegos <- unique(videojuegos)

# Chequeo de cuantos NA hay en cada columna
colSums(is.na(videojuegos))

# Chequeo de listas vacias en columnas
videojuegos %>% 
  count(Developers, sort = TRUE)

videojuegos %>% 
  count(Platforms, sort = TRUE)

videojuegos %>% 
  count(Genres, sort = TRUE)

# Funcion ya integrada de dplyr, que convierte en NA el valor que le indiquemos ("[]" = lista vacia)
videojuegos <- videojuegos %>%
  mutate(
    Developers = na_if(Developers, "[]"),
    Platforms = na_if(Platforms, "[]"),
    Genres = na_if(Genres, "[]")
  )

#======================= Análisis descriptivo: Rating =======================

# 1. Medidas de tendencia central y dispersión
mean(videojuegos$Rating, na.rm = TRUE)    # Media
median(videojuegos$Rating, na.rm = TRUE)  # Mediana
sd(videojuegos$Rating, na.rm = TRUE)      # Desvío estándar
IQR(videojuegos$Rating, na.rm = TRUE)     # Rango intercuartílico

# Minimos y Maximos, para ver los valores extremos
min(videojuegos$Rating, na.rm = TRUE)     # Mínimo
max(videojuegos$Rating, na.rm = TRUE)     # Máximo

# 2. Gráficos simples
hist(videojuegos$Rating, 
     main = "Distribución de Calificaciones (Rating)", 
     xlab = "Calificación", 
     ylab = "Frecuencia",
     col = "skyblue")

#======================= Filtros y Orden =======================

# filtramos cuales son los mejores juegos, teniendo en cuenta el rating y cuantos juegadores lo juegan  
datos <- filter(videojuegos, Rating >= 4.5 & Playing >= 500)

# Ordenamos por Rating (mayor a menor) y después por Playing (mayor a menor)
# Usamos los primeros 10
top10 <- datos %>%
  arrange(desc(Rating), desc(Playing)) %>%
  slice(1:10)

print(top10)

#=============== Gráfico Scatter Plot con los 10 mejores ===============

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

#=================== Año de los 5 géneros con más juegos ===================

# Elegimos los 5 géneros
top_generos <- c("Adventure", "Indie", "RPG", "Simulator", "Strategy")

lanz_gen <- videojuegos %>%
  
  mutate(anio = as.numeric(format(Release_Date, "%Y"))) %>% # Nos quedamos con el año
  
  filter(!is.na(anio), anio >= 2000, anio <= 2023) %>% # Sacamos los NA y nos quedamos con 2000-2023
  
  separate_rows(Genres, sep = ", ") %>% # Separamos cada género en su propia fila
  
  filter(Genres %in% top_generos) %>%
  
  count(anio, Genres)


#============== Gráfico de lanzamientos por año según el género ==============

# gráfico de líneas múltiples

colores <- c(
  "Adventure" = "#FF8FA3",  
  "Indie"     = "#5CC8E0",  
  "RPG"       = "#A58CFF",  
  "Simulator" = "#FFC857",  
  "Strategy"  = "#6FD6A5"   
)
ggplot(lanz_gen, aes(x = anio, y = n, color = Genres)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = colores) +  # acá se aplican los colores
  labs(
    title = "Lanzamientos por año según género",
    subtitle = "Top 5 géneros más frecuentes, 2000-2023",
    x = "Año",
    y = "Cantidad de juegos",
    color = "Género"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#=======================================================================
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
        cex.names = 0.8)

