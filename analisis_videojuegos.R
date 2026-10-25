# Cargar las librerias (si no estan descargadas usar install.packets)
library(dplyr)   #install.packages("dplyr")
library(tidyr)   # install.packages("tydyr")
library(ggplot2) # install.packages("ggplot2")
library(ggrepel) # install.packages("ggrepel")

# Cargamos la base de datos (backloggd_games)
backloggd_games <- read.csv("backloggd_games.csv", sep = ",")
videojuegos <- backloggd_games

# Chequeo
str(videojuegos)
sum(duplicated(videojuegos))
unique(videojuegos$Rating)

# Eliminamos la columna X y Summary
videojuegos <- videojuegos %>% select(-X)
videojuegos <- videojuegos %>% select(-Summary)

# Sacamos los corchetes y comillas de Genres y Platforms
# (vienen como "['Adventure', 'RPG']" y los dejamos como "Adventure, RPG")
videojuegos$Genres <- gsub("\\['", "", videojuegos$Genres)
videojuegos$Genres <- gsub("\\']", "", videojuegos$Genres)
videojuegos$Genres <- gsub("'", "", videojuegos$Genres)

videojuegos$Platforms <- gsub("\\['", "", videojuegos$Platforms)
videojuegos$Platforms <- gsub("\\']", "", videojuegos$Platforms)
videojuegos$Platforms <- gsub("'", "", videojuegos$Platforms)

# Funcion que pasa de texto a numero. Los valores como "4.1K" significan
# 4100, entonces sacamos la K y multiplicamos por 1000
convertir_a_numeric <- function(columna) {
  columna <- as.character(columna)
  columna <- trimws(columna)
  columna <- toupper(columna)   # CAMBIO: antes decia "columns <-" (error de tipeo)
  
  tiene_k <- grepl("K", columna)
  columna[tiene_k] <- as.character(as.numeric(gsub("K", "", columna[tiene_k])) * 1000)
  as.numeric(columna)
}

videojuegos <- videojuegos %>%
  mutate(
    Rating   = convertir_a_numeric(Rating),
    Plays    = convertir_a_numeric(Plays),
    Playing  = convertir_a_numeric(Playing),
    Backlogs = convertir_a_numeric(Backlogs),
    Wishlist = convertir_a_numeric(Wishlist),
    Lists    = convertir_a_numeric(Lists),
    Reviews  = convertir_a_numeric(Reviews)
  )

# Cambiamos la clase de "Release_Date" a fecha
videojuegos$Release_Date[videojuegos$Release_Date == ""]    <- NA
videojuegos$Release_Date[videojuegos$Release_Date == "TBD"] <- NA

Sys.setlocale("LC_TIME", "C")
videojuegos$Release_Date <- as.Date(videojuegos$Release_Date, format = "%b %d, %Y")

#======================= Duplicados y NA =======================

# Verificamos y eliminamos filas duplicadas
sum(duplicated(videojuegos))
videojuegos <- unique(videojuegos)

# Cuantos NA hay en cada columna
colSums(is.na(videojuegos))

# Chequeo de listas vacias
videojuegos %>% count(Developers, sort = TRUE)
videojuegos %>% count(Platforms, sort = TRUE)
videojuegos %>% count(Genres, sort = TRUE)

# Las listas vacias "[]" pasan a ser NA
# (IMPORTANTE: despues hay que sacarlos antes de contar categorias)
videojuegos <- videojuegos %>%
  mutate(
    Developers = na_if(Developers, "[]"),
    Platforms  = na_if(Platforms, "[]"),
    Genres     = na_if(Genres, "[]")
  )

# CAMBIO: datos para el informe (cuantos registros hay en cada variable)
nrow(videojuegos)                    # total de videojuegos
sum(!is.na(videojuegos$Rating))      # juegos CON Rating (los demas no tienen)
sum(is.na(videojuegos$Platforms))    # juegos sin plataforma
sum(is.na(videojuegos$Genres))       # juegos sin genero

#======================= Análisis descriptivo: Rating =======================

# 1. Medidas de tendencia central y dispersión
mean(videojuegos$Rating, na.rm = TRUE)    # Media
median(videojuegos$Rating, na.rm = TRUE)  # Mediana
sd(videojuegos$Rating, na.rm = TRUE)      # Desvío estándar
IQR(videojuegos$Rating, na.rm = TRUE)     # Rango intercuartílico
min(videojuegos$Rating, na.rm = TRUE)     # Mínimo
max(videojuegos$Rating, na.rm = TRUE)     # Máximo

# 2. Gráficos simples

# --- Histograma de Rating ---
colores <- colorRampPalette(c("#10002B", "#7B2CBF", "#E0AAFF"))(10)

hist(videojuegos$Rating,
     main = "Distribución de Calificaciones (Rating)",
     xlab = "Calificación",
     ylab = "Frecuencia",
     breaks = seq(0, 5, by = 0.5),   # 10 barras: una por cada tono
     col = colores,
     border = "white")

# --- Boxplot de Rating ---
colores <- colorRampPalette(c("#10002B", "#7B2CBF", "#E0AAFF"))(10)

boxplot(videojuegos$Rating,
        main = "Diagrama de Caja de Calificaciones",
        xlab = "Rating",
        col = colores[6],       # relleno de la caja
        border = colores[1],    # contorno y lineas 
        horizontal = TRUE)

#======================= Análisis descriptivo: Playing =======================
# CAMBIO (nuevo): la consigna pide analizar las 4 variables y Playing faltaba

# Hay 5 registros con Playing = -1 (dato invalido), los dejamos afuera
playing_valido <- videojuegos$Playing[!is.na(videojuegos$Playing) &
                                        videojuegos$Playing >= 0]

summary(playing_valido)       # la mediana es 0: casi nadie esta jugando ahora
sd(playing_valido)            # desvío estándar
mean(playing_valido == 0)     # proporcion de juegos con 0 jugadores actuales

# Casi todos tienen 0 o 1 jugador, entonces un histograma comun se ve
# como una sola barra. Agrupamos en categorias (sin escala log):
grupos_playing <- cut(playing_valido,
                      breaks = c(-Inf, 0, 9, 99, Inf),
                      labels = c("0", "1 a 9", "10 a 99", "100 o más"))

# --- Barras de Playing  ---

colores <- c("#FFA1C2", "#FFF3B2", "#8FB3E8", "#2E5AA7")

barplot(table(grupos_playing),
        main = "Jugadores actuales por videojuego (Playing)",
        xlab = "Cantidad de jugadores actuales",
        ylab = "Cantidad de videojuegos",
        col = colores,
        border = "black") 

# ==========================================
# ANÁLISIS DE PLATAFORMAS (Frecuencias y Porcentajes)
# ==========================================

# 1. Tabla de frecuencias y porcentajes
tabla_platforms <- videojuegos %>%
  mutate(
    Platforms = gsub("[", "", Platforms, fixed = TRUE),
    Platforms = gsub("]", "", Platforms, fixed = TRUE),
    Platforms = gsub("'", "", Platforms, fixed = TRUE)
  ) %>%
  separate_rows(Platforms, sep = ", ") %>%    # una fila por cada plataforma
  mutate(Platforms = trimws(Platforms)) %>%
  filter(!is.na(Platforms)) %>%               # CAMBIO: saca los NA (eran la barra sin nombre)
  count(Platforms, sort = TRUE) %>%           # Frecuencia absoluta (n)
  mutate(Porcentaje = round((n / sum(n)) * 100, 2))

# 2. Gráfico de barras Top 10
top_platforms <- head(tabla_platforms, 10)

# --- Plataformas ---
colores <- c("#03045E", "#023E8A", "#0077B6", "#0096C7", "#00B4D8",
             "#48CAE4", "#90E0EF", "#ADE8F4", "#CAF0F8", "#E0F4FA")

# mas margen abajo (primer numero de mar) para que no se corten los nombres
par(mar = c(7, 5, 4, 2))

barplot(top_platforms$n,
        names.arg = top_platforms$Platforms,
        las = 2,
        main = "Top 10 Plataformas más frecuentes",
        ylab = "Frecuencia",
        cex.names = 0.8,
        col = colores,
        border = colores[1])   # contorno oscuro para que se vean las barras claras

# ==========================================
# ANÁLISIS DE GÉNEROS (Frecuencias y Porcentajes)
# ==========================================

# 1. Tabla de frecuencias y porcentajes
tabla_genres <- videojuegos %>%
  mutate(
    Genres = gsub("[", "", Genres, fixed = TRUE),
    Genres = gsub("]", "", Genres, fixed = TRUE),
    Genres = gsub("'", "", Genres, fixed = TRUE)
  ) %>%
  separate_rows(Genres, sep = ", ") %>%       # una fila por cada género
  mutate(Genres = trimws(Genres)) %>%
  filter(!is.na(Genres)) %>%                  # CAMBIO: saca los NA (eran la barra sin nombre)
  count(Genres, sort = TRUE) %>%
  mutate(Porcentaje = round((n / sum(n)) * 100, 2))

# 2. Gráfico de barras Top 10
top_generos <- head(tabla_genres, 10)

# --- Géneros ---
colores <- colorRampPalette(c("#081C15", "#40916C", "#B7E4C7"))(10)

par(mar = c(7, 5, 4, 2))

barplot(top_generos$n,
        names.arg = top_generos$Genres,
        las = 2,
        main = "Top 10 Géneros más frecuentes",
        ylab = "Frecuencia",
        cex.names = 0.8,
        col = colores,
        border = colores[1])

par(mar = c(5, 4, 4, 2) + 0.1)   # volvemos al margen normal

#======================= Filtros y Orden =======================

# Juegos con muy buena calificacion Y mucha gente jugando hoy
datos <- filter(videojuegos, Rating >= 4.5 & Playing >= 500)
nrow(datos)   # cuantos cumplen las dos condiciones (son 11)

# Ordenamos por Rating (mayor a menor) y despues por Playing (mayor a menor)
# y nos quedamos con los primeros 10
top10 <- datos %>%
  arrange(desc(Rating), desc(Playing)) %>%
  slice(1:10)

print(top10)   # aca se ven los titulos completos

#=============== Gráfico del Top 10 (reemplaza al scatter plot) ===============
# Por que cambiamos: los 10 juegos tienen Rating 4.5 o 4.6, entonces en el
# scatter los puntos quedaban en solo 2 alturas y no mostraba nada.
# Lo que queremos comparar es cuanta gente juega cada titulo: barras.

# Acortamos los titulos largos (mas de 30 letras) para que entren en el grafico
titulos <- ifelse(nchar(top10$Title) > 30,
                  paste0(substr(top10$Title, 1, 28), "..."),
                  top10$Title)

# --- Top 10 juegos ---
colores <- colorRampPalette(c("#590D22", "#C9184A", "#FFB3C1"))(10)

par(mar = c(5, 12, 4, 5))   # margen izquierdo para los titulos

# rev() invierte el orden para que el #1 quede ARRIBA del grafico
barras <- barplot(rev(top10$Playing),
                  names.arg = rev(titulos),
                  horiz = TRUE, las = 1,
                  xlim = c(0, max(top10$Playing) * 1.25),   # lugar para el texto
                  cex.names = 0.7,
                  main = "Top 10 videojuegos: jugadores actuales",
                  xlab = "Jugadores actuales (Playing)",
                  col = rev(colores),       # rev() porque se dibuja de abajo hacia arriba
                  border = colores[1])

# Escribimos el Rating al final de cada barra
# (barras = posicion vertical de cada barra, la devuelve barplot)
text(x = rev(top10$Playing), y = barras,
     labels = paste("Rating", rev(top10$Rating)),
     pos = 4, cex = 0.7)

par(mar = c(5, 4, 4, 2) + 0.1)

