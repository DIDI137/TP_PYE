# ==========================================================================
# Trabajo Práctico - Probabilidad y Estadística 2026
# Pregunta: ¿Qué combinaciones de género y plataforma tienen mayor consumo
# (Playing) y mejor valoración (Rating) de los videojuegos?
# ==========================================================================

# Cargar las librerias
library(dplyr)   # install.packages("dplyr")
library(tidyr)   # install.packages("tidyr")

# ======================= Carga de datos =======================
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

# ======================= Limpieza =======================

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
  columna <- toupper(columna)
  
  tiene_k <- grepl("K", columna)
  columna[tiene_k] <- as.character(as.numeric(gsub("K", "", columna[tiene_k])) * 1000)
  as.numeric(columna)
}

# Solo convertimos las variables que usa el analisis
videojuegos <- videojuegos %>%
  mutate(
    Rating  = convertir_a_numeric(Rating),
    Playing = convertir_a_numeric(Playing)
  )

# ======================= Duplicados y NA =======================

# Verificamos y eliminamos filas duplicadas
sum(duplicated(videojuegos))
videojuegos <- unique(videojuegos)

# Cuantos NA hay en cada columna
colSums(is.na(videojuegos))

# Chequeo de listas vacias
videojuegos %>% count(Platforms, sort = TRUE)
videojuegos %>% count(Genres, sort = TRUE)

# Las listas vacias "[]" pasan a ser NA
# (despues hay que sacarlos antes de contar categorias)
videojuegos <- videojuegos %>%
  mutate(
    Platforms = na_if(Platforms, "[]"),
    Genres    = na_if(Genres, "[]")
  )

# Datos para el informe (cuantos registros hay en cada variable)
nrow(videojuegos)                    # total de videojuegos (muestra)
sum(!is.na(videojuegos$Rating))      # juegos CON Rating
sum(is.na(videojuegos$Rating))       # juegos SIN Rating
sum(is.na(videojuegos$Platforms))    # juegos sin plataforma
sum(is.na(videojuegos$Genres))       # juegos sin genero
sum(videojuegos$Playing < 0, na.rm = TRUE)   # registros invalidos de Playing (-1)

# ======================= Análisis descriptivo: Rating =======================

# 1. Medidas de tendencia central y dispersión
mean(videojuegos$Rating, na.rm = TRUE)    # Media
median(videojuegos$Rating, na.rm = TRUE)  # Mediana
sd(videojuegos$Rating, na.rm = TRUE)      # Desvío estándar
IQR(videojuegos$Rating, na.rm = TRUE)     # Rango intercuartílico
min(videojuegos$Rating, na.rm = TRUE)     # Mínimo
max(videojuegos$Rating, na.rm = TRUE)     # Máximo
summary(videojuegos$Rating)               # Incluye Q1 y Q3

# 2. Gráficos
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
boxplot(videojuegos$Rating,
        main = "Diagrama de Caja de Calificaciones",
        xlab = "Rating",
        col = colores[6],       # relleno de la caja
        border = colores[1],    # contorno y lineas
        horizontal = TRUE)

# ======================= Análisis descriptivo: Playing =======================
# 1. Medidas de tendencia central y dispersión
mean(videojuegos$Playing, na.rm = TRUE)    # Media
median(videojuegos$Playing, na.rm = TRUE)  # Mediana
sd(videojuegos$Playing, na.rm = TRUE)      # Desvío estándar
IQR(videojuegos$Playing, na.rm = TRUE)     # Rango intercuartílico
min(videojuegos$Playing, na.rm = TRUE)     # Mínimo
max(videojuegos$Playing, na.rm = TRUE)     # Máximo
summary(videojuegos$Playing)               # Incluye Q1 y Q3

# Hay 5 registros con Playing = -1 (dato invalido), los dejamos afuera
playing_valido <- videojuegos$Playing[!is.na(videojuegos$Playing) &
                                        videojuegos$Playing >= 0]

summary(playing_valido)       # la mediana es 0: casi nadie esta jugando ahora
sd(playing_valido)            # desvío estándar
mean(playing_valido == 0)     # proporcion de juegos con 0 jugadores actuales

# Casi todos tienen 0 o 1 jugador, entonces un histograma comun se ve
# como una sola barra. Agrupamos en categorias:
grupos_playing <- cut(playing_valido,
                      breaks = c(-Inf, 0, 9, 99, Inf),
                      labels = c("0", "1 a 9", "10 a 99", "100 o más"))

# Guardamos la tabla en 'tabla_playing'
tabla_playing <- table(grupos_playing)          # cantidad de juegos en cada grupo

# --- Gráfico de Torta de Playing ---
colores <- c("#FFA1C2", "#FFF3B2", "#8FB3E8", "#2E5AA7")

# Calcular porcentajes para las etiquetas del gráfico de torta
porcentajes_playing <- round(tabla_playing / sum(tabla_playing) * 100, 1)
etiquetas_playing <- paste0(names(tabla_playing), " (", porcentajes_playing, "%)")

# Ajustar márgenes para el gráfico circular
par(mar = c(2, 2, 4, 2))

pie(tabla_playing,
    labels = etiquetas_playing,
    main = "Jugadores actuales por videojuego (Playing)",
    col = colores,
    border = "black",
    cex = 0.9)

# ==========================================
# ANÁLISIS DE PLATAFORMAS (Frecuencias y Porcentajes)
# ==========================================

# 1. Tabla de frecuencias y porcentajes
tabla_platforms <- videojuegos %>%
  separate_rows(Platforms, sep = ", ") %>%    # una fila por cada plataforma
  mutate(Platforms = trimws(Platforms)) %>%
  filter(!is.na(Platforms)) %>%               # saca los NA (juegos sin plataforma)
  count(Platforms, sort = TRUE) %>%           # Frecuencia absoluta (n)
  mutate(Porcentaje = round((n / sum(n)) * 100, 2))

head(tabla_platforms, 5)      # frecuencias y % del informe
nrow(tabla_platforms)         # cantidad de plataformas distintas

# 2. Gráfico de barras Top 10
top_platforms <- head(tabla_platforms, 10)

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
  separate_rows(Genres, sep = ", ") %>%       # una fila por cada género
  mutate(Genres = trimws(Genres)) %>%
  filter(!is.na(Genres)) %>%                  # saca los NA (juegos sin género)
  count(Genres, sort = TRUE) %>%
  mutate(Porcentaje = round((n / sum(n)) * 100, 2))

head(tabla_genres, 5)         # frecuencias y % del informe
nrow(tabla_genres)            # cantidad de géneros distintos

# 2. Gráfico de barras Top 10
top_generos <- head(tabla_genres, 10)

colores <- colorRampPalette(c("#081C15", "#40916C", "#B7E4C7"))(10)

barplot(top_generos$n,
        names.arg = top_generos$Genres,
        las = 2,
        main = "Top 10 Géneros más frecuentes",
        ylab = "Frecuencia",
        cex.names = 0.8,
        col = colores,
        border = colores[1])

par(mar = c(5, 4, 4, 2) + 0.1)   # volvemos al margen normal

# ======================= Género x Plataforma =======================
# Pregunta: ¿Qué combinaciones de género y plataforma tienen
# mayor consumo (Playing) y mejor valoración (Rating)?

# 1. Tabla cruzada (top 5 géneros x top 5 plataformas)
cruce <- videojuegos %>%
  separate_rows(Genres, sep = ", ") %>%
  separate_rows(Platforms, sep = ", ") %>%
  filter(Genres %in% top_generos$Genres[1:5],
         Platforms %in% top_platforms$Platforms[1:5],
         !is.na(Rating),
         !is.na(Playing), Playing >= 0) %>%   # saca los registros con Playing = -1
  group_by(Genres, Platforms) %>%
  summarise(n = n(),
            rating_prom  = mean(Rating),
            playing_prom = mean(Playing),
            .groups = "drop") %>%
  filter(n >= 30)   # evita promedios de pocos juegos

# 2. Top 5 combinaciones (para las conclusiones)
cruce %>% arrange(desc(rating_prom))  %>% slice(1:5)   # mejor valoradas
cruce %>% arrange(desc(playing_prom)) %>% slice(1:5)   # más consumidas

# 3. Matriz para barplot (filas = plataformas, columnas = géneros)
m_playing <- xtabs(playing_prom ~ Platforms + Genres, data = cruce)

# --- Gráfico: Playing promedio (consumo) ---
colores <- c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725")

barplot(m_playing,
        beside = TRUE,
        las = 2,
        col = colores,
        border = "white",
        ylim = c(0, max(m_playing) * 1.3),   # espacio para la leyenda
        legend.text = rownames(m_playing),
        args.legend = list(x = "topright", cex = 0.7),
        main = "Jugadores actuales promedio por género y plataforma",
        ylab = "Playing promedio",
        cex.names = 0.8)

