library(ggplot2)
library(ggthemes)
library(dplyr)
library(readxl)
require(scales)
options(scipen = 999)


# REVISION

library(readr)
# RSL
datos <- read_csv("...")

agno <- datos$Year
ID <- 1967:2020

rsl <- datos$Count
df <- data.frame(agno, rsl)

df %>%
  ggplot(aes(x = agno, y = rsl)) +
  geom_line(size=1.5) +
  scale_x_continuous(labels = as.character(ID), breaks = ID)+
  scale_y_continuous(limits=c(0, max(datos$Count))) +
  labs( x = "Año", y = "RSL publicadas",
        title = "Evolución del número revisiones sistemáticas de la literatura publicadas en PubMed",
        subtitle = "Fuente: PubMed") +
  theme_igray()+
  theme(legend.position = "bottom") 

# METAANALISIS
datos <- read_csv("...")

agno <- datos$Year
ID <- 1982:2020

meta <- datos$Count
df <- data.frame(agno, meta)

df %>%
  ggplot(aes(x = agno, y = meta)) +
  geom_line(size=1.5) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5))+
  scale_y_continuous(limits=c(0, max(datos$Count))) +
  labs( x = "Año", y = "RSL publicadas",
        title = "Evolución del número de metaanálisis publicados en PubMed",
        subtitle = "Fuente: PubMed") +
  theme_igray()+
  theme(legend.position = "bottom") 




# CON PLOT NORMAL
datos <- read_csv("...")

plot(datos$Year, datos$Count, type = "o", lwd = 2, col = "deepskyblue4", xlab = "Año", ylab = "Publicaciones", main = "RSL publicadas en PubMed")
grid()

# META-ANALISIS
datos <- read_csv("...")

plot(datos$Year, datos$Count, type = "o", lwd = 2, col = "deepskyblue4", xlab = "Año", ylab = "Publicaciones", main = "Metaanálisis publicadas en PubMed")
grid()




############### Tipo de publicacion

datos <- read_xlsx("...")
df <- datos$año
# Eliminamos los valores NA
df <- df[!is.na(df)]
frecuencias <- as.data.frame(table(df))
frecuencias$df=as.numeric(levels(frecuencias$df))[frecuencias$df]

frecuencias %>%
  ggplot(aes(x = df, y = Freq)) +
  geom_line(size=1.5) +
  scale_x_continuous(labels = as.character(df), breaks = df)+
  scale_y_continuous(limits=c(0, max(frecuencias$Freq))) +
  labs( x = "Año", y = "Número de publicaciones",
        title = "Evolución de la literatura sanitaria en la que se emplean eventos discretos") +
  theme_igray()+
  theme(legend.position = "bottom") 




datos <- read_xlsx("...")

df2 <- as.data.frame(datos$año, datos$tipo)

library(reshape2)

df2.new <- dcast(df2, tipo ~ año, fun.aggregate = length)
df2.new
datos <- read_xlsx("...")

datos %>%
  filter(tipo %in% c("Investigación aplicada", "Caso de estudio", "Libro", "Revisión sistemática", "Teórico-conceptual", "Survey")) %>%
  ggplot(aes(x = año, y = acumuladas, group = tipo, colour = tipo)) +
  geom_line(size=1.5) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+
  theme_light() +
  theme(axis.text.x = element_text(size=12, angle=0,face="bold"),
        axis.text.y = element_text(size=12, angle=0,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.position = "bottom") +
  labs(x = "Año", y = "Número de publicaciones",
       title = "Publicaciones acumuladas de la literatura sanitaria en la que se emplean eventos discretos por tipo de artículo") +
  scale_color_manual(values = c("#88CCEE", "#117733", "#DDCC77", "#CC6677", "#882255", "#888888"),
                     name = "Tipo de artículo",
                     labels = c("Investigación aplicada", "Caso de estudio", "Libro", "Revisión sistemática", "Survey", "Teórico-conceptual"))



datos %>%
  filter(tipo %in% c("Investigación aplicada", "Caso de estudio", "Libro", "Revisión sistemática", "Teórico-conceptual", "Survey")) %>%
  ggplot(aes(x = año, y = acumuladas, group = tipo, colour = tipo)) +
  geom_line(size=1.5) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+
  labs(x = "Año", y = "Número de publicaciones",
       title = "Publicaciones acumuladas de la literatura sanitaria en la que se emplean eventos discretos por tipo de artículo") +
  scale_color_manual(values = c("#88CCEE", "#117733", "#DDCC77", "#CC6677", "#882255", "#888888"),
                     name = "Tipo de artículo",
                     labels = c("Investigación aplicada", "Caso de estudio", "Libro", "Revisión sistemática", "Survey", "Teórico-conceptual"))+
  theme_igray()+
  theme(legend.position = "bottom") 


# Paises
datos <- read_xlsx("...")
df <- datos$pais
# Eliminamos los valores NA
df <- df[!is.na(df)]
frecuencias <- as.data.frame(table(df))


#  top 10 paises
datos <- read_xlsx("...")
df <- datos$pais
# Eliminamos los valores NA
df <- df[!is.na(df)]
df <- as.data.frame(table(df))
df <- df[order(df[,"Freq"], decreasing = T),]

df$relative <- round(df$Freq/sum(df$Freq)*100,1)
df

df <- df[1:10,]

text <- "
country,Freq
USA,  153
UK,   85
Canada,   37
Italy,   26
Netherlands,   20
Australia,   18
Spain,   16
France,   14
Germany,   12
China,   10
"
df <- read.table(textConnection(text), sep=",", header = T, stringsAsFactors = F)


df <- arrange(df, Freq)
df$country <- factor(df$country, levels = df$country)




ggplot(df, aes(country, Freq, fill = country)) +
  scale_y_continuous(breaks = seq(0, max(df$Freq), by = 10))+
  labs(x = "País", y = "Número de publicaciones",
       title = "Número de publicaciones por países") +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#661100", "#888888"),
                    name = "Tipo de artículo")+
  theme_igray()+
  geom_col() + coord_flip() 


# creamos el nuevo xlsx manualmente con los 10 primeros y el resto como: otros
datos <- read_xlsx("...")


datos %>%
  mutate(paises = reorder(paises, Freq)) %>%
  ggplot(aes(paises, Freq)) +
  scale_y_continuous(breaks = seq(0, max(datos$Freq), by = 10))+
  geom_bar(stat="identity", width = 0.6) +
  coord_flip() +
  theme_light() +
  # theme(axis.text.y = element_text(size = 6)) +
  theme(panel.grid.major.y = element_blank(),
        # panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=12, angle=0,face="bold"),
        axis.text.y = element_text(size=12, angle=0,face="bold"),
        axis.title=element_text(size=14,face="bold")) +
  geom_text(aes(y = Freq, label = paste0(Freq)),
            size = 4.5, family = "Bahnschrift", nudge_y = 2.5) +
  labs(x = "País", y = "Número de publicaciones") 



datos <- read_xlsx("...")
df <- datos$pais
# Eliminamos los valores NA
df <- df[!is.na(df)]
df <- as.data.frame(table(df))
df <- df[order(df[,"Freq"], decreasing = T), ]


################## Para obtener el continente
library(countrycode)
df$continent <- countrycode(sourcevar = df[, "df"],
                            origin = "country.name",
                            destination = "continent")
df <- df[,2:3]

# resultado:
df %>% 
  group_by(continent) %>% 
  summarise_all(funs(sum))

# Africa        6 (1%)
# North America    206  (40%)
# South America   6 (1%)
# Asia         63 (12%)
# Europe      216 (42%)
# Oceania      22 (4%)


################## SOFTWARE

datos <- read_xlsx("...")
df <- datos$software
# Eliminamos los valores NA
df <- df[!is.na(df)]
df <- as.data.frame(table(df))
df <- df[order(df[,"Freq"], decreasing = T), ]
df$relative <- round(df$Freq/sum(df$Freq)*100,1)
df
# creamos el nuevo xlsx manualmente con los 10 primeros y el resto como: otros
datos <- read_xlsx("...")

doughnut <-
  function (x, labels = datos$software1, edges = 200, outer.radius = 0.8,
            inner.radius=0.6, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL,
            angle = 45, col = NULL, border = FALSE, lty = NULL,
            main = NULL, ...)
  {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels))
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
      col <- if (is.null(density))
        c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
          "#44AA99", "#999933", "#882255", "#661100", "#888888")
    else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p),
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i],
              angle = angle[i], border = border[i],
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0),
             font = 2)
        #cex=1.1)
      }
      ## Agnadir disco blanco        
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i],
              angle = angle[i], border = border[i],
              col = "white", lty = lty[i])
    }
    
  }

datos <- read_xlsx("...")

doughnut(datos$Freq, inner.radius=0.5)


# ################################ RESULTADOS DE LA EXPERIENCIA
datos <- read_xlsx("...")
df <- datos$resultados
# Eliminamos los valores NA
df <- df[!is.na(df)]
df <- as.data.frame(table(df))
df <- df[order(df[,"Freq"], decreasing = T), ]
df
# creamos el nuevo xlsx manualmente con los 10 primeros y el resto como: otros
datos <- read_xlsx("...")


library(tidyverse)
library(dslabs)
library(gridExtra)


datos %>%
  mutate(experiencia = reorder(experiencia, Freq)) %>%
  ggplot(aes(experiencia, Freq)) +
  scale_y_continuous(breaks = seq(0, max(datos$Freq), by = 10))+
  geom_bar(stat="identity") +
  coord_flip() +
  theme_light() +
  # theme(axis.text.y = element_text(size = 6)) +
  theme(panel.grid.major.y = element_blank(),
        # panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14, angle=0),
        axis.text.y = element_text(size=14, angle=0, face = "bold"),
        axis.title=element_text(size=14,face="bold")) +
  geom_text(aes(y = Freq, label = paste0(Freq)),
            size = 4.5, family = "Bahnschrift", nudge_y = 1) +
  labs(x = "Resultados de la experiencia", y = "Número de publicaciones") 


# ############################## DEPARTAMENTOS
datos <- read_xlsx("...")
df <- datos$departamento
# Eliminamos los valores NA
df <- df[!is.na(df)]
df <- as.data.frame(table(df))
df <- df[order(df[,"Freq"], decreasing = T), ]
df


# creamos el nuevo xlsx manualmente con los 10 primeros y el resto como: otros
datos <- read_xlsx("...")


datos %>%
  arrange(publicaciones) %>%   
  mutate(name=factor(departamento, levels=departamento)) %>%  
  ggplot( aes(x=name, y=publicaciones)) +
  scale_y_continuous(breaks = seq(0, max(datos$publicaciones), by = 10))+
  geom_segment( aes(xend=name, yend=0), color="darkgrey") +
  geom_point( size=3, color="black") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    # panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=12, angle=0,face="bold"),
    axis.text.y = element_text(size=14, angle=0,face="bold"),
    axis.title=element_text(size=14,face="bold"))+
  labs(x = "Servicios médicos", y = "Número de publicaciones")
