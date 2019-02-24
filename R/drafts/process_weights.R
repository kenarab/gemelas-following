library(ggplot2)

source.filename <- paste("~/gemelas_neonatologia_pesos.csv")
weights.data <- read.csv(file = source.filename)
#weights.data$Fecha <- as.Date(as.character(weights.data$Fecha), format ="%D%m%Y")
weights.data$date <- as.Date(as.character(weights.data$date))
weights.data$date
weights.data$Sofia <- as.numeric(weights.data$Sofia)
weights.data$Margarita <- as.numeric(weights.data$Margarita)
str(weights.data)

ggplot <- ggplot(data = weights.data) +
            geom_line(aes(x = date, y = Sofia, color="Sofia")) + #, label = "Sofia"
            geom_line(aes(x = date, y = Margarita, color="Margarita")) #, label = "Margarita"
ggplot

ggsave(paste("~/gemelas_pesos.png"), ggplot)

plot(weights.data$Fecha, weights.data$Sofia, type = "l")
