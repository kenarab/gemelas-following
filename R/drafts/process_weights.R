library(ggplot2)
library(reshape2)

source.filename <- paste("~/gemelas_neonatologia_pesos.csv")
weights.data <- read.csv(file = source.filename)
#weights.data$Fecha <- as.Date(as.character(weights.data$Fecha), format ="%D%m%Y")
weights.data$date <- as.Date(as.character(weights.data$date))
weights.data$date
weights.data$Sofia <- as.numeric(weights.data$Sofia)
weights.data$Margarita <- as.numeric(weights.data$Margarita)
str(weights.data)


weights.data$prev.date <- c(as.Date(NA), weights.data$date[-nrow(weights.data)])
weights.data$prev.date
weights.data$diff.date <- difftime(weights.data$date, weights.data$prev.date)




weights.data.norm <- weights.data[,c("date", "Sofia", "diff.date")]
names(weights.data.norm)[2] <- "weight"
weights.data.norm$name <- "Sofia"
weights.data.norm$prev.weight <- c(as.numeric(NA), weights.data.norm$weight[-nrow(weights.data)])
weights.data.norm$rel.weight <- weights.data.norm$weight / weights.data.norm$prev.weight


weights.data.norm.margarita <- weights.data[,c("date", "Margarita", "diff.date")]
names(weights.data.norm.margarita)[2] <- "weight"
weights.data.norm.margarita$name <- "Margarita"
weights.data.norm.margarita$prev.weight <- c(as.numeric(NA), weights.data.norm.margarita$weight[-nrow(weights.data)])
weights.data.norm.margarita$rel.weight <- weights.data.norm.margarita$weight/ weights.data.norm.margarita$prev.weight

weights.data.norm <- rbind(weights.data.norm, weights.data.norm.margarita)
weights.data.norm
weights.data.norm$daily.rate <- round(weights.data.norm$rel.weight^
                                             (1/as.numeric(weights.data.norm$diff.date))-1,
                                           4)
weights.data.norm$log.weight <- log(weights.data.norm$weight, base =2)

dcast(weights.data.norm,
      formula = name~date,
      value.var = "daily.rate",
      FUN = mean)

ggplot <- ggplot(data = weights.data.norm) +
  geom_line(aes(x = date, y = daily.rate, group = name, color = name))+
  ggtitle("Sofia y Margarita- Daily rate")
ggplot
ggsave(paste("~/gemelas_daily_rate.png"), ggplot)


ggplot <- ggplot(data = weights.data.norm) +
  geom_line(aes(x = date, y = log.weight, group = name, color = name))+
  ggtitle("Sofia y Margarita- Log weight")
#g
ggplot
ggsave(paste("~/gemelas_log_weight.png"), ggplot)





ggplot <- ggplot(data = weights.data) +
          ggtitle("Sofia y Margarita- weights") +
            geom_line(aes(x = date, y = Sofia, color="Sofia")) + #, label = "Sofia"
            geom_line(aes(x = date, y = Margarita, color="Margarita")) #, label = "Margarita"
ggplot

ggsave(paste("~/gemelas_pesos.png"), ggplot)

plot(weights.data$Fecha, weights.data$Sofia, type = "l")
