library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)
library("childsds")

source("R/twins-lib.R")

source.filename <- file.path("inst","extdata", "gemelas_neonatologia_pesos.csv")
weights.data <- read.csv(file = source.filename,
                         stringsAsFactors = FALSE)
#weights.data$Fecha <- as.Date(as.character(weights.data$Fecha), format ="%D%m%Y")
weights.data$date <- as.Date(as.character(weights.data$date))
weights.data$date
weights.data$Sofia <- as.numeric(weights.data$Sofia)
weights.data$Margarita <- as.numeric(weights.data$Margarita)
weights.data[is.na(weights.data$Obs),"Obs"] <- ""

weights.data <- weights.data[!is.na(weights.data$Sofia),]

str(weights.data)


weights.data$prev.date <- c(as.Date(NA), weights.data$date[-nrow(weights.data)])
weights.data$prev.date
weights.data$diff.date <- as.numeric(difftime(weights.data$date, weights.data$prev.date))


#normalized


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


weights.data.norm$bi.month.rate <- (1+weights.data.norm$daily.rate) ^ 15 -1



dcast(weights.data.norm,
      formula = name~date,
      value.var = "daily.rate",
      FUN = mean)

ggplot <- ggplot(data = weights.data.norm) +
  geom_line(aes(x = date, y = daily.rate, group = name, color = name))+
  geom_smooth(aes(x = date, y = daily.rate, group = name, color = name))+
  ggtitle("Sofia y Margarita- Daily rate")
ggplot
ggplotly(ggplot)
ggsave(paste("~/gemelas_daily_rate.png"), ggplot)


ggplot <- ggplot(data = weights.data.norm) +
  geom_line(aes(x = date, y = log.weight, group = name, color = name))+
  geom_smooth(aes(x = date, y = log.weight, group = name, color = name))+
  ggtitle("Sofia y Margarita- Log weight base 2")
#g
ggplot
ggsave(paste("~/gemelas_log_weight.png"), ggplot)

ggplot <- ggplot(data = weights.data.norm) +
  geom_line(aes(x = date, y = weight, group = name, color = name))+
  geom_smooth(aes(x = date, y = weight, group = name, color = name))+
  ggtitle("Sofia y Margarita- Weight")
#g
ggplot
ggsave(paste("~/gemelas_weight.png"), ggplot)






#projection
weights.data <- makePrediction(weights.data, 
                               weights.data.norm,
                               date = "2019-03-03",
                               date.trend = "2019-02-22") 

tail(makePrediction(weights.data, 
                               weights.data.norm,
                               date = "2019-03-10",
                               date.trend = "2019-03-04"), 
     n =2)

tail(makePrediction(weights.data, 
                     weights.data.norm,
                     date = "2019-03-10",
                     date.trend = "2019-02-22"), n =2)


tail(makePrediction(weights.data, 
                    weights.data.norm,
                    date = "2019-03-15",
                    date.trend = "2019-03-04"), 
     n =2)




12*8*2

(1+last.daily.rate.sofia)^12 * weights.data[i-1,"Sofia"]
(1+last.daily.rate.margarita)^12 * weights.data[i-1,"Margarita"]

12*8*2

Sys.Date()+12

# Different available tables
#https://github.com/mvogel78/childsds/wiki/Weight

tab <- make_percentile_tab(ref = cdc.ref,
                           item = "weight",
                           perc = c(25,50,75),
                           age = seq(0,2,by=1/24),
                           stack = T)


ggplot <- mergeWithTablesPlot(weights.data.norm = weights.data.norm,
                    max.age = 2,
                    tab = tab)
ggplot
ggsave(paste("~/gemelas_weight_against_tables.png"), ggplot)
