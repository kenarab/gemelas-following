
makePrediction <- function(weights.data,
                           weights.data.norm,
                           date,
                           date.trend = weights.data[nrow(weights.data),"date"]){
  weights.data <- weights.data %>% filter(Obs!="Predicted")
  
  i <- nrow(weights.data)+1
  weights.data[i,"date"] <- as.Date(date)
  
  weights.data[i,"Obs"] <- "Predicted"
  
  weights.data[,"diff.date"] <- c(NA,diff(weights.data[,"date"]))
  
  
  #date.trend <- weights.data[i-1,"date"]
  
  last.daily.rate.sofia <- as.numeric(weights.data.norm %>% filter(date == date.trend, name == "Sofia") %>% select(daily.rate))
  last.daily.rate.margarita <- as.numeric(weights.data.norm %>% filter(date == date.trend, name == "Margarita") %>% select(daily.rate))
  
  print(paste("daily rate for Sofia",last.daily.rate.sofia))
  print(paste("daily rate for Margarita",last.daily.rate.margarita))
  
  weights.data[i,"Sofia"] <- round((1+last.daily.rate.sofia)^as.numeric(weights.data[i,"diff.date"]) * weights.data[i-1,"Sofia"], -1)
  weights.data[i,"Margarita"] <- round((1+last.daily.rate.margarita)^as.numeric(weights.data[i,"diff.date"]) * weights.data[i-1,"Margarita"], -1)
  weights.data
}


mergeWithTablesPlot <- function(weights.data.norm,
                            zero.day = as.Date("2019-03-09"),
                            max.age = NULL,
                            tab
){
  #Correct weights.data.norm
  weights.data.norm$age.months <- as.numeric((weights.data$date-zero.day))/30
  weights.data.norm$weight.kg <- weights.data.norm$weight/1000
  #Correct tab
  tab$age.months <- round(tab$age*12,1)
  
  if (!is.null(max.age)){
    tab <- tab %>% filter(age.months <= max.age)
  }
  ggplot <- ggplot(data = weights.data.norm) +
    geom_line(aes(x = age.months, y = weight.kg, group = name, color = name))+
    geom_smooth(aes(x = age.months, y = weight.kg, group = name, color = name))+
    ggtitle("Sofia y Margarita- Observed weight against p25 - p50 - p75")
  ggplot <- ggplot +
    geom_line(data = tab, aes(x = age.months, y = value, group=paste(sex, variable), linetype = sex)) +
    #theme_classic() +
    theme(legend.position = c(0.1,0.8))
  ggplot
}