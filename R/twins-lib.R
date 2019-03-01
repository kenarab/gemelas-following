
makePrediction <- function(weights.data,
                           weights.data.norm,
                           date,
                           date.trend = weights.data[nrow(weights.data),"date"]){
  weights.data <- weights.data %>% filter(Obs!="Predicted")
  weights.data[,"diff.date"] <- c(NA,diff(weights.data[,"date"]))
  
  i <- nrow(weights.data)+1
  weights.data[i,"date"] <- as.Date(date)
  
  weights.data[i,"Obs"] <- "Predicted"
  
  #date.trend <- weights.data[i-1,"date"]
  
  last.daily.rate.sofia <- as.numeric(weights.data.norm %>% filter(date == date.trend, name == "Sofia") %>% select(daily.rate))
  last.daily.rate.margarita <- as.numeric(weights.data.norm %>% filter(date == date.trend, name == "Margarita") %>% select(daily.rate))
  
  print(paste("daily rate for Sofia",last.daily.rate.sofia))
  print(paste("daily rate for Margarita",last.daily.rate.margarita))
  
  weights.data[i,"Sofia"] <- round((1+last.daily.rate.sofia)^as.numeric(weights.data[i-1,"diff.date"]) * weights.data[i-1,"Sofia"], -1)
  weights.data[i,"Margarita"] <- round((1+last.daily.rate.margarita)^as.numeric(weights.data[i-1,"diff.date"]) * weights.data[i-1,"Margarita"], -1)
  weights.data
}
