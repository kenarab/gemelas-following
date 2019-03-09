library("stats")
library("zoo")

makePrediction <- function(weights.data,
                           weights.data.norm,
                           date,
                           date.trend = weights.data[nrow(weights.data),"date"],
                           delete.predicted = TRUE){
  if (delete.predicted){
    weights.data <- weights.data %>% filter(Obs!="Predicted")
  }
  
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


makePredictionsWithTables <- function(weights.data, 
                          weights.data.norm,
                          date.max,
                          date.trend,
                          zero.day = as.Date("2019-03-09"),
                          tab){
  weights.data <- weights.data %>% filter(Obs!="Predicted")
  
  days2predict <- seq(max(weights.data$date)+1, date.max, by = 1)
  days2predict.df <- data.frame(date = days2predict)
  #debug
  
  print(str(days2predict.df$date))
  days2predict.df$age <- as.numeric((days2predict.df$date-zero.day))/30
  tab.female <- tab %>% 
                filter(sex == "female") %>%
                filter(variable == "perc_50_0")
  days2predict.df <- days2predict.df %>% 
                      left_join(tab.female, by = "age")
  #days2predict.df$value.prev <- na.locf(days2predict.df$value)                      
  # days2predict.df$value <- stats::approx(days2predict.df$age, days2predict.df$value, 
  #                                        which(is.na(days2predict.df$value)))
  #                           
  tab.dates <- tab %>% filter(date %in% days2predict)
  
  for (i in seq_len(length(days2predict))){
    day2predict <- days2predict[i]
    print(paste("For date", day2predict))
    weights.data <- makePrediction(weights.data = weights.data,
                         weights.data.norm = weights.data.norm,
                         date = day2predict,
                         date.trend = date.trend,
                         delete.predicted = FALSE)
    weights.data.predicted <- weights.data %>% filter(date == day2predict)
    #debug
    weights.data.predicted <<- weights.data.predicted
    n <- nrow(weights.data.norm)+1
    weights.data.norm[n,"date"] <- day2predict
    weights.data.norm[n,"weight"] <- weights.data.predicted$Sofia
    weights.data.norm[n,"name"] <- "Sofia"
    weights.data.norm[n,"Obs"] <- "Predicted"
    
    n <- nrow(weights.data.norm)+1
    weights.data.norm[n,"date"] <- day2predict
    weights.data.norm[n,"weight"] <- weights.data.predicted$Margarita
    weights.data.norm[n,"name"] <- "Margarita"
    weights.data.norm[n,"Obs"] <- "Predicted"
    
    
    weights.data.norm[n,]
  }
  weights.data.norm %>% filter(Obs=="Predicted")
} 

mergeWithTablesPlot <- function(weights.data.norm,
                            zero.day = as.Date("2019-03-09"),
                            max.age = NULL,
                            weights.data.norm.predicted = NULL,
                            tab
){
  
  #Correct weights.data.norm
  weights.data.norm <- weights.data.norm %>% filter(Obs!="Predicted")
  
  weights.data.norm$age.months <- as.numeric((weights.data.norm$date-zero.day))/30
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
  if (!is.null(weights.data.norm.predicted)){
    weights.data.norm.predicted$age.months <- as.numeric((weights.data.norm.predicted$date-zero.day))/30
    weights.data.norm.predicted$weight.kg <- weights.data.norm.predicted$weight/1000
    
    ggplot <- ggplot +
      geom_line(data = weights.data.norm.predicted, 
                aes(x = age.months, y = weight.kg, 
                    group = name, color = name),
                linetype = "dashed")
  }
  
  ggplot <- ggplot +
    geom_line(data = tab, aes(x = age.months, y = value, group=paste(sex, variable), linetype = sex)) +
    #theme_classic() +
    theme(legend.position = c(0.1,0.8))
  ggplot
}