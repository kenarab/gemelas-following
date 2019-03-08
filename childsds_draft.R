install.packages("childsds")
install.packages("FSA")
library("childsds")
library("FSA")

who.ref

make_percentile_tab(ref = nl4.ref,
                    item = "heightM",
                    perc = c(5,50,95),
                    age = 1:3)


tab <- make_percentile_tab(ref = nl4.ref,
                                item = "heightM",
                                perc = c(5,50,95),
                                age = seq(0,2,by=1/24),
                                stack = T)
tab <- tab %>% filter(sex == "female")
tab$age.months <- round(tab$age*12,1)
nrow(tab)
head(tab)


library(ggplot2)
ggplot(tab, aes( x = age, y = value, group=paste(sex, variable))) +
  geom_line(aes(linetype = sex)) +
  #theme_classic() +
  theme(legend.position = c(0.1,0.8))


names(nl4.ref)


tab <- make_percentile_tab(ref = cdc.ref,
                           item = "weight",
                           perc = c(25,50,75),
                           age = seq(0,2,by=1/24),
                           stack = T)
tab <- tab %>% filter(sex == "female")
tab$age.months <- round(tab$age*12,1)


ggplot(tab, aes( x = age, y = value, group=paste(sex, variable))) +
  geom_line(aes(linetype = sex)) +
  #theme_classic() +
  theme(legend.position = c(0.1,0.8))
tab <- tab %>% arrange(age,sex, variable)


head(tab)
#

# Generation of estimates for growing rates
tab.tab <- dcast(tab, formula = age.months~variable, 
                 fun.aggregate = mean, value.var = "value")

tab.tab$days <- 30* tab.tab$age.months
tab.tab$diff.days <- c(NA, diff(tab.tab$days))
variables <- names(tab.tab)[2:4]
for (v in variables){
  rate.var <- paste("bi.month.rate", v, sep = "_")
  tab.tab[,rate.var] <- c(NA,lagratio(tab.tab[, v]))
  #tab.tab[,rate.var] <- c(NA,lagratio(tab.tab[, v])^(1/tab.tab[-1,"diff.days"]))
}

head(tab.tab, n =10)


