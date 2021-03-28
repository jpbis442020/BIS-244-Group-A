library(tidyverse)
library(stringr)
library(utils)
library(dplyr)
library(readr)
statepopandvote <- read_csv("statepopandvote.csv")
View(statepopandvote)
PovertyEstimates <- read_csv("PovertyEstimates.csv")
View(PovertyEstimates)
PovertyEstimates <- left_join(PovertyEstimates,statepopandvote,by = c("Area_name" = "State"))
PovertyEstimates <-PovertyEstimates %>%
  select(Area_name, POVALL_2019, `2018 Population`, `2020vote`)
PovertyEstimates$popdivpoverty <- (PovertyEstimates$POVALL_2019 / PovertyEstimates$`2018 Population`)
3.500636e+07/2.896963e+08
averagepov=0.1208381
p<-ggplot(data=PovertyEstimates, aes(x=Area_name, y=popdivpoverty, fill=`2020vote`)) +
  
  geom_bar(stat="identity")+  labs(x = "state",
                                   y = "rate of poverty",title = "Which states voted for who vs average poverty rate")+
  
  geom_hline(yintercept=0.1208381, linetype="dashed", color = "red")

p