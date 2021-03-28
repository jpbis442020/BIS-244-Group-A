library(tidyverse)
library(stringr)
library(utils)
library(dplyr)


NYTdata<-read.csv("us-counties.csv")
#View(NYTdata)
censusdata<-read.csv("Average_Household_Size_and_Population_Density_-_County.csv")
#View(censusdata)
collegescovid<-read.csv("colleges.csv")
#View(collegescovid)

dataneeded<-select(collegescovid, county, cases, college, state)
#View(dataneeded)
cleanedNYTcountycases<- select(NYTdata, date, county, cases, fips)
#View(cleanedNYTcountycases)
cleanedcensusdata<- select(censusdata, NAME, B01001_001E, State, GEOID)
#View(cleanedcensusdata)

names(cleanedcensusdata)[2] <- "totalpopulation"
names(dataneeded)[2] <- "cvoidcasesincollege"
#View(cleanedcensusdata)


cleanedcensusdata <- cleanedcensusdata %>% separate(col=NAME,into=c("county", "delete"),"Count",extra="merge")
#View(cleanedcensusdata)
cleanedcensusdata$delete <- NULL

dataneeded <- dataneeded[order(dataneeded$"county"),]
View(dataneeded)
cleanedcensusdata <- cleanedcensusdata[order(cleanedcensusdata$"county"),]
View(cleanedcensusdata)

dataneeded<-dataneeded %>% distinct(state, county, .keep_all = TRUE)
View(dataneeded)
names(cleanedcensusdata)[3] <- "state"


NYTdata <- NYTdata[order(NYTdata$"fips"),]
#View(NYTdata)
NYTdata$deaths<-NULL
NYTdata = NYTdata[NYTdata$date == "2020-11-14",]
View(NYTdata)

names(cleanedcensusdata)[4] <- "fips"
cleanedcensusdata <- cleanedcensusdata[order(cleanedcensusdata$"fips"),]
#View(cleanedcensusdata)

totdata<- left_join(cleanedcensusdata,NYTdata,by = c("fips" = "fips"))
totdata$county.x<-NULL
totdata$state.x<-NULL
names(totdata)[4] <- "county"
names(totdata)[5] <- "state"
#View(totdata)

totdata <- transform(totdata, casestotalpoppercentage = cases / totalpopulation)
totdata <- transform(totdata, casestotalpoppercentage = casestotalpoppercentage*100)
totdata$countystate <- paste(totdata$county,totdata$state)
#totdata<- totdata[complete.cases(totdata), ]
View(totdata)

#assist
dataneeded$countystate <- paste(dataneeded$county,dataneeded$state)
dataneeded<- dataneeded %>% add_column(ifcollege = 1)
view(dataneeded)
cleanedcensusdata$countystate <- paste(cleanedcensusdata$county,cleanedcensusdata$state)
view(cleanedcensusdata)
dataneeded$college<-NULL
dataneeded$covidcasesincollege<-NULL
joineddata <- left_join(totdata,cleanedcensusdata,by = c("countystate" = "countystate"))
joineddata$county.y<-NULL
joineddata$totalpopulation.y<-NULL
joineddata$state.y<-NULL
joineddata$fips.y<-NULL
joineddata <- left_join(joineddata,dataneeded,by = c("countystate" = "countystate"))
joineddata[is.na(joineddata)] = 0
list1 <- c(77, 95, 549, 1831, 1852, 1859, 1869, 1871, 2674)
joineddata <- joineddata[-list1,]
joineddata$county<-NULL
joineddata$cvoidcasesincollege<-NULL
joineddata$state<-NULL
joineddata$fips.x<-NULL
joineddata$date<-NULL
finalizeddata<-joineddata
joineddata$county.x<-NULL
joineddata$state.x<-NULL
view(joineddata)
finalizeddata <- finalizeddata[order(finalizeddata$"ifcollege"),]

library(readr)
povertydata <- read_csv("povertydata.csv")
View(povertydata)

povertydata$combinedname <- paste(povertydata$Area_name,povertydata$Stabr)

povertydataandschools <- left_join(povertydata,finalizeddata,by = c("combinedname" = "countystate"))
povertydataandschools <-povertydataandschools %>%
  select(totalpopulation.x, ifcollege, peopleinpoverty, combinedname)

povertydataandschools <-povertydataandschools[complete.cases(povertydataandschools), ]

povertydataandschools$popdivpoverty <- (povertydataandschools$peopleinpoverty / povertydataandschools$totalpopulation.x)
3.500636e+07/2.896963e+08
0.1208381


p<-ggplot(data=povertydataandschools, aes(x=combinedname, y=popdivpoverty, color=ifcollege)) +
  geom_bar(stat="identity")
p

colSums(Filter(is.numeric, finalizeddata))
#totnotcollege=2304
#totcolleges=907

aggregate(x= finalizeddata$casestotalpoppercentage,by= list(finalizeddata$ifcollege),FUN=sum)
#totpercasesnocollege=8387.258
#totpercasescollege=3146.037
#8387.258/2304
#3146.037/907
#view(finalizeddata)

#H <- c(3.47,3.64)
#M <- c("With Colleges", "Without Colleges")



#barplot(H,names.arg = M, xlab= "Counties", ylab= "Average Population Case %", col = "blue", main= "Case Percentages", border = "red")




