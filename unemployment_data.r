library(ggplot2)
library(dplyr)
library(readr)
library(rworldmap)
library(maps)
library(scales)

dataset = read.csv('API_ILO_country_YU.csv')
head(dataset)

summary(dataset)
str(dataset)

dataset = mutate(dataset, avgrate= (X2010+ X2011 +X2012 +X2013 +X2014)/5)

#dataset %>% mutate(
# avgrate = (2010 +2011 +2012 +2013 +2014)/5
#)

head(dataset)

dataset = arrange(dataset,desc(avgrate))

head(dataset)

# corelation of unemployemnet between different years

correlation = cor(dataset[,3:7])
head(correlation)

dataset1 = mutate(dataset, sector = 4) #New column sector to define severity of unemployment.
head(dataset1)

#declaring the levels of unemployment
dataset1$sector[dataset1$avgrate<15]=1
dataset1$sector[dataset1$avgrate>15 & dataset1$avgrate<30]=2
dataset1$sector[dataset1$avgrate>30 & dataset1$avgrate<45]=3
dataset1$sector[dataset1$avgrate>45]=4

head(dataset1)
str(dataset1)

by_sector = group_by(dataset1, sector)

count = summarise(by_sector, num=n(), meanrate = mean(avgrate, na.rm = TRUE)) #no. of countries
head(count)
plot(count)

n= data.frame(country= dataset1$Country.Name, value= dataset1$avgrate)

p <- joinCountryData2Map(n, joinCode="NAME", nameJoinColumn="country")
                        

mapCountryData(p, nameColumnToPlot="value", mapTitle="World Average Unemployment Rate")

hist(dataset1$avgrate,
     main = 'Histogram of Average Rate',
     xlab = 'Average rate',
     border ="black",
     col = "blue")
b <- ggplot(dataset1, aes(x=Country.Code, y= avgrate, colour = sector))+
    geom_bar(stat = "identity")
b
