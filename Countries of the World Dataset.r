
#Load the data from my preferred directory
setwd("D:/2018 Fall Baruch/CIS 3920 Data Mining")
getwd()
countries <- read.csv("countries of the world.csv", header = TRUE)

#Take a look
head(countries, n = 5)
names(countries)

#Remove the Country and Region column because they are not variables
rownames <- countries[, 1:2]
countries <- countries[, -(1:2)]

#View the change
head(countries, n = 5)

#Get the numerical summary of the variables
summary(countries)

#Produce our scatterplot matrix
pairs(countries[, 1:10])

#Plot a side-by-side boxplots of GDP in all Regions
plot(rownames$Region, countries$GDP, 
     xlab = "Region", ylab = "GDP", 
     main = "GDP in Regions in the World")

#Create a variable called "CountryStatus" 
#by binning the "GDP....per.capita." variable

CountryStatus <- rep("Developing", nrow(countries))
CountryStatus[countries$GDP....per.capita. > 12000] <- "Developed"
CountryStatus <- as.factor(CountryStatus)

countries$CountryStatus <- CountryStatus
summary(countries$CountryStatus)

#Plot the GDP in Developed versis Developing Countries
plot(countries$CountryStatus, countries$GDP....per.capita., 
     xlab = "Country Status", ylab = "GDP per Capita",
     main = "GDP per Capita in Developed versus Developing Countries")

#Check the datatype 
str(countries)

#Create Histograms for Quantitative Variables
countries$Literacy.... <- as.numeric(countries$Literacy....)
countries$Deathrate <- as.numeric(countries$Deathrate)
countries$Pop..Density..per.sq..mi.. <- as.numeric(countries$Pop..Density..per.sq..mi..)
par(mfrow = c(2,2))
hist(countries$Pop..Density..per.sq..mi.., col = 4, xlab = "Population Density", ylab = "Count")
hist(countries$Deathrate, col = 10, xlab = "Deathrate", ylab = "Count")
hist(countries$GDP....per.capita., col = 5, xlab = "GDP Per Capita", ylab = "Count")
hist(countries$Literacy...., col = 6, xlab = "Literacy", ylab = "Count")

#install data visualization package
#install.packages("ggplot2")
library("ggplot2")

#Expore the variables
ggplot(countries, aes(x = Literacy...., y = GDP....per.capita.)) +
        geom_point()

summary(countries$Literacy....)

#Examine weird data
over100 <- countries[countries$Literacy.... > 100, ]
nrow(over100)
lower27 <- countries[countries$Literacy.... < 27, ]
nrow(lower27)

#Plotting with messy data
ggplot(data = countries, aes(x = Literacy...., y = GDP....per.capita.)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#Clean data
clean <- countries[countries$Literacy.... <= 100 & countries$Literacy.... > 27, ]
head(clean)
str(clean)

#Plotting with cleaned data
ggplot(data = clean, aes(x = Literacy...., y = GDP....per.capita.)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#What are the countries with the highest GDP per capita seen on the graph above?
rownames[clean$GDP....per.capita. > 15000 & CountryStatus == "Developed",]

#Highest Literacy Rate
rownames[max(clean$Literacy....),]

clean_developing <- clean[clean$CountryStatus == "Developing",]
str(clean_developing)
ggplot(data = clean_developing, 
       aes(x = Literacy...., y = GDP....per.capita.)) +
       geom_point() + geom_smooth(method = "lm", se = FALSE)

#Linear Model
lm(Literacy.... ~ GDP....per.capita., data = clean_developing)
