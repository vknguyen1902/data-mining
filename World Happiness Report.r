
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("D:/2018 Fall Baruch/CIS 3920 Data Mining")
getwd()

happiness <- read.csv("2017.csv")
head(happiness)

str(happiness)

happiness <- happiness[, -c(4,5)]

colnames(happiness) <- c("Country", "Happiness.Rank", "Happiness.Score", 
                         "Economy", "Family", "Life.Expectancy", "Freedom", 
                         "Generosity", "Trust", "Dystopia.Residual")
names(happiness)

happiness$Continent <- NA

asia <- c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
          "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
          "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
          "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
          "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
          "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
          "Cambodia", "Afghanistan", "Yemen", "Syria")

europe <- c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
            "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
            "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
            "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
            "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
            "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
            "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
            "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
            "Bulgaria", "Albania", "Ukraine")

north.america <- c("Canada", "Costa Rica", "United States", "Mexico",  
                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras", "Haiti")

south.america <- c("Chile", "Brazil", "Argentina", "Uruguay", "Colombia", 
                   "Ecuador", "Bolivia", "Peru", "Paraguay", "Venezuela")

australia <- c("New Zealand", "Australia")

happiness$Continent[which(happiness$Country %in% asia)] <- "Asia"
happiness$Continent[which(happiness$Country %in% europe)] <- "Europe"
happiness$Continent[which(happiness$Country %in% north.america)] <- "North America"
happiness$Continent[which(happiness$Country %in% south.america)] <- "South America"
happiness$Continent[which(happiness$Country %in% australia)] <- "Australia"
happiness$Continent[which(is.na(happiness$Continent))] <- "Africa"

happiness <- happiness %>% select(Country, Continent, Happiness.Rank, Happiness.Score, 
                                  Economy, Family, Life.Expectancy, Freedom, 
                                  Generosity, Trust, Dystopia.Residual) 

happiness$Continent <- as.factor(happiness$Continent)

str(happiness)

#View the cleaned data
head(happiness, n = 5)

#Separate country names from the variables
rownames <- happiness[, 1]
happiness <- happiness[, -1]
head(happiness, n = 5)

#Numerical Summary
summary(happiness)

#Scatterplot Matrix
pairs(happiness[, 1:10])

#Side-by-side Boxplot
ggplot(happiness, aes(x = Continent, y = Happiness.Score, fill = Continent, alpha = 60)) +
       geom_boxplot() + ggtitle("Happiness Scores Accross Continents") +
       xlab("Continents") + ylab("Happiness Scores") + theme_bw()

#Happiness Score Histogram
ggplot(happiness, aes(Happiness.Score, fill = Continent, color = Continent)) + 
       geom_histogram(aes(alpha = 60), bins = 10) +
       ggtitle("Distribution of Happiness Scores") +
       xlab("Happiness Score") + ylab("Count of Countries") + theme_bw()

#Create qualitative variable
Status <- rep("Neutral", nrow(happiness))
Status[happiness$Happiness.Rank <= 25] <- "Utopia"
Status[happiness$Happiness.Rank > 130] <- "Distopia"
Status <- as.factor(Status)

happiness$Status <- Status
summary(happiness$Status)

#Boxplot for Happiness Status
ggplot(happiness, aes(x = Status, y = Economy, fill = Continent)) +
       geom_boxplot(aes(alpha = 60)) + theme_bw() +
       xlab("Happiness Status") + ylab("Importance of Economy") +
       ggtitle("Happiness Status and Importance of Economy")

#Histogram for variables
par(mfrow = c(2,2))
hist(happiness$Economy, col = 4, xlab = "Economy", ylab = "Count")
hist(happiness$Life.Expectancy, col = 10, xlab = "Life Expectancy", ylab = "Count")
hist(happiness$Family, col = 5, xlab = "Family", ylab = "Count")
hist(happiness$Freedom, col = 6, xlab = "Freedom", ylab = "Count")

ggplot(subset(happiness, happiness$Continent != "Australia"), aes(x = Life.Expectancy, y = Happiness.Score)) + 
       geom_point(aes(color=Continent), size = 3, alpha = 0.6) +  
       geom_smooth(aes(color = Continent, fill = Continent), 
                   method = "lm", fullrange = TRUE) +
       facet_wrap(~Continent) +
       theme_bw() + labs(title = "Happiness Score vs. Life Expectancy")

ggplot(subset(happiness, happiness$Continent != "Australia"), aes(x = Economy, y = Happiness.Score)) + 
       geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
       geom_smooth(aes(color = Continent, fill = Continent), 
                   method = "lm", fullrange = TRUE) +
       facet_wrap(~Continent) +
       theme_bw() + labs(title = "Happiness score vs. Economy")
