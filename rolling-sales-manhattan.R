setwd("D:/2018 Fall Baruch/CIS 3920 Data Mining")
getwd()
install.packages('readxl')
library(readxl)
rolling <- read_excel("rollingsales_manhattan.xls", col_names = FALSE)
sales <- rolling[-(1:4),]
colnames(sales) <- sales[1,]
sales <- sales[-1,]

library(dplyr)
library(tidyr)
install.packages('ggplot2')
library(ggplot2)
library(reshape2)

str(sales)
names(sales) <- c('Borough', 'Neighborhood', 'BuildingClassCategory', 
                  'TaxClassAtPresent', 'Block', 'Lot', 'EaseMent', 
                  'BuildingClassAtPresent', 'Address', 'ApartmentNumber', 
                  'ZipCode', 'ResidentialUnits', 'CommercialUnits', 
                  'TotalUnits', 'LandSquareFeet', 'GrossSquareFeet', 
                  'YearBuilt', 'TaxClassAtTimeOfSale', 
                  'BuildingClassAtTimeOfSale', 'SalePrice', 'SaleDate')
names(sales)
factors <- c("Borough", "Neighborhood", "BuildingClassCategory", 
             "TaxClassAtPresent", "BuildingClassAtPresent", 
             "TaxClassAtTimeOfSale", "BuildingClassAtTimeOfSale")
numerics <- c("ZipCode", "ResidentialUnits", "CommercialUnits", 
              "YearBuilt", "TotalUnits", "LandSquareFeet", 
              "GrossSquareFeet", "SalePrice")
sales <- sales %>% mutate_at(factors, as.factor) 
sales <- sales %>% mutate_at(numerics, as.numeric)
str(sales)
summary(sales[,-1])

transfer <- sales %>% 
  filter(SalePrice == 0 & ZipCode != 0 & 
         YearBuilt != 0 & GrossSquareFeet != 0)
head(transfer, n = 5)
str(transfer)

clean_sales <- sales %>%
  filter(SalePrice != 0 & ZipCode != 0 & 
         YearBuilt != 0 & GrossSquareFeet != 0)
summary(clean_sales[,-1])

num_var <- data.frame(clean_sales[,numerics[-1]])
par("mar")
par(mar=c(1,1,1,1))
pairs(num_var, log = "x")

#Sale Price by Manhattan Neighborhoods
ggplot(clean_sales, aes(x=Neighborhood, y=SalePrice, fill=Neighborhood, alpha=60)) +
       geom_boxplot() + ggtitle('Sale Prices in Manhattan Neigghborhoods') +
       xlab('Neighborhood') + ylab('Sale Prices') + 
       theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) + 
       scale_y_log10() 
