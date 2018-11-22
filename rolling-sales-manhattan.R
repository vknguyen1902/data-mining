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

his_price <- ggplot(clean_sales, aes(clean_sales$SalePrice)) + 
  geom_histogram(fill="orange", alpha = 0.6, bins = 50) + 
  scale_x_log10() + ggtitle('Distribution of Sale Price') + 
  xlab('Sale Price') + ylab('Frequency') + theme_bw()

bpl_price <- ggplot(clean_sales, aes(y=clean_sales$SalePrice, x='Borough')) +
  geom_boxplot(fill = 'red', alpha = 0.6) + scale_y_log10() + 
  ggtitle('Boxplot of Sale Price') +
  xlab('Manhattan') + ylab('Sale Prices') + theme_bw()

install.packages("cowplot", repos = "http://cran.us.r-project.org")

library(cowplot)

plot_grid(his_price, bpl_price, ncol = 2)

#Remove outliers
x <- clean_sales$SalePrice
qnt <- quantile(x, probs=c(.25,.75),na.rm = TRUE)
H <- 1.5 * IQR(x, na.rm = TRUE)
low <- which(x < (qnt[1])) 
up <- which(x > (qnt[2] + H))

no_outlier_sales <- clean_sales
no_outlier_sales <- no_outlier_sales[-c(low,up),]

no_his_price <- no_outlier_sales %>%
  ggplot(aes(no_outlier_sales$SalePrice)) + 
  geom_histogram(fill="orange", alpha = 0.6, bins = 50) + scale_x_log10() + 
  ggtitle('Distribution of Sale Price') + 
  xlab('Sale Price') + ylab('Frequency') + theme_bw()

no_bpl_price <- no_outlier_sales %>% 
  ggplot(aes(y=no_outlier_sales$SalePrice, x='Borough')) +
  geom_boxplot(fill = 'red', alpha = 0.6) + scale_y_log10() + 
  ggtitle('Boxplot of Sale Price') +
  xlab('Manhattan') + ylab('Sale Prices') + theme_bw()

plot_grid(no_his_price, no_bpl_price)
summary(no_outlier_sales$SalePrice)

#Sale Price by Neighborhood
ggplot(no_outlier_sales, aes(x = Neighborhood, y = SalePrice, 
                             fill = Neighborhood, alpha = 60)) +
  geom_boxplot() + ggtitle('Sale Price Accross Neighborhood') +
  xlab('Neighborhoods') + ylab('Sale Price') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = 'none') + scale_y_log10() + coord_flip()

#Extract quantitative variables
num_var <- data.frame(no_outlier_sales[,numerics[-1]])
head(num_var)

#Overview quantitative variables
num_var %>%
  select(TotalUnits, YearBuilt, LandSquareFeet, 
         GrossSquareFeet, SalePrice) %>%
  pairs(col='navy')

#Create function to remove outliers 
outliers <- function(x,low=TRUE,up=TRUE) {
  qnt <- quantile(x, probs=c(.25,.75),na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  if (low == TRUE) {
    low <- which(x < (qnt[1] - H))
  } else {
    low <- which(x < (qnt[1]))
  }
  if (up == TRUE) {
    up <- which(x > (qnt[2] + H))
  } else {
    up <- which(x > (qnt[2]))
  }
  c(low,up)
}

#Remove outliers
num_var <- num_var[-outliers(num_var$GrossSquareFeet),]
num_var <- num_var[-outliers(num_var$LandSquareFeet),]
num_var <- num_var[-outliers(num_var$YearBuilt),]
num_var <- num_var[-outliers(num_var$TotalUnits),]

#Total Units and Empty Land
filled_land <- num_var %>% 
  filter(TotalUnits != 0, GrossSquareFeet != 0, SalePrice != 0)

units <- filled_land %>% 
  ggplot(aes(x = TotalUnits, y = SalePrice)) + 
  geom_point(alpha=0.6) + theme_bw() + scale_y_log10()

sqft <- filled_land %>% 
  ggplot(aes(x = GrossSquareFeet, y = SalePrice)) + 
  geom_point(alpha=0.6) + theme_bw() 

land <- filled_land %>% 
  ggplot(aes(x = LandSquareFeet, y = SalePrice)) + 
  geom_point(alpha=0.6) + theme_bw() 

year <- filled_land %>% 
  ggplot(aes(x = YearBuilt, y = SalePrice)) + 
  geom_point(alpha=0.6) + theme_bw() 

plot_grid(units,sqft,land,year, nrow=2, ncol=2)

num_var %>%
  select(SalePrice, TotalUnits, YearBuilt, LandSquareFeet, 
         GrossSquareFeet) %>%
  pairs(col='navy')

#Gross Square Feet versus Sale Price
fit_sqft <- lm(filled_land$SalePrice ~ filled_land$GrossSquareFeet,
               data = filled_land)
summary(fit_sqft)

fit_sqft_land <- lm(lm(filled_land$SalePrice ~ 
                       filled_land$GrossSquareFeet + 
                       filled_land$LandSquareFeet, 
                       data = filled_land))
summary(fit_sqft_land)
anova(fit_sqft_land)
confint(fit_sqft_land)

#Adding qualitative variables
empty_land <- no_outlier_sales
empty_land$Empty <- NA
empty_land$Empty[which(empty_land$TotalUnits == 0)] <- "Yes"
empty_land$Empty[which(empty_land$TotalUnits != 0)] <- "No"
head(empty_land$Empty)
empty_land$Empty <- as.factor(empty_land$Empty)
str(empty_land$Empty)
levels(empty_land$TaxClassAtPresent)

resident <- c("1","1A","1C","2","2A","2B","2C")
nonresident <- c("3","4")
empty_land$Residence <- NA
empty_land$Residence[which(empty_land$TaxClassAtPresent %in% resident)] <- "Yes"
empty_land$Residence[which(empty_land$TaxClassAtPresent %in% nonresident)] <- "No"

head(empty_land)

qual_var <- empty_land %>% 
  select(Empty, Residence, SalePrice) 

head(qual_var)

fit_qual <- glm(qual_var$SalePrice~factor(qual_var$Empty)+
                  factor(qual_var$Residence))
fit_qual
summary(fit_qual)
