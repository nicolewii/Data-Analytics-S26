library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("/Users/nicolelee/Documents/Github/Data-Analytics-S26/Lab 2/NY-House-Dataset.csv")
dataset <- NY_House_Dataset

## clear out missing values/ general outliers
dataset <- dataset[!is.na(dataset$PRICE) & 
                     !is.na(dataset$PROPERTYSQFT) & 
                     !is.na(dataset$BEDS) & 
                     !is.na(dataset$BATH), ]

dataset <- dataset[dataset$BATH != 2.373860858 & dataset$PROPERTYSQFT!=2184.207862,]

## Model 1 - PRICE vs. SQFT ##

#clean the data set 
dataset1 <- dataset[dataset$PROPERTYSQFT <= 10000,]

# linear fit & print
lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset1)
summary(lmod1)

#Model 1: PLOT
ggplot(dataset1, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="green") + 
  labs(title = "Model 1: Price vs. PropertySQFT")

#Model 1: RESIDUAL PLOT
ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "green") +
  labs(title = "Model 1: Residual Plot", x = "Fitted", y = "Residual")

## Model 2 - PRICE vs. BEDS ##

#clean the data set of beds
dataset2 <- dataset[dataset$BEDS <= 10,]

# linear fit & print
lmod2 <- lm(log10(PRICE)~BEDS, data = dataset2)
summary(lmod2)

#Model 2: PLOT
ggplot(dataset2, aes(x = BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="royalblue") + 
  labs(title = "Model 2: Price vs. Beds")

#Model 2: RESIDUAL PLOT
ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "royalblue") +
  labs(title = "Model 2: Residual Plot", x = "Fitted", y = "Residual")

## Model 3 - PRICE vs. log10(PROPERTYSQFT),BED and BATH ##

# linear fit & print
lmod3 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)
summary(lmod3)

#Model 3: PLOT
ggplot(dataset, aes(x = log10(PROPERTYSQFT) + BEDS + BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="violet") + 
  labs(title = "Model 3: Price vs. PropertySQFT, Bed, and Bath")

#Model 3: RESIDUAL PLOT
ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "violet") +
  labs(title = "Model 3: Residual Plot", x = "Fitted", y = "Residual")

