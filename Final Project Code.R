setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loading libraries
library(dplyr)
library(faraway)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(tidyverse)
library(caret)
library(ggcorrplot)
library(pls)
#library(MASS)

theme_set(theme_bw())
library("sf")

# Reading the data frame
df=readxl::read_xlsx("Real_estate_valuation_data_set.xlsx")


# Viewing rows
head(df,5)

# Changing column names
colnames(df) = c("index",
                 "transaction_date",
                 "house_age",
                 "distance_to_nearest_metro_station",
                 "no_of_convenience_stores",
                 "latitude",
                 "longitude",
                 "house_price")

# dropping index
df = df[,-1]

# Sorting the months (The dates range from Aug 2012 to July 2013)
months = sort(unique(df$transaction_date))

# subsetting months vector
v1 = months[1:5] # year 2012
v2 = months[6:12] # year 2013

# helper function find out month number
month = function(x){
  
  if(x == months[1]){return(08)}
  else if(x == months[2]){return(09)}
  else if(x == months[3]){return(10)}
  else if(x == months[4]){return(11)}
  else if(x == months[5]){return(12)}
  else if(x == months[6]){return(01)}
  else if(x == months[7]){return(02)}
  else if(x == months[8]){return(03)}
  else if(x == months[9]){return(04)}
  else if(x == months[10]){return(05)}
  else if(x == months[11]){return(06)}
  else if(x == months[12]){return(07)}
}

# helper function to determine year
year = function(x){
  if(x %in% v1){return(2012)}
  else if(x %in% v2) {return(2013)}
}


df$month = sapply(df$transaction_date, month) # creating month column
df$year = sapply(df$transaction_date, year) # creating year column

# Dropping transaction_date column
df = df[,-1]
df = df %>% select(year,
                   month,
                   house_age, 
                   distance_to_nearest_metro_station,
                   no_of_convenience_stores,
                   latitude,
                   longitude,
                   house_price)


# Starting univariate analysis
# Distribution of house_age
ggplot(df, aes(x=house_age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="#0073C2FF", binwidth = 1)+
  geom_density(alpha=.2, fill="#FF6666") + 
  labs(title = "Distribution of House Age", x = "House Age", y = "Proportion") + 
  geom_vline(aes(xintercept=mean(house_age)),color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(house_age)),color="black", linetype="solid", size=1) +
  annotate(geom = "text",
           label = c("mean", "median"),
           x = c(mean(df$house_age), median(df$house_age)-1.5),
           y = c(0.06, 0.06),
           angle = 90, 
           vjust = 1)


# Distribution of distance_to_nearest_metro_station
ggplot(df, aes(x=distance_to_nearest_metro_station)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="#0073C2FF", binwidth = 500)+
  geom_density(alpha=.2, fill="#FF6666") + 
  labs(title = "Distribution of Distance to Metro Station", x = "Distance", y = "Proportion") + 
  geom_vline(aes(xintercept=mean(distance_to_nearest_metro_station)),color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(distance_to_nearest_metro_station)),color="black", linetype="solid", size=1) + 
  annotate(geom = "text",
           label = c("mean", "median"),
           x = c(mean(df$distance_to_nearest_metro_station), median(df$distance_to_nearest_metro_station)),
           y = c(0.00075, 0.00075),
           angle = 90, 
           vjust = 1)


# Frequency plot of Number of Houses
ggplot(df %>% group_by(no_of_convenience_stores) %>% summarise(Houses = n()), aes(x = no_of_convenience_stores, y = Houses)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Houses), vjust = -0.3) +
  labs(title = "Number of Houses vs Number of Convenience Stores")



# Distribution of price
ggplot(df, aes(x=house_price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="#0073C2FF", binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666") + 
  labs(title = "Distribution of House Price", x = "House Price", y = "Proportion") + 
  geom_vline(aes(xintercept=mean(house_price)),color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(house_price)),color="black", linetype="solid", size=1) +
  annotate(geom = "text",
           label = c("mean", "median"),
           x = c(mean(df$house_price), median(df$house_price)-3),
           y = c(0.04, 0.04),
           angle = 90, 
           vjust = 1)


### Bivariate analysis
# Add the regression line
ggplot(df, aes(x=house_age, y=house_price)) + 
  geom_point()+
  geom_smooth(method=lm) +
  labs(title = paste0("House Price vs House Age (Corr = ",as.character(round(cor(df$house_age, df$house_price),2)),")"), x = "House Age", y = "House Price")


ggplot(df, aes(x=distance_to_nearest_metro_station, y=house_price)) + 
  geom_point()+
  geom_smooth(method=lm) +
  labs(title = paste0("House Price vs Distance to Nearest Metro Station (Corr = ",
                      as.character(round(cor(df$distance_to_nearest_metro_station, 
                                             df$house_price),2)),")"), 
       x = "Distance to Nearest Metro Station", 
       y = "House Price")

# House Price vs. Number of Convenience Stores
ggplot(df %>% 
         group_by(no_of_convenience_stores) %>% 
         summarise(avg_price = mean(house_price)) %>% 
         mutate(avg_price = round(avg_price,2)), aes(x = no_of_convenience_stores, y = avg_price)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = avg_price), vjust = -0.3) +
  labs(title = "Average Price by different number of nearby convenience stores", x = "No of Stores", y = "Average House Price")



# Making a scatter plot of lon
ggplot(df, aes(x=longitude, y=latitude, colour=house_price)) +
  geom_point(alpha = 0.3, size = 3.5) + 
  scale_colour_gradient(low="#A9C8F3", high="#0C2389")+
  labs(title = "House Price vs Location")


# Checking variation of price against months

# Boxplot basic

box_df = df %>% 
  dplyr::select(year, month, house_price) %>% 
  arrange(year, month) %>% 
  mutate(year_month = factor(as.Date(paste0(year,"-",month,"-01")))) %>% 
  select(year_month, house_price)


df %>% select(year,month,house_price)
ggplot(box_df, aes(x=year_month, y=house_price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(title = "Variation of House Price by Year Month", x = "Year-Month", y = "House Price") + 
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))



# Correlation Matrix
ggcorrplot(round(cor(df[,colnames(df)[3:8]]),2), hc.order = TRUE, type = "full",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#E46726", "white", "#6D9EC1"))



# Taking a summary of the data. Before we do that let's convert the year and month to factors
df_new = df
df_new$year = factor(df_new$year)
df_new$month = factor(df_new$month)

# Summary of the data set
summary(df_new)

# Number of rows and columns
nrow(df_new)
ncol(df_new)

# Missing values in the dataset(no missing values in the data set)

for(item in colnames(df_new)){
  str = paste0("is.na(df_new$",item,")")
  print(paste0("Number of Missing values in the column ",item," is = ",sum(eval(parse(text = str)))))
}


# Identifying high leverage points
lmod = lm(house_price ~ ., data = df)
lev = influence(lmod)$hat
sort(lev[lev > 3*ncol(df)/nrow(df)])


# Identifying outliers
p = ncol(df)
n = nrow(df)
jack = rstudent(lmod)
sort(abs(jack), decreasing=TRUE)[1:5]

halfnorm(lev, 8, labs = row.names(df), ylab = "leverages")

# Checking the cook's distance to identify influential observations
max(cooks.distance(lmod))
halfnorm(cooks.distance(lmod), labs = row.names(df), ylab = "Cook's distance")
qt(.05/(2*n), n-p-1)

df = df[-271,]
df = df[-313,]

# 

# Modelling Section

# Making Variables factor
df1 = df %>% mutate(year = factor(year),
                    month = factor(month))

# Cleaning the data frame
df_clean = df
#df_clean = df1

set.seed(42)

# Test Train Split
df_trn_idx = sample(nrow(df_clean),size = 0.8*nrow(df_clean))
df_trn = df_clean[df_trn_idx,]
df_tst = df_clean[-df_trn_idx,-8]
y_tst = df_clean[-df_trn_idx,8]

# Building a simple linear model
lm1 = lm(house_price ~., data = df_trn)
summary(lm1)


# conducting prediction on the test set
calc_rmse = function(y,pred){
  sqrt(mean((y - pred)^2))
}

# Error on test data set simple linear regression
calc_rmse(as.vector(y_tst$house_price),predict(lm1, df_tst))

# Regression Diagnostics
plot(lm1,1)
plot(lm1,2)
plot(lm1,3)
plot(lm1,4)

# Principal Component Regression
pr_house = prcomp(df_trn[,1:7], center = TRUE, scale = TRUE)
summary(pr_house)
dim(pr_house$x)

predict(pr_house, df_tst)

plot(pr_house$sdev, type = "l", ylab = "SD of PC", xlab = "PC Number")

scree_df = data.frame(cbind(1:7,pr_house$sdev))
colnames(scree_df) = c("PC Number","SD of PC")
ggplot(data=scree_df, aes(x=`PC Number`, y=`SD of PC`, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title = "Scree Plot for PCR")


# Lets find out the number of components required using cross validation
pcrmod = pcr(house_price ~. , data = df_trn, ncomp = 7)
pcrmse = RMSEP(pcrmod, newdata = cbind(df_tst,y_tst))
plot(pcrmse, main = "")
cv_df = data.frame(cbind(0:7,pcrmse$val))
colnames(cv_df) = c("Number of Components","RMSE")
ggplot(data=cv_df, aes(x=`Number of Components`, y=`RMSE`, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title = "Test Error vs Number of Components")

# Finding out what number of Principal Components give the least error on the test set 
which.min(pcrmse$val)

# Training model with 5 principal components
pr_house = prcomp(df_trn[,1:7], center = TRUE, scale = TRUE)
summary(pr_house)
pcr_train_df = cbind(data.frame(pr_house$x[,1:5]),df_trn$house_price)
colnames(pcr_train_df) = c("PC1","PC2","PC3","PC4","PC5","house_price")
pcr_mod = lm(house_price ~ ., data = pcr_train_df)
summary(pcr_mod)

# Changing the test data set
pcr_test = data.frame(predict(pr_house, df_tst)[,1:5])
predict(pcr_mod, pcr_test)
calc_rmse(as.vector(y_tst$house_price),predict(pcr_mod, pcr_test))




# Lasso Regression
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
x = data.matrix(df_trn[,1:7])
y = data.matrix(df_trn[,8])
cv_model = cv.glmnet(x, y, alpha = 1)

cv_model$lambda.min

plot(cv_model)

#find optimal lambda value that minimizes test MSE
best_lambda = cv_model$lambda.min

#produce plot of test MSE by lambda value
plot(cv_model) 

# Fitting the best model
best_model = glmnet(x, y, alpha = 1, lambda = best_lambda)

coef(best_model)
