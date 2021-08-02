install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')
library(readr)

library(corrplot)
library(mlbench)
library(Amelia)
library(reshape2)
library(caTools)
library(dplyr)
#crim - per capita crime rate by town
#zn - proportion of residential land zoned for lots over 25,000 sq.ft
#indus - proportion of non-retail business acres per town
#chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#nox - nitric oxides concentration (parts per 10 million)
#rm - average number of rooms per dwelling
#age - proportion of owner-occupied units built prior to 1940
#dis - weighted distances to five Boston employment centres
#rad - index of accessibility to radial highways
#tax - full-value property-tax rate per USD 10,000
#ptratio- pupil-teacher ratio by town
#b 1000(B - 0.63)^2, where B is the proportion of blacks by town
#lstat - percentage of lower status of the population
#medv - median value of owner-occupied homes in USD 1000’s
#loading the data and assgning it to sat variable
data(BostonHousing)
dat<- BostonHousing
str(dat)
dim(dat) #506 row and 14 column
head(dat,10) # 10 rows for top
tail(dat,10)  # 10 rows for buttom
summary(dat) #summary of the data


#Data cleaning  (check if there is any Na present in the dataset)
sum(is.na(dat))

#loading libraries
library(ggplot2)
library(caret)
# using the corrplot we come to know the correlation between the between two random variable in the dataset
cc<- cor(dat)
corrplot(cc,method = "circle")
head(dat)

#visualizing the distribution of the target variable
dat %>% 
  ggplot(aes(medv)) +
  stat_density() + 
  theme_bw()
ggplot(dat,aes(medv))+
  geom_density(color="darkblue", fill="black")

# looking at the effect of independent variable with respect to 
dat %>%
  select(c(crim, rm, age, rad, tax, lstat, medv,indus,nox,ptratio,zn)) %>%
  melt(id.vars = "medv") %>%
  ggplot(aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_classic()

#Splitting the given data set and building training and testing dataset using caTools library
set.seed(123)
split<- sample.split(dat,SplitRatio = 0.8)
train<- subset(dat,split==TRUE)
test<- subset(dat,split==FALSE)
#building our model
model <- lm(medv ~ crim + rm + tax + lstat , data = train)
summary(model)
#residuals= actual-predicted value
res <- residuals(model)
# Convert residuals to a DataFrame 
res <- as.data.frame(res)

ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)
#prediting testing dataset using model which was trained on training dataset
pred <- predict(model,test)
#visualizing the model by predciting on the testing dataset.
pp <-test %>% 
  ggplot(aes(medv,pred)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv')+
  theme_bw()
#evaluating model using  Root mean square error
error <- test$medv-pred
rmse <- sqrt(mean(error)^2)



