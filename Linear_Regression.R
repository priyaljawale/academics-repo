library(ggplot2)
library(corrplot)
library(dplyr)
library(scales)
library(GGally)
library(tidyr)
library(plotly)
library(xkcd)
library(memisc)
library(Metrics)
library(modelr)
library(RColorBrewer)

#read diamond dataset
diamonds <- read.csv("./MBA6636_SM21_Professor_Proposes_Data.csv")

#display some data
head(diamonds)

#data frame by rows and columns
str(diamonds)

#Check NA values
sum(is.na(diamonds))

#remove the $Price since it was treating price as categorical data
diamonds$Price <- gsub('[$]', '', diamonds$Price)
diamonds$Price <- as.numeric(gsub(',', '', diamonds$Price))
diamonds$Price

data(diamonds)
nrow(diamonds)
ncol(diamonds)
names(diamonds)

summary(diamonds)

#number of rows and columns
dim(diamonds)

plot(diamonds$price)

#create a random sample to look at the dataset
samp_index = sample(1:nrow(diamonds), 30, replace = FALSE)
samp_index

diamonds[samp_index,]

#categorical and numerical variables
#let's look at some categorical variables
str(diamonds$cut)
levels(diamonds$cut) #5 levels
plot(diamonds$cut)

#let's look at color categorical variable
str(diamonds$color)
levels(diamonds$color) #7 levels
plot(diamonds$color)

#clarity categorical value
str(diamonds$clarity)
levels(diamonds$clarity) # 8 levels
plot(diamonds$clarity)

#let's look continuous variable
#continous variable
str(diamonds$carat)
table(diamonds$carat)
plot(diamonds$carat)

#continuous variable
str(diamonds$price)
table(diamonds$price)
plot(diamonds$price)

#depth continuous variable
str(diamonds$depth)
plot(diamonds$depth)

#continuous variable
str(diamonds$table)
plot(diamonds$table)

#####Visualations#####
#plot simple histogram of diamond prices
ggplot(data=diamonds) + 
  geom_histogram(binwidth = 500, aes(x=price)) + 
  ggtitle("Diamond Price Distribution") + 
  xlab ("Diamond Price") + 
  ylab("Frequency") + 
  theme_classic()

#get mean and median
mean(diamonds$price) #3932.8
median(diamonds$price) #2401

#observe the graph if it gets higher than the expected frequency
#plotting graph by limitting the value less than $2500
ggplot(data=diamonds) + 
  geom_histogram(binwidth=500, aes(price)) + 
  ggtitle("Diamond Price Distribution") + 
  xlab("Diamond Price - Binwidth 500") + 
  ylab("Frequency") + 
  theme_minimal() + 
  xlim(0,2500)

#plotting graph by lowering the binwidth to 100
ggplot(data=diamonds) + 
  geom_histogram(binwidth=100, aes(x=price)) + 
  ggtitle("Diamond Price Distribution") + 
  xlab("Diamond Price - Binwidth 100") + 
  ylab("Frequency") + 
  theme_minimal() + 
  xlim(0,2500)
#frequency dropped

#plotting the graph by lowering the bindwidth to 50
ggplot(data=diamonds) + 
  geom_histogram(binwidth=50, aes(x=price)) + 
  ggtitle("Diamond Price Distribution") + 
  xlab("Diamond Price - Binwidth 50") + 
  ylab("Frequency") + 
  theme_minimal() + 
  xlim(0,2500)
#frequency dropped again!

#relationships between variables
#plotting histogram for Diamond Price Distribution by Cut
ggplot(data=diamonds) + 
  geom_histogram(binwidth=500, aes(x=price)) + 
  ggtitle("Diamond Price Distribution by Cut") + 
  xlab("Diamond Price") + 
  ylab("Frequency") + 
  theme_classic() + 
  facet_wrap(~cut)

#cut for the highest priced diamond
subset(diamonds, price == max(price)) # $18823 Premium cut

#cut for the lowest priced diamond
subset(diamonds, price == min(price)) # $326 for Ideal and Premium cut (carat is different)

#plotting price per carat of different cuts
ggplot(data=diamonds) + 
  geom_histogram(binwidth=50, aes(x=price/carat)) + 
  ggtitle("Diamond Price per Carat Distribution by Cut") + 
  xlab("Diamond Price per Carat") + 
  ylab("Frequency") + 
  theme_minimal() + 
  facet_wrap(~cut)

#plotting histogram for Diamond Price Distribution by Color 
ggplot(data=diamonds) + 
  geom_histogram(binwidth=500, aes(x=price)) + 
  ggtitle("Diamond Price Distribution by Color") + 
  xlab("Diamond Price") + 
  ylab("Frequency") + 
  theme_classic() + 
  facet_wrap(~color)

#plotting histogram for Diamond Price Distribution by Clarity
ggplot(data=diamonds) + 
  geom_histogram(binwidth=500, aes(x=price)) + 
  ggtitle("Diamond Price Distribution by Clarity") + 
  xlab("Diamond Price") + 
  ylab("Frequency") + 
  theme_classic() + 
  facet_wrap(~clarity)

#relationship between two continuous variables.
#Let's consider the price of a diamond and it's carat weight.
#Create a scatterplot of price (y) vs carat weight (x).
ggplot(data=diamonds, aes(x=carat, y=price)) +
  # get rid of top percentile
  scale_x_continuous(lim=c(0,quantile(diamonds$carat,0.99))) +
  scale_y_continuous(lim=c(0,quantile(diamonds$price,0.99))) +
  geom_point(fill=I('#dd3333'), color= I("black"), aes(alpha=1/10),shape=21) +
  stat_smooth(method='lm') +
  ggtitle("Diamond Price Distribution by Carat") + 
  xlab("Diamond Carat") + 
  ylab("Diamond Price") +
  theme_xkcd()
  
#linear Price and Carat Relationship
ggplot(data=diamonds, aes(x=carat, y=price)) +
  # get rid of top percentile
  scale_x_continuous(lim=c(0,quantile(diamonds$carat,0.99))) +
  scale_y_continuous(lim=c(0,quantile(diamonds$price,0.99))) +
  geom_point(color=I('#dd3333'),alpha=1/10) +
  stat_smooth(method='lm') +
  ggtitle("Linear Fit of Carat Weight to Price") + 
  xlab("Diamond Carat") + 
  ylab("Diamond Price") +
  theme_xkcd()

#relationship between continuous variable - Price and categorical variables - Cut, Clarity and Color.
#boxplot of Diamond Price according Cut
ggplot(diamonds, aes(factor(cut), price, fill=cut)) + 
  geom_boxplot() + 
  ggtitle("Diamond Price according Cut") + 
  xlab("Type of Cut") + 
  ylab("Diamond Price") + 
  coord_cartesian(ylim=c(0,7500))

#boxplot of Diamond Price according Clarity
ggplot(diamonds, aes(factor(clarity), price, fill=clarity)) +
  geom_boxplot() + 
  ggtitle("Diamond Price according Clarity") + 
  xlab("Clarity") + 
  ylab("Diamond Price") + 
  coord_cartesian(ylim=c(0,7500))

#boxplot of Diamond Price according Color
ggplot(diamonds, aes(factor(color), price, fill=color)) + 
  geom_boxplot() + 
  ggtitle("Diamond Price according Color") + 
  xlab("Type of Color") + 
  ylab("Diamond Price") +
  coord_cartesian(ylim=c(0,7500))

#boxplot of Diamond Price per Carat according Color
ggplot(diamonds, aes(factor(color), (price/carat), fill=color)) + 
  geom_boxplot() + 
  ggtitle("Diamond Price per Carat according Color") + 
  xlab("Color") + 
  ylab("Diamond Price per Carat")
  
#correlation plot a between Price and Carat Weight
ggcorr(diamonds[,1:10])

#scatterplot transformations
#price transformation
ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(color=I("#359bed"), alpha=1/50) + 
  scale_y_continuous(trans=log10_trans()) +
  ggtitle('Price - Log10 Tranformation') +
  xlab("Carat") + 
  ylab("Price Log 10") +
  theme_xkcd()

#carat transformation
cuberoot_trans = function() trans_new('cuberoot', transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(aes(carat, price), data = diamonds) + 
  geom_point() + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price log10 by Cube-Root of Carat') +
  theme_xkcd()

#Price vs. Carat and Clarity
ggplot(data = diamonds, aes(x = carat, y = price,color=clarity)) + 
  geom_point(alpha = 0.2, size = .75, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price log10 by Cube-Root of Carat and Clarity') +
  theme_xkcd()

#Price vs. Carat and Cut
ggplot(data = diamonds, aes(x = carat, y = price, color = cut)) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price log10 by Cube-Root of Carat and Cut') +
  theme_xkcd()

#Price vs. Carat and Color
ggplot(data = diamonds, aes(x = carat, y = price, color = color)) + 
  geom_point(alpha = 0.5, size = .75, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = "Color", reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price log10 by Cube-Root of Carat and Color') + 
  theme_xkcd()

#####Model####
##simple linear regression##
#considered carat variable

#Using the log function - transforms logarithmic data to linear data.
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))
model_diamonds <- lm(lprice ~ lcarat, data = diamonds2)

model_diamonds <- lm(lprice ~ lcarat, data = diamonds2)

summary(model_diamonds)
#Adjusted R-squared is 0.9334 (Carat weight is strong determinant of price)

#plotting linear model
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(model_diamonds, "lprice") %>%
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, color = "red", size = 1)

##multiple linear regression##
diamondsdata <- data.frame(diamonds, header = TRUE)

#create variables
carat <- diamonds$carat
cut <- diamonds$cut
color <- diamonds$color
clarity <- diamonds$clarity
price <- diamonds$price

#check for missing values
sum(is.na(carat))

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds) 
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut) 
m4 <- update(m3, ~ . + color) 
m5 <- update(m4, ~ . + clarity)
mtable(m1, m2, m3, m4, m5)

summary(m5)

n_obs = dim(diamonds)[1]
n_obs

prop_split = 0.80
train_index = sample(1:n_obs, round(n_obs * prop_split))
predictors <- diamonds[c(1:4)]

target <- diamonds$price

thisDiamond_example = data.frame(carat = 1.01, cut = "Fair",
                         color = "I", clarity="I1") #actualcost = $2,192
modelEstimate_example = predict(m5, newdata = thisDiamond_example,
                        interval="prediction", level = .95) 

exp(modelEstimate_example)
# 95% chance that the price will fall in this range
# fit = 1852.529 lwr = 1439.341 upr = 2384.329


 
