library(knitr)
library(tidyverse)
library(corrplot)
library(data.table)
library(dplyr)
library(GGally)
library(scales)
library(Hmisc)
library(caTools)
library(gridExtra)
library(rpart) 
library(rpart.plot)
library(epiDisplay)
library(ggcorrplot)

#read bank dataset        
banks = read.table('./bank-full.csv',sep=';',header = T)

summary(banks)

banks$y = ifelse(banks$y=='yes',1,0)

#data frame by rows and columns
str(banks)

#display some data
head(banks)

#Check NA values
sum(is.na(banks))

#number of rows and columns
dim(banks)

####Univariate Analysis####
###numeric variables###
##duration##
range(banks$duration)
#The attribute duration is a numeric attribute with values ranging from 0 to 4918.
summary(banks$duration)

ggplot(banks, aes(x=duration)) + 
  geom_histogram(color = "blue", fill = "blue") +
  ggtitle('Duration Histogram') + ylab('Count') + xlab('Duration') 

##y##
prop_y = prop.table(table(banks$y))
print(prop_y)
prop_y = as.data.frame(prop_y)

ggplot(prop_y,aes(x=Var1,y=Freq)) + geom_col(width = 0.5)
#The preceding plot shows that the dataset is imbalanced, with the number of negative classes being nearly eight times the number of positive classes.
#It's worth noting that the dataset's anticipated outcome (y) is heavily skewed in favour of 'no,' with over 88 percent.

#bar plot of variable(y)
ggplot(banks, aes(y)) +
  geom_bar(width = 0.1) +
  ggtitle("Bank Subscription of Term Deposit Distribution") + 
  xlab ("Term Deposit (y)") + 
  ylab("Frequency") 

table(banks$y)

##age##
#This is numeric feature in range between 18 and 95 years old.

summary(banks$age)

ggplot(banks) + 
  geom_histogram(aes(x=age),color="black", fill="white", binwidth = 5) +
  ggtitle('Age Distribution (red mean line)') +
  ylab('Count') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age), color = "red")) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position = "none")
#The majority of clients are between the ages of 33 (1st Quartile) and 48 (3rd Quartile), with a mean of 41 as depicted by the red vertical line on the histogram.
#This plot is right-skewed, but also similar to normal distribution.

ggplot(banks, aes(x = '', y = age)) +
  geom_boxplot() +
  ggtitle('Age Boxplot') +
  ylab('Age')
#We notice a more diffused distribution for clients who subscribed to a term deposit, but this is due to fewer persons saying yes.
#The statistics are largely the same in a boxplot of age, however there are outliers above the age of 65.

#remove the outliers
outliers_age <- boxplot(banks$age, plot = FALSE)$out
print(outliers_age)
banks <- banks[-which(banks$age %in% outliers_age),]

#without outliers
ggplot(banks, aes(x = '', y = age)) +
  geom_boxplot() +
  ggtitle('Age Boxplot without outliers') +
  ylab('Age')
#The majority of data older than 70 years are eliminated. 
#After this operation, the area between quantiles 0.25 and 0.75 does not alter significantly.

##balance##
summary(banks$balance)
#This attribute represents account balance, and it is numeric in nature, with values ranging from -8019 to 102127.
plot(banks$balance)
#The graph depicts the account balances of all clients. We can see that there aren't many clients with balances more than 4000.

#box plot of balance
ggplot(banks, aes(x = '', y = balance)) +
  geom_boxplot() +
  ggtitle('Balance Boxplot') +
  ylab('Balance')

#remove outliers
outliers_balance <- boxplot(banks$balance, plot = FALSE)$out
print(outliers_balance)
banks <- banks[-which(banks$balance %in% outliers_balance),]

#box-plot of balance without outliers
ggplot(banks, aes(x = '', y = balance)) +
  geom_boxplot() +
  ggtitle('Balance Boxplot without outliers') +
  ylab('Balance')

ggplot(banks, aes(x=balance)) + 
  geom_histogram(color = "blue", fill = "blue") +
  ggtitle('Balance Histogram') + ylab('Count') + xlab('Balance')
#We can see that the smallest range has the greatest amount of clients.

##campaign##
summary(banks$campaign)

ggplot(banks, aes(x=campaign)) + 
  geom_histogram(color = "blue", fill = "blue") +
  ggtitle('Campaign Histogram') + ylab('Count') + xlab('Campaign') 
#The bar plot is skewed to the right, indicating that the number of clients reached who had not signed up for the bank term deposit policy was greater than the number of clients contacted who had signed up for the bank term deposit policy.

ggplot(banks, aes(x = '', y = campaign)) +
  geom_boxplot() +
  ggtitle('Campaign Boxplot') +
  ylab('Campaign')

##pdays##
summary(banks$pdays)
#The p-days feature is numeric in nature, and the values assigned to it range from -1 to 871.
ggplot(banks, aes(x=pdays)) + 
  geom_histogram(color = "blue", fill = "blue") +
  ggtitle('Pdays Histogram') + ylab('Count') + xlab('Pdays') 

##previous##
summary(banks$previous)
#The attribute 'previous' is a numeric attribute with values ranging from 0 to 275.
ggplot(banks, aes(x=previous)) +
  geom_histogram(color = "blue", fill = "blue") +
  ggtitle('Previous Histogram') + 
  ylab('Count') + 
  xlab('Previous')

#categorical variables
##job##
#It is categorical in type and has categorical features that takes the following values: 
#’admin.’, ’blue-collar’, ’entrepreneur’, ’housemaid’, ’management’, ’re-tired’, ’self-employed’, ’services’, ’student’, ’technician’, ’unemployed’, ’unknown’.
str(banks$job)
job_table <- table(banks$job)
new_table_job <- as.data.table(job_table)
new_table_job

frequency_job <- as.data.table(prop.table(job_table)*100)
frequency_job
#The census count shows that blue-collar jobs employ a total of 9732 people, making up 21.52 percent of the workforce.
#With a total of 9458 employees and a percentage of 20.92, management is the second most popular position.

levels(banks$job) #12 levels, we have 12 different categories of job type.

#bar plot of distribution of types of jobs.
ggplot(banks, aes(x=job)) +
  geom_bar(color = "blue", fill = "blue", width = 0.1) +
  ggtitle('Jobs') + 
  ylab('Count') + 
  xlab('Job')

##marital_status##
#This attribute is marital, and it is categorical in nature, with values ranging from 'divorced', 'married' and 'single.'
str(banks$marital)
levels(banks$marital) 
#3 levels, we have 3 different categories of marital status
as.data.table(table(banks$marital))
table(banks$marital, banks$y)
#Married people have the highest number of term deposits subscribed and unsubscribed, followed by single people, while divorced people have the lowest number of term deposits subscribed and unsubscribed.
plot(banks$marital)

##education##
#The education attribute in the data set is categorical in nature and has the following values: 'primary', 'secondary', 'tertiary' and 'unknown'.
str(banks$education)
levels(banks$education)
# 4 levels, we have 4 different categories of education
education_table <- table(banks$education, banks$y)
education_table
tabpct(banks$education, banks$y, graph = F)
#looking at column percent
#With 20752 (52 %) subscribed and 2450 (46.3 %) unsubscribed to the bank term deposit, Secondary education has the biggest number of subscribers and unsubscribers.
#Tertiary education is next, with 11305 (28.3 %) subscribers and 1996 (37.7 %) non-subscribers, followed by primary and unknown, with the least number of subscribers and non-subscribers to the bank term deposit.

plot(banks$education)

##housing##
str(banks$housing)
levels(banks$housing)
plot(banks$housing)

##loan##
str(banks$loan)
levels(banks$loan)
plot(banks$loan)

##default##
str(banks$default)
levels(banks$default)
plot(banks$default)

##contact##
#We'll look at the attribute 'contact,' which is categorical in nature and has the values 'cellular', 'telephone', and 'unknown' as attribute values.
str(banks$contact)
levels(banks$contact)
# 3 levels
plot(banks$contact)

##poutcome##
str(banks$poutcome)
#Here the feature we analyse is the P-outcome, which is categorical in nature and has the values 'failure', 'other', 'success', and 'unknown'.
levels(banks$poutcome)
#4 levels, we have 4 different categories of poutcome
plot(banks$poutcome)

####Bivariate Analysis####
#age vs subscription
ggplot (banks, aes(x=age)) + 
  geom_histogram(color = "blue", fill = "blue", binwidth = 5) +
  facet_grid(cols=vars(y)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0,100,5))

#duration vs subscription
ggplot (banks, aes(x=duration)) + 
  geom_histogram(color = "blue", fill = "blue", binwidth = 5) +
  facet_grid(cols=vars(y)) + 
  ggtitle('Duration Distribution by Subscription') + ylab('Count') + xlab('Duration') +
  scale_x_continuous(breaks = seq(0,100,5))

#balance vs subcription
ggplot (banks, aes(x=balance)) + 
  geom_histogram(color = "blue", fill = "blue") +
  facet_grid(cols=vars(y)) + 
  ggtitle('Balance Histogram') + ylab('Count') + xlab('Balance')

#pdays vs subscription
ggplot (banks, aes(x=pdays, fill=as.factor(y))) + 
  geom_bar() +
  ggtitle('Bar-plot of pdays') + 
  ylab('Count') + 
  xlab('Pdays') +
  guides(fill=guide_legend(title="y"))

#previous vs subcription
ggplot (banks, aes(x=previous, fill=as.factor(y))) + 
  geom_bar() +
  ggtitle('Bar-plot of previous') + 
  ylab('Count') + 
  xlab('Previous') +
  guides(fill=guide_legend(title="y"))

#bar plot of job vs subcription
ggplot(data = banks, aes(x=job, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Job") +
  xlab("Jobs") +
  guides(fill=guide_legend(title="Subscription of Term Deposit (y)"))
#We can see that the job type that took out a term deposit is Management has the greatest term deposit subscriptions, followed by technicians, while blue-collar workers have the least.
#can remove the unknown values they do not hold any significance.

#bar plot of jobs according to education
ggplot(data = banks, aes(x=job, fill=as.factor(education))) +
  geom_bar() +
  ggtitle("Bar-plot of jobs according to education") +
  xlab(" Job") +
  guides(fill=guide_legend(title="Education"))

#education vs subscription
ggplot(data = banks, aes(x=education, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="y"))

#marital vs subscription
ggplot(data = banks, aes(x=marital, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Marital Status") +
  xlab("Marital Status") +
  guides(fill=guide_legend(title="y"))
#Because marriage people make up the largest group of analysed clients, the married category has the highest subscription to a term deposit, with a census count of over 3000, as well as the highest unsubscription to a term deposit.
#The single has the second-largest term deposit subscription, with a count of 2000, and the second-largest term deposit unsubscription.

#housing vs subscription
ggplot(data = banks, aes(x=housing, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on housing") +
  xlab("Housing") +
  guides(fill=guide_legend(title="y"))

#loan vs subcription
ggplot(data = banks, aes(x=loan, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Loan") +
  xlab("Loan") +
  guides(fill=guide_legend(title="y"))

#default vs subcription
ggplot(data = banks, aes(x=default, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Default") +
  xlab("Default") +
  guides(fill=guide_legend(title="y"))

#default, housing, loan, y
banks_select3 <- banks %>% dplyr::select(default, housing, loan, y)
pairs(banks_select3)

#contact vs subscription
ggplot(data = banks, aes(x=contact, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Contacts") +
  xlab("Contacts") +
  guides(fill=guide_legend(title="y"))

#month vs subscription
ggplot(data = banks, aes(x=month, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Month") +
  xlab("Months") +
  guides(fill=guide_legend(title="y"))

#poutcome vs subcription
ggplot(data = banks, aes(x=poutcome, fill=as.factor(y))) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on poutcomes") +
  xlab("Poutcomes") +
  guides(fill=guide_legend(title="y"))

#age distribution vs marital status that subscribed term deposit
ggplot(banks, aes(x = age, fill = marital)) +
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

#Subscription based on Number of Contact during Campaign
#It can be observed from barchart that there will be no subscription beyond 7 contact during the campaign. Future campaign could improve resource utilization by setting limits to contacts during a campaign. Future campaigns can focus on first 3 contacts as it will have higher subscription rate.

ggplot(data=banks, aes(x=campaign, fill=y))+
  geom_histogram()+
  ggtitle("Subscription based on Number of Contact during the Campaign")+
  xlab("Number of Contact during the Campaign")+
  xlim(c(min=1,max=30)) +
  guides(fill=guide_legend(title="y"))

#Scatterplot of Duration by Age
#Less clients after age of 60. Duration during call looks similar.
banks %>% 
  ggplot(aes(age, duration)) +
  geom_point() +
  facet_grid(cols = vars(y)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Scatterplot of Duration vs Age for Subscription of Term Deposit")

#Scatterplot of Duration by Campaign
#Duration on call similar for first 10 contacts during campaign. Successful subscription (y=1) occur within first 10 contacts. Much less after that.

banks %>% filter(campaign < 63) %>% 
  ggplot(aes(campaign, duration)) +
  geom_point() +
  facet_grid(cols = vars(y)) +
  ggtitle("Scatterplot of Duration vs Campaign for Subscription of Term Deposit")

banks_select1 <- banks %>% dplyr::select(duration, month, day, balance)
pairs(banks_select1)

banks_select2 <- banks %>% dplyr::select(balance, housing, loan, campaign)
pairs(banks_select2)

ggcorr(banks)
#duration, pdays, age, day

##Logistic Regression Model##
#split into training and testing
set.seed(123)
split = sample.split(banks$y,SplitRatio = 0.70)
training_set = subset(banks, split == TRUE)
test_set = subset(banks, split == FALSE)

#scale
training_set[c(1,6,10,12,13)] = scale(training_set[c(1,6,10,12,13)])
test_set[c(1,6,10,12,13)] = scale(test_set[c(1,6,10,12,13)])
  
binclass_eval = function (actual, predict) {
  cm = table(as.integer(actual), as.integer(predict), dnn=c('Actual','Predicted'))
  ac = (cm['1','1']+cm['0','0'])/(cm['0','1'] + cm['1','0'] + cm['1','1'] + cm['0','0'])
  pr = cm['1','1']/(cm['0','1'] + cm['1','1'])
  rc = cm['1','1']/(cm['1','0'] + cm['1','1'])
  fs = 2* pr*rc/(pr+rc)
  list(cm=cm, recall=rc, precision=pr, fscore=fs, accuracy=ac)
}

#creating the classifier
classifier.lm = glm(formula = y ~ duration+age+poutcome+housing+contact+
                      loan+marital+education+pdays+job+month+campaign
                      ,
                    family = binomial,
                    data = training_set)



summary(classifier.lm)

#choose the best threshold as 0.30
pred_lm = predict(classifier.lm, type='response', newdata=test_set[-17])
test.eval.LR = binclass_eval(test_set[, 17], pred_lm > 0.30)

#making the Confusion Matrix
test.eval.LR$cm

#calculate accuracy, precision etc.
acc_LR=test.eval.LR$accuracy
prc_LR=test.eval.LR$precision
recall_LR=test.eval.LR$recall
fscore_LR=test.eval.LR$fscore

cat("Accuracy:  ",   acc_LR,
    "\nPrecision: ", prc_LR,
    "\nRecall:    ", recall_LR,
    "\nFScore:    ", fscore_LR)

#Accuracy is 0.894279 considering categories duration, age and poutcome. 
#Accuracy is 0.8963433 considering categories duration, age, poutcome and housing
#Accuracy is 0.8977544 considering categories duration, age, poutcome, housing and contact
#Accuracy is 0.899366 considering categories duration, age, poutcome, housing, contact and loan
#Accuracy is 0.8995871 considering categories duration, age, poutcome, housing, contact, loan and marital
#Accuracy is 0.9003244 considering categories duration, age, poutcome, housing, contact, loan, marital and education
#Accuracy is 0.9013565 considering categories duration, age, poutcome, housing, contact, loan, marital, education, job, pdays, month and campaign

#The categorical features affected the accuracy of the model, whereas the numeric features like previous, balance and day weren't showing the desired results and didn't increase the accuracy.
#The following 12 features duration, age, poutcome, housing, contact, loan, marital, education, job, pdays, month and campaign give the desired accuracy for the logistic model.


classifier.lm = glm(formula = y ~ .,
                    family = binomial,
                    data = training_set)

#Accuracy is 0.9013565