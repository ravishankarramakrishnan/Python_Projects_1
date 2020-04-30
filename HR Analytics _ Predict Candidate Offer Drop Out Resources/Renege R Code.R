# _______________________________________________________________________________________________________________________________________
# Step 01 - Importing Data
# _______________________________________________________________________________________________________________________________________

# Importing CSV data set
data <- read.csv(file.choose())


# _______________________________________________________________________________________________________________________________________
# Step 02 - Validate for correctness
# _______________________________________________________________________________________________________________________________________

# Count of Rows and Columns
dim(data)

# View top 10 rows of the data set
head(data, 10)

# View last 10 rows of the data set
tail(data, 10)

# Understand type of variables
str(data)

# Count of Yes/No of the Dependent variable
table(data$offer_dropped)


# Offer dropped percentage
(1024/1999)*100

# Insight: 51% of candidates have dropped the offers


#____________________________________________________________________________________________________________________________________________________________
# Step 03 - Data Preparation
#___________________________________________________________________________________________________________________________________________________________
# Univariate Analysis
# ____________________________________________________________________________________________________________________________________________________________________________________

# Obtain descriptive statistics of the data 
summary(data)


# We will start with categorical variables
# Visual approach for marital_status
plot(data$marital_status, xlab = "marital_status")
# Insight: The bar chart shows married candidates have highest frequency

# Metric approach for martial_status
tab_marital<- table(data$marital_status)
tab_marital
prop.table(tab_marital)*100
# Insight: Married candidates have 69% of the total

# Visual approach for education_level
plot(data$education_level, xlab = "education_level")
# Insight: Associate degree have highest frequency followed by associate certification

# Metric approach for education_level
tab_education<- table(data$education_level)
tab_education
prop.table(tab_education)*100
# Insight: Associate Degree and associate certification are 60% of total

# Visual approach for gender
plot(data$gender, xlab = "gender")
# Insight: Male candidates are more than female candidates

# Metric approach
tab_gender<- table(data$gender)
tab_gender
prop.table(tab_gender)*100
# Insight: Male candidates are 3/4th of total

# Visual approach for distance_from_home
plot(data$ distance_from_home, xlab = "distance_from_home")
# Insight: Most of the candidate lives in 15 km or more from work location

# Metric approach for distance_from_home
tab_dist<- table(data$distance_from_home)
tab_dist
prop.table(tab_dist)*100
# Insight: 40% of candidates live 20 kms away from work location 

# Visual approach for sourcing_channel
plot(data$sourcing_channel, xlab = "sourcing_channel")
# Insight: Job portals is the most common source followed by consultants


# Metric approach for sourcing_channel
tab_channel<- table(data$sourcing_channel)
tab_channel
prop.table(tab_channel)*100
# Insight: Internal referrals are the lowest sourcing channel 

# Visual approach for career_growth
plot(data$career_growth, xlab = "career_growth")
# Insight: Most of the candidates were offered lateral career growth


# Metric approach for career_growth
tab_growth<- table(data$career_growth)
tab_growth
prop.table(tab_growth)*100
# Insight: 64% of candidates were offered lateral career growth

# Visual approach for flexi_work
plot(data$flexi_work, xlab = "flexi_work")
# Insight: Most candidates were offerd flexible work timing


# Metric approach for flexi_work
tab_flexi<- table(data$flexi_work)
tab_flexi
prop.table(tab_flexi)*100
# Insight: Approximately 60% of candidates were offerd flexible work timing

#Visual approach for timely_communication
plot(data$timely_communication, xlab =  "timely_communication")
# Insight: Most of the candidates were not timely commmunicated

# Metric approach
tab_communication<- table(data$timely_communication)
tab_communication
prop.table(tab_communication)*100
# Insight: 60% of candidates were not timely commmunicated

# We will start with Numerical  variables

# Visual approach for age
hist(data$age)
# Insight: Age of candidates ranges from 25 to 46 and seems to be normally distributed

# Metric approach for age
tab_age<- table(data$age)
tab_age
prop.table(tab_age)*100
# Insight: Almost same number of candidates lie in the age group of 25 to 45.
#          Although least number of candidates are of age 46.

# Visual approach for percent_hike
hist(data$percent_hike)
# Insight: salary hike ranges from 10% to 40% and distibution is right skewed

# Metric approach for percent_hike
tab_percent_hike <- table(data$ percent_hike)
tab_percent_hike 
prop.table(tab_percent_hike)*100
# Insight: Maximum salary hike ranges from 10% to 13%

# Visual approach for satisfaction_index
hist(data$satisfaction_index)
# Insight: Most of the candidates are not satisfied

# Metric approach for satisfaction_index
tab_satisfaction_index <- table(data$satisfaction_index)
tab_satisfaction_index
prop.table(tab_satisfaction_index)*100
# Insight: Most candidates have rated satisfaction index between 15 to 30 

# Visual approach for total_rounds
hist(data$total_rounds)
# Insight: Total rounds of interviews vary from 3 to 10

# Metric approach for total_rounds
tab_total_rounds <- table(data$ total_rounds)
tab_total_rounds
prop.table(tab_total_rounds)*100
# Insight: 23% of candidates gave 3 rounds of interviews

 
# _______________________________________________________________________________________________________________________________________
# Step 04 - Missing Value
# _______________________________________________________________________________________________________________________________________   

# Check if there are missing values
sum(is.na(data))
# Insight: There are no missing values in the dataset

# _______________________________________________________________________________________________________________________________________
# Step 05 - Feature Engineering - New Variable Creation
# _______________________________________________________________________________________________________________________________________

# create job hopping index: jhi
data$jhi <- data$total_experience/data$no_companies_worked

# Metric approach
summary(data$jhi)
# Insight: Higher job hoping index at 7.5 and lower at 0.5

# Visual approach
hist(data$jhi)
# Insight: Maximum job hoping index ranges from 0.5 to 3

# create days to offer: days_offered
data$days_offered <- as.Date(data$date_offered,"%m/%d/%Y") - as.Date(data$date_1st_contact,"%m/%d/%Y")

# Convert days offered to numeric
data$days_offered<-as.numeric(data$days_offered)
# Insight: days_offered is converted in numeric from difftime format
class(data$days_offered)

# Visual approach for days_offered
hist(data$days_offered)
# Insight: Candidates were offered job as less as 1 day to maximum 180 days

# Metric approach 
tab_days_offered_num <- table(data$days_offered)
tab_days_offered_num
prop.table(tab_days_offered_num)
# Insight: Approx 35% of candidates were offered job after 100 days


# Create data2 and remove "date_1st_contact", "date_offered", "no_companies_worked", "total_experience"  
data2<-data[c(-9,-10,-12,-15)]
names(data2)

# ___________________________________________________________________________________________________________________________________

# Step 06 - Bi-Variate Analysis - Independent vs. Dependent and Hypothesis testing
# ___________________________________________________________________________________________________________________________________

# Let's do some basic investigation of the remaining variables and get a feel for their relationship 
# to offer dropped
# We will start with categorical variables
# We may want to answer some questions that will help us build some understanding about the data2 at hand


## Are there any clear differences in marital_status of candidates who dropped offer vs those who accepted ?
# Visual approach
plot(data2$marital_status, data2$offer_dropped, xlab ="marital_status" , ylab = "Offer Dropped")

# Metric approach for marital_status vs offer_dropped
tab_marital_status<- table(data2$offer_dropped, data2$marital_status)
tab_marital_status

# Column level proportion
prop.table(tab_marital_status, 2)*100
# Insight: Equal proportion of candidates drop or accept offers across various marital status



## Are there any clear differences for education_level of candidates who dropped offer vs those who accepted?

# Visual approach for eductaion_level vs offer_dropped
plot(data2$education_level, data2$offer_dropped, xlab = "education_level", ylab = "Offer Dropped")

# Metric approach for eductaion_level vs offer_dropped
tab_education_level<- table(data2$offer_dropped, data2$education_level)
tab_education_level

# Column level proportion
prop.table(tab_education_level, 2)
# Insight 1 : Equal proportion of candidates drop or accept offers across various education_level 


## Are there any clear differences for gender of candidates who dropped offer vs those who accepted?

# Visual approach
plot(data2$gender, data2$offer_dropped, xlab ="gender" , ylab = "Offer Dropped")


# Metric approach
tab_gender_lev<- table(data2$offer_dropped, data2$gender)
tab_gender_lev

# Column level proportion
prop.table(tab_gender_lev, 2)
# Insight: There are three times more males in the data2set than females, however not too much of a difference 
# in the proportion of employees dropping offer in each category of gender

## Are there any clear differences for distance_from_home of candidates who dropped offer vs those who accepted?

# Visual approach
plot(data2$distance_from_home, data2$offer_dropped,  xlab = "distance_from_home", ylab = "Offer Dropped")

# Metric approach
tab_dfh<- table(data2$offer_dropped, data2$distance_from_home)
tab_dfh

# Column level proportion
prop.table(tab_dfh, 2)
# Insight: 58% of people are not accepting offers because of distance from home is more than 20 kms 


## Are there any clear differences for sourcing channel with dropped offer vs those who accepted?
# Visual approach
plot(data2$sourcing_channel, data2$offer_dropped, xlab = "sourcing channel", ylab = "Offer Dropped")

# Metric approach
tab_sc<- table(data2$offer_dropped, data2$sourcing_channel)
tab_sc

# Column level proportion
prop.table(tab_sc, 2)
# Insight: Out of all internal references 70% dropped the offer, hence it may not be good source


## Are there any clear differences for career growth of the candidates who dropped offer vs those who accepted?
# Visual approach
plot(data2$career_growth, data2$offer_dropped,  ylab = "Offer Dropped", xlab = "career_growth")

# Metric approach
tab_career_growth<- table(data2$offer_dropped, data2$career_growth)
tab_career_growth

# Column level proportion
prop.table(tab_career_growth, 2)
# Insight: More than 60% candidates who dropped offer were in lateral growth category

## Are there any clear differences for flexi_work of the candidates who dropped offer vs those who accepted?
# Visual approach
plot(data2$flexi_work, data2$offer_dropped, xlab = "flexi_work", ylab = "Offer Dropped")

# Metric approach
tab_flexi_work<- table(data2$offer_dropped, data2$flexi_work)
tab_flexi_work

# Column level proportion
prop.table(tab_flexi_work, 2)
# Insight: There is 6:4 ratio of offer drop between the candidates who have flexible work timing or do not have flexible work timing


## Are there any clear differences for timely_communication of the candidates who dropped offer vs those who accepted?

#Visual approach
plot(data2$timely_communication, data2$offer_dropped, xlab = "timely_communication", ylab = "Offer Dropped")
# Insight: Timely communication may not have a significant impact on offers getting dropped as evident from the output

# Metric approach
tab_timely_communication<- table(data2$offer_dropped, data2$timely_communication)
tab_timely_communication

# Column level proportion
prop.table(tab_timely_communication, 2)
# Insight: 60% candidates dropped offer were not timely communicated



# let's do Bi-Variate Analysis - dependent vs. independent Numeric variables

# We may want to answer some questions that will help us build some understanding about the data2 at hand 


## Are there any clear differences for age within candidates who dropped offer vs those who accepted?

# Visual approach
boxplot(data2$age ~ data2$offer_dropped, xlab = "Offer Dropped", ylab = "Age",range=95)


# metric approach
# calculate mean
means_age <- by(data2$age, data2$offer_dropped, mean)                        
means_age
# calculate median
med_age <- by(data2$age, data2$offer_dropped, median) 
med_age
# Insight: Age does not seem to be a reason of offer drop

## Are there any clear differences for percent hike within candidates who dropped offer vs those who accepted?

# Visual approach
boxplot(data2$percent_hike ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "percent_hike",range=95)

# Metric approach
# calculate mean
mean_percent_hike <- by(data2$percent_hike, data2$offer_dropped, mean)                        
mean_percent_hike
# calculate median
med_percent_hike <- by(data2$percent_hike, data2$offer_dropped, median)                        
med_percent_hike
# calculate standard deviation
std_percent_hike <- by(data2$percent_hike, data2$offer_dropped, sd)                        
std_percent_hike
# Insight: The median percent hike for candidates who dropped the offer is less than who accepted


## Are there any clear differences for satisfaction_index within candidates who dropped offer vs those who accepted?

# Visual approach
boxplot(data2$satisfaction_index ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "satisfaction_index",range=95)

# Metric approach
mean_satisfaction_index <- by(data2$satisfaction_index, data2$offer_dropped, mean)                        
mean_satisfaction_index
med_satisfaction_index <- by(data2$satisfaction_index, data2$offer_dropped, median)                        
med_satisfaction_index
std_satisfaction_index <- by(data2$satisfaction_index, data2$offer_dropped, sd)                        
std_satisfaction_index
# Insight: The graph suggests a review of the overall interview process & candidates who dropped offers have not rated more than 30


## Are there any clear differences for total_rounds within candidates who dropped offer vs those who accepted?

# Visual approach
boxplot(data2$total_rounds ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "total_rounds",range=95)

# Metric approach
mean_total_rounds <- by(data2$total_rounds, data2$offer_dropped, mean)                        
mean_total_rounds
med_total_rounds <- by(data2$total_rounds, data2$offer_dropped, median)                        
med_total_rounds

# Insight: High number of interview rounds are leading to increased offer drop


## Are there any clear differences for jhi within candidates who dropped offer vs those who accepted?

# Visual approach
boxplot(data2$jhi ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "jhi",range=95)

# Metric approach
mean_jhi <- by(data2$jhi, data2$offer_dropped, mean)                        
mean_jhi
med_jhi <- by(data2$jhi, data2$offer_dropped, median)                        
med_jhi
std_jhi <- by(data2$jhi, data2$offer_dropped, sd)                        
std_jhi
# Insight: jhi indicates stability of a candidate; higher the jhi, more stable the candidate 

## Are there any clear differences for days_offered within candidates who dropped offer vs those who accepted?

# Visual approach
boxplot(data2$days_offered ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "days_offered",range=95)

# Metric approach
mean_days_offered <- by(data2$days_offered, data2$offer_dropped, mean)                        
mean_days_offered
med_days_offered <- by(data2$days_offered, data2$offer_dropped, median)                        
med_days_offered
# Insight: This calls for interview process optimization as delayed process may lead candidates to apply for jobs elsewhere due to lack of clarity



#_______________________________________________________________________________________________________________________________________
# Step 07 - Hypothesis Testing - Categorical Variables
#_______________________________________________________________________________________________________________________________________

# The chi-square test of independence is used to determine if there is a significant relationship between two 
# nominal (categorical) variables.
# The null hypothesis for this test is that there is no relationship between the variables and the alternate hypothesis that 
# there is a relationship

# Test 1
# Test whether there is a significant relationship between marital status dropping offers

# Hypothesis Statement
# H0: The two variables (marital_status and offer_dropped) are independent

# H1: The two variables relate to each other


# Particularly in this test, we have to check the p-values. we reject the null hypothesis if the p-value that
# comes out in the result is less than a predetermined significance level, which is usually 0.05

tab_marital_status<- table(data2$marital_status, data2$offer_dropped)
tab_marital_status
chisq.test(tab_marital_status)

# Insight: marital_status variable is not significant


# Test 2
# Hypothesis statement  
# H0: education_level and offer_dropped are independent

# H1: The two variables relate to each other

tab_education_level<-table(data2$education_level, data2$offer_dropped)
tab_education_level
chisq.test(tab_education_level)

# Insight: education_level variable is not significant


# Test 3

# Hypothesis statement  
# H0: gender and offer_dropped are independent

# H1: The two variables relate to each other

tab_gender<-table(data2$gender, data2$offer_dropped)
tab_gender
chisq.test(tab_gender)
# Insight: gender variable is insignificant


# Test 4

# Hypothesis statement  
# H0: distance_from_home and offer_dropped are independent

# H1: The two variables relate to each other

tab_dfh<-table(data2$distance_from_home, data2$offer_dropped)
tab_dfh
chisq.test(tab_dfh)
# Insight: distance_from_home variable is significant


# Test 5

# Hypothesis statement 
# H0: sourcing_channel and offer_dropped are independent

# H1: The two variables relate to each other

tab_sourcing_channel<-table(data2$sourcing_channel, data2$offer_dropped)
chisq.test(tab_sourcing_channel)
tab_sourcing_channel
# Insight: sourcing_channel variable is significant


# Test 6

# Hypothesis statement 
# H0: career_growth and offer_dropped are independent

# H1: The two variables relate to each other

tab_career_growth<-table(data2$career_growth, data2$offer_dropped)
tab_career_growth
chisq.test(tab_career_growth)
# Insight: career_growth variable is significant


# Test 7

# Hypothesis statement 
# H0: flexi_work and offer_dropped are independent

# H1: The two variables relate to each other

tab_flexi_work<-table(data2$flexi_work, data2$offer_dropped)
tab_flexi_work
chisq.test(tab_flexi_work)

# Insight: tab_flexi_work variable is significant


# Test 8

# Hypothesis statement 
# H0: timely_communication and offer_dropped are independent

# H1: The two variables relate to each other

tab_timely_com<-table(data2$timely_communication, data2$timely_communication)
tab_timely_com
chisq.test(tab_timely_com)
# Insight: timely_communication variable is significant

# Drop the insignificant variables 
names(data2)

# Drop the insignificant variables and create new data set data3
data3<- data2[c(-2, -3, -4)]

# Verify the list of variables
names(data3)

#_______________________________________________________________________________________________________________________________________
# Step 08 - Dummy variables creation
#_______________________________________________________________________________________________________________________________________

# Dummy coding refers to the process of coding a categorical variable into dichotomous variables(1,0)
# Use : Why is it used?
# Regression analysis treats all independent (X) variables in the analysis as numerical. Numerical variables
# are interval or ratio scale variables whose values are directly comparable, e.g. '10 is twice as much as 5', or
# '3 minus 1 equals 2'. Often, however, you might want to include an attribute or nominal scale variable such
# as 'Product Brand' or 'Type of Defect' in your study. Say you have three types of defects, numbered '1', '2'
# and '3'. In this case, '3 minus 1' doesn't mean anything. you can't subtracting defect 1 from defect 3

# The numbers here are used to indicate or identify the levels of 'Defect Type' and do not have intrinsic meaning of
# their own. Dummy variables are created in this situation to 'trick' the regression algorithm into correctly
# analyzing attribute variables 

# The number of dummy variables necessary to represent a single attribute variable is equal to the
# number of levels (categories) in that variable minus one. 
# Not Doing so would give the regression redundant information, result in multicollinearity, 
# and break the model. This means we have to leave one category out, and we call this missing category 
# the reference category 

# Finding the refernce groups for variable with more than 2 levels
table(data3$distance_from_home) #  > 20 kms will be our reference
table(data3$sourcing_channel)  # Job Portals will be our reference

# for distance_from_home
data3$dfh_15<- ifelse(data3$distance_from_home == '<15 kms', 1, 0)
data3$dfh_15to20 <- ifelse(data3$distance_from_home == '15-20 kms', 1, 0)

# for sourcing_channel
data3$ch_com_web<- ifelse(data3$sourcing_channel == 'Company Website', 1, 0)
data3$ch_cons <- ifelse(data3$sourcing_channel == 'Consultants', 1, 0)
data3$ch_Int_ref <- ifelse(data3$sourcing_channel == 'Internal Referrals', 1, 0)
data3$ch_soc_med <- ifelse(data3$sourcing_channel == 'Social Media', 1, 0)

# for career_growth
data3$career_growth_lateral<-ifelse(data3$career_growth == 'Lateral', 1, 0)

# for flexi_work
data3$flexi_work_yes<-ifelse(data3$flexi_work == 'Yes', 1, 0)

# for timely_communication
data3$tc_yes<-ifelse(data3$timely_communication == 'Yes', 1, 0)

# drop the original categorical variables
names(data3)
data4<-data3[c(-3,-4, -7, -8, -9)]


#_______________________________________________________________________________________________________________________________________
# Step 09 -  Dimension Reduction
#_______________________________________________________________________________________________________________________________________
# Step 09 - A - Information value
#_______________________________________________________________________________________________________________________________________



# We will be using Information value (IV) to screen important numeric predictor variables
# Information value is one of the most useful technique to select important variables in a predictive model 
# It helps to rank variables on the basis of their importance
# by convention if the IV statistic is :
# Less than 0.02, then the predictor is not useful for modeling (separating the Goods from the Bads)
# 0.02 to 0.1, then the predictor has only a weak relationship 
# 0.1 to 0.3, then the predictor has a medium strength relationship 
# 0.3 to 0.5, then the predictor has a strong relationship 

# The dependent variable in our data set is a categorical variable; 
class(data4$offer_dropped)

# In order to calculate the IV of the we need to convert the dependent variable into numeric variable
# Create a numeric variable of offer_dropped: offer_drop_num
data4$offer_drop_num<-ifelse(data4$offer_dropped == "Yes", 1, 0)
class(data4$offer_drop_num)
str(data4)
# We will drop the original variable offer_drop
data4<-data4[c(-5)]

# Save all numeric variables in a new data set: data_num
data_num <- data4[,c("age", "percent_hike", "total_rounds", "satisfaction_index", "jhi", "days_offered", "dfh_15", "dfh_15to20",
                     "ch_com_web","ch_cons", "ch_Int_ref","ch_soc_med","career_growth_lateral","flexi_work_yes","tc_yes","offer_drop_num")]



# Calculate Information Value for all numeric variables
#install.packages("Information")
library(Information)
IV <- create_infotables(data = data_num, y = "offer_drop_num")

# Print Information Value for all numeric variables
IV

# Select all those numerical variables which has IV of 0.1 or more
# Insights: Variable ch_cons has a IV of 0.0203, dfh_15 has IV of 0.0054 and ch_soc_med has IV of 0.0002 which makes it a weak predictor

# Hence drop ch_cons, dfh_15, ch_soc_med variables
data5<-data4[c(-7,-10,-12)]
names(data5)



#_______________________________________________________________________________________________________________________________________
# Step 09 - B - Detecting and Dealing with Multicollinearity 
#_______________________________________________________________________________________________________________________________________

# The basic problem is multicollinearity results in unstable parameter estimates which makes it very 
# difficult to assess the effect of independent variables on dependent variables

# variance inflation factors (VIF) helps us to detect multicollinearity
# It quantifies how much the variance is inflated
# VIF of 4 or more suggests the presence of multicollinearity
# install.packages("car")
library(car)
vif(glm(offer_drop_num ~ ., family = binomial, data = data5))

# Insights: There are no variables which are multicollinear, no action required



#_______________________________________________________________________________________________________________________________________
# Step 10 - Data Partitioning - Training & Testing Data sets
#_______________________________________________________________________________________________________________________________________

# Shuffeling the data before partitioning
data5 <-  data5[sample(nrow(data5)),] 
# Insight: This was done to achieve a good distribution in both test and train data

# Splitting data into train data and test data

set.seed(567)
index <- sample(2, nrow(data5), replace = TRUE, prob = c(0.7, 0.3))
# Training data set
data5_train<- data5[index == 1,]
# Testing data set
data5_test<- data5[index == 2,]




#_______________________________________________________________________________________________________________________________________
# Step 11 - Model Building 
#_______________________________________________________________________________________________________________________________________


# Iteration 01 - Model with all variables
model_0 <- glm(offer_drop_num ~ . , family = binomial, data = data5_train)
summary(model_0)

# Interpretation and Insigts: Lets answer some questions

# What do coefficients represent?
# Co-efficient value estimates the magnitude of impact of an independent variable on the outcome of interest, i.e. the dependent variable

# What do the signs of coefficients represent?
# sign implies whether the presence of a particular independent variable will increase or decrease the chances of getting 
# the outcome of interest

# What does Intercept represent?
# It represents the magnitude of chances of getting a desired outcome in the absence of any independent variable 


# What does Pr(>|t|) imply ?
# Simply put it is p-value of a corresponding independent variable. p-value of less than 0.05 indicates that 
# the independent variable significantly impacts the dependent variable and sould be part of the model
# The strength of the significance is indicated by the asterisk


# Iteration 02 - Model after removing age
model_1 <- glm(offer_drop_num ~ . -age , family = binomial, data = data5_train)
summary(model_1)

# Iteration 03 - Model after removing job hopping index, age
model_2 <- glm(offer_drop_num ~ . -jhi -age, family = binomial, data = data5_train)
#model_2 <- glm(offer_drop_num ~ . -ch_com_web -age, family = binomial, data = data5_train)
summary(model_2)


# Iteration 04 - Model after removing ch_com_web, job hopping index, age
model_3 <- glm(offer_drop_num ~ . -ch_com_web -jhi -age, family = binomial, data = data5_train)
summary(model_3)

# Iteration 05 - Model after removing total_rounds, ch_com_web, job hopping index, ch_cons
model_4 <- glm(offer_drop_num ~ .  -total_rounds -ch_com_web -jhi -age, family=binomial, data=data5_train)
summary(model_4)

# Insight: After this we do not have any insignificant variable present.


#_______________________________________________________________________________________________________________________________________
# Step 12 - Predicting Training Data 
#_______________________________________________________________________________________________________________________________________
# 

# Predicting on training data
data5_train$pred<- predict(model_4, newdata=data5_train, type = "response")
data5_train$pred
class(data5_train)


#_______________________________________________________________________________________________________________________________________
# Step 13 - Evaluating the Model - Part 1 - Training Data 
#_______________________________________________________________________________________________________________________________________

# A logistic regression model has been built and the coefficients have been examined 
# However, some critical questions remain 
# Is the model any good? 
# How well does the model fit the data? 
# Which predictors are most important? 
# Are the predictions accurate?

# Rest of this lesson will cover techniques for answering these questions and provide R code to conduct that analysis

# Testing goodness of fit
# One of the most frequent questions I get about logistic regression is "How can I tell if my model fits the data?" Often
# the questioner is expressing a genuine interest in knowing whether a model is a good model or a not-so-good model
# But a more common motivation is to convince someone else--a boss, an editor, or a regulator--that the model is OK 

# One approach to evaluating model fit is to compute a goodness-of-fit statistic. One such statistic is pseudo R2
# The pseudo R2 can be interpreted as a measure of how well your model fits the data

# Pseudo R^2
#install.packages("pscl")
library(pscl)
# There are many different ways to calculate R2 for logistic regression; McFadden's R2 is the one most commonly used

pR2(model_4)
# Insight: value of 0.86 represents a good fit

# Hosmer-Lemeshow Test

# Another approch to determining the goodness of fit is through the Hosmer-Lemeshow statistics, 
# which is computed on data after the observations have been segmented into groups based on 
# having similar predicted probabilities. It examines whether the observed proportions of events
# are similar to the predicted probabilities of occurence in subgroups of the data set 

# Note: Higher the p-value better is the fit of the model

#install.packages("generalhoslem")
library(generalhoslem)
logitgof(obs = data5_train$offer_drop_num, exp = fitted(model_4))



# Insight: p value = 0.4091 > 0.05, model is a good fit


# ROC Curve

# The receiving operating characteristic is a measure of classifier performance 
# Using the proportion of positive data points that are correctly considered as positive 
# and the proportion of negative data points that are mistakenly considered as positive, 
# we generate a graphic that shows the trade off between the rate at which you can correctly 
# something with the rate of incorrectly predicting something 
# Ultimately, we're concerned about the area under the ROC curve, or AUROC 
# That metric ranges from 0.50 to 1.00, and values above 0.80 indicate that the model does a good job 
# in discriminating between the two categories which comprise our target variable

#install.packages("ROCR")
library(ROCR)
pred2 <- prediction(data5_train$pred,labels = data5_train$offer_drop_num)
class(pred2)

slotNames(pred2)

roc.perf = performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

abline(a = 0, b = 1)

auc <- performance(pred2, measure = "auc")
auc
auc <- auc@y.values[[1]]
auc


# Extract the maximum accuracy(classification rate) and the corresponding cut-off
acc.perf = performance(pred2, measure = "acc")

slotNames(acc.perf)

ind = which.max(slot(acc.perf, "y.values")[[1]] )

ind

acc = slot(acc.perf, "y.values")[[1]][ind]

cutoff = slot(acc.perf, "x.values")[[1]][ind]



print(c(accuracy = acc, cutoff = cutoff))


# Insight: Accuracy of the model is 95%
acc.perf$y.values
# Testing the accuracy with the obtained cut-off on training data
predcat47<- ifelse(data5_train$pred >0.5114, 1, 0)

accuracy<- table(predcat47, data5_train[,"offer_drop_num"])
diag(accuracy)
sum(diag(accuracy)) / sum(accuracy)
table(data5_train$offer_drop_num)

# Insight: Accuracy of 96 % represents good model

#_______________________________________________________________________________________________________________________________________
# Step 14 - Evaluating the Model - Part 2 - Testing Data 
#_______________________________________________________________________________________________________________________________________


# Now Lets do some prediction using the model on the test data set. We have created a new new variable by the name pred 
# which has the predicted probability values 

# Predicting on testing data
data5_test$pred = predict(model_4, newdata=data5_test, type = "response")

# Testing the accuracy with the obtained cut-off on testing data
predtest47<- ifelse(data5_test$pred >0.5114, 1, 0)
accuracy<- table(predtest47, data5_test[,"offer_drop_num"])
sum(diag(accuracy)) / sum(accuracy)
# Insight: an accuracy of 94% represents a good model that is ready to be used to predict the offer drop out on a 
# brand new data set

