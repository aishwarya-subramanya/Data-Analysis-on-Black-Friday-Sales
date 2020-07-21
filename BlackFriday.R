#loading the libraries
library(DataExplorer) # For initial exploratory data analysis
library(dplyr) # For data manipulation
install.packages("psych")
library(psych)
library("ggplot2")
library("scales")
library("corrplot")
library("psych")
library("vcd")
library("MASS")
library("dplyr")

BlackFriday <- read.csv("~/Downloads/BlackFriday.csv")
dim(BlackFriday) #To find the number of rows and columns in the dataset

#DESCRIPTIVE STATISTICS

str(BlackFriday) #STRUCTURE of the dataset
plot_str(BlackFriday) #To have an insight about the structure of the dataset

describe(BlackFriday)#Properties of the Dataset

sapply(BlackFriday, function(x) sum(is.na(x))) #to obtain the missing values
plot_missing(BlackFriday) #plot for the missing data


# average number of products sold of each type
#product 1
hist(BlackFriday$Product_Category_2,xlab = 'number of transactions',main = 'product category 1')
mean(BlackFriday$Product_Category_1)

#product 2
hist(BlackFriday$Product_Category_2,xlab = 'number of transactions',main = 'Product Category 2')
mean(BlackFriday$Product_Category_2)

#product 3
mean(BlackFriday$Product_Category_3)
hist(BlackFriday$Product_Category_3,xlab = 'number of transactions',main = 'product category 3')

#married and unmarried couple
# 0 - unmarried
# 1 - married
hist(BlackFriday$Marital_Status)


#Which category of products has the highest number of sales on Black Friday?
table(BlackFriday$Product_Category_1)
table(BlackFriday$Product_Category_2)
table(BlackFriday$Product_Category_3)

#Which age group has spent the maximum amount of money on Black Friday? 
prop.table(table(BlackFriday$Age))

-------------------------------------------------------------------------
  
#STATISTICAL INFERENCE
  
#structure 
str(BlackFriday)
summary(BlackFriday)

summary(BlackFriday$Occupation)

table(BlackFriday$Age)

#head of the data
head(BlackFriday)

#tail of the data
tail(BlackFriday)

#Total Missing Values
sum(is.na(BlackFriday))

#Category wise missing values
apply(BlackFriday,2,function(x)sum(is.na(x)))

#Imputing 0 For NA in Product_Category_2

BlackFriday$Product_Category_2 <- as.numeric(BlackFriday$Product_Category_2)
BlackFriday[is.na(BlackFriday$Product_Category_2), "Product_Category_2"] <- 0

#Imputing 0 For NA in Product_Category_3
BlackFriday$Product_Category_3 <- as.numeric(BlackFriday$Product_Category_3)
BlackFriday[is.na(BlackFriday$Product_Category_3), "Product_Category_3"] <- 0
BlackFriday$User_ID <- NULL
BlackFriday$Product_ID <- NULL

#Distribution
ggplot(BlackFriday, aes(x = Purchase, fill = Occupation)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Occupation) +
  labs(title= "Purchases Histogram by Occupation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(BlackFriday$Age)

ggplot(BlackFriday, aes(x = Purchase, fill = Age)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Age) +
  labs(title= "Purchases Histogram by Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(corrplot)
bf_correlate <- data.frame(BlackFriday$Occupation, BlackFriday$Product_Category_1, BlackFriday$Product_Category_2, BlackFriday$Product_Category_3)
corrplot(cor(bf_correlate), method="number")

#Splitting of Data
set.seed(7)
split = sample(1:nrow(BlackFriday), 0.7 * nrow(BlackFriday))
train = BlackFriday[split,]
test = BlackFriday[-split,]
print("nrow(train)")
nrow(train)
print("nrow(test)")
nrow(test)
print("nrow(BlackFriday)")
nrow(BlackFriday)

#Transformation
trainInt <- train %>% select(Gender, Age, Occupation, Marital_Status, City_Category, Stay_In_Current_City_Years, Purchase, Product_Category_1)
trainInt$Gender <- as.factor(trainInt$Gender)
trainInt$Age <- as.numeric(trainInt$Age)
trainInt$Occupation <- as.factor(train$Occupation)
trainInt$Marital_Status <- as.factor(trainInt$Marital_Status)
trainInt$City_Category <- as.factor(trainInt$City_Category)
trainInt$Stay_In_Current_City_Years <- as.factor(trainInt$Stay_In_Current_City_Years)
trainInt$Product_Category_1 <- as.factor(trainInt$Product_Category_1)
str(trainInt)
testInt <- test %>% 
  select(Gender, Age, Occupation, Marital_Status, City_Category, Stay_In_Current_City_Years, Purchase, Product_Category_1)
testInt$Gender <- as.factor(testInt$Gender)
testInt$Age <- as.numeric(testInt$Age)
testInt$Occupation <- as.factor(testInt$Occupation)
testInt$Marital_Status <- as.factor(testInt$Marital_Status)
testInt$City_Category <- as.factor(testInt$City_Category)
testInt$Stay_In_Current_City_Years <- as.factor(testInt$Stay_In_Current_City_Years)
testInt$Product_Category_1 <- as.factor(testInt$Product_Category_1)
str(testInt)

#LINEAR REGRESSION MODEL

model <- (Purchase ~ Gender + Age + Occupation + Marital_Status + City_Category + Stay_In_Current_City_Years +Product_Category_1)
fit <- lm(model, data = trainInt, method = 'anova')

equation <- noquote(paste('Purchase =',
                          round(fit$coefficients[1],0), '+',
                          round(fit$coefficients[2],0), '* Gender', '+',
                          round(fit$coefficients[3],0), '* Age', '+',
                          round(fit$coefficients[4],0), '* Occupation', '+',
                          round(fit$coefficients[5],0), '* Marital_Status', '+',
                          round(fit$coefficients[6],0), '* City_Category', '+',
                          round(fit$coefficients[7],0), '* Stay_In_Current_City_Years', '+',
                          round(fit$coefficients[7],0), '* Product_Category_1'))
equation
summary(fit)
anova(fit)

predTrain <- predict(fit)
sseTrain <- sum((predTrain - trainInt$Purchase) ^ 2)
sstTrain <- sum((mean(train$Purchase) - trainInt$Purchase) ^ 2)
print ("Model R2 (Train Data)")
modelR2Train <- 1 - sseTrain/sstTrain
modelR2Train
print ("Model RMSE (Train Data)")
rmseTrain <- sqrt(mean((predTrain - trainInt$Purchase) ^ 2))
rmseTrain

predTest <- predict(fit, newdata = testInt)
sseTest <- sum((predTest - testInt$Purchase) ^ 2)
sstTest <- sum((mean(test$Purchase) - testInt$Purchase) ^ 2)
print ("Model R2 (Test Data)")
modelR2Test <- 1 - sseTest/sstTest
modelR2Test
print ("Model RMSE (Test Data)")
rmseTest <- sqrt(mean((predTest - testInt$Purchase) ^ 2))
rmseTest

#Accuracy check
pred1 <- predict(fit, testInt)
res<- cbind(pred1, testInt$Purchase)
colnames(res) <- c('pred', 'real')
head(res)


plot(fit, which=1, col=c("blue")) # Residuals vs Fitted Plot
plot(fit, which=2, col=c("red"))  # Q-Q Plot


-------------------------------------------------------------------------

#APRIORI ALGORITHM
  
customers_products <- read.csv("~/Downloads/customers_products.csv")

# Data Preprocessing
# Getting the dataset into the correct format
customers_products = BlackFriday %>%
select(User_ID, Product_ID) %>%          # Selecting the columns we will need
group_by(User_ID) %>%                   # Grouping by "User_ID"          
arrange(User_ID) %>%                  # Arranging by "User_ID" 
mutate(id = row_number()) %>%         # Defining a key column for each "Product_ID" and its corresponding "User_ID" (Must do this for spread() to work properly)
spread(User_ID, Product_ID) %>%      # Converting our dataset from tall to wide format, and grouping "Product_IDs" to their corresponding "User_ID"
t()                                  # Transposing the dataset from columns of "User_ID" to rows of "User_ID"

# Now we can remove the Id row we created earlier for spread() to work correctly.
customers_products = customers_products[-1,]

summary(customersProducts)

itemFrequencyPlot(customersProducts, topN = 25)    # topN is limiting to the top 50 products

rules = apriori(data = customersProducts,
                parameter = list(support = 0.008, confidence = 0.80, maxtime = 0)) # maxtime = 0 will allow our algorithim to run until completion with no time limit


inspect(sort(rules, by = 'lift'))

plot(rules, method = 'graph')
