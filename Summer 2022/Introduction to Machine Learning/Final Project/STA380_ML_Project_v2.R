#STA380 Project 
#Group Members - Apurva Audi, James Anderson, Sameer Ahmed, Amanda Nguyen, Shubhada Kapre
# Kaggle Link for dataset : https://www.kaggle.com/datasets/arindam235/startup-investments-crunchbase?select=investments_VC.csv

df_inv <- read.csv(file = 'C:/Users/apurv/OneDrive/Desktop/investments_VC.csv')

head(df_inv,5)

dim(df_inv)

colnames(df_inv)
ncol(df_inv)


select_region <- c("Atlanta","Austin","Boston","Chicago","Los Angeles","New York City","Seattle","SF Bay Area")
df_usa <- subset(df_inv, country_code == "USA" & (region %in% select_region))

dim(df_usa)                                 

#df_usa is a subset of the entire dataset with specific regions within usa.

summary(df_usa)

#Cleaning data by finding null values
unique(df_usa$region) #unique Values of region

dim(df_usa)

df_usa <- df_usa %>% filter(!is.na(status))

df_usa <- na.omit(df_usa)

dim(df_usa)

#Data cleaning

df_usa$funding_total_usd <- as.numeric(gsub(",","",df_usa$funding_total_usd))

names(df_usa) <- gsub(" ", "_", names(df_usa)) #function to replace blanks in column names with an underscore


df_usa$funding_total_usd[is.na(df_usa$funding_total_usd)] <- 0 # converting Na to 0 as earlier some values were -

summary(df_usa)

#Adding a new column to categorize market as Technology or Non-Technology
#277 market categorized as Technology, rest as Non-Technology

dim(df_usa)

#turning all date columns in to date

df_usa$founded_at <- as.Date(df_usa$founded_at, "%m/%d/%Y")

df_usa$first_funding_at <- as.Date(df_usa$first_funding_at,format= "%m/%d/%Y")

df_usa$last_funding_at <- as.Date(df_usa$last_funding_at,format= "%m/%d/%Y")

df_usa$founded_year <- as.Date(df_usa$founded_year, format="%Y")


df_usa$market <- trimws(df_usa$market, which = c("both"))
#removing space from beginning and end of market column

dim(df_usa)

df_usa <-df_usa %>%
  rowwise() %>%
  mutate(
    total_investment = sum(c(seed,venture,equity_crowdfunding,undisclosed,convertible_note,debt_financing,angel,grant,private_equity,post_ipo_equity,post_ipo_debt,secondary_market,product_crowdfunding,round_A,round_B,round_C,round_D,round_E,round_F,round_G,round_H))
  )

dim(df_usa)

#creating new column for total investment

tech_list <- c("News","Curated Web","Analytics","Software","Electronics","Biotechnology","E-Commerce",
               "Software","Designers","Enterprise Software","Big Data","Hardware + Software","Social Fundraising",
               "Corporate IT","Bitcoin","Mobile","Electronic Health Records","Automotive","Web Hosting","Clean Technology",
               "Web Design","Cloud Computing","Technology","Video","Predictive Analytics","Telecommunications","Consulting",
               "Design","Social Games","Business Productivity","Communications Hardware","Health Care Information Technology",
               "Advertising Platforms","Cloud Infrastructure","Online Travel","Social Media","Cloud Management","Semiconductors",
               "3D","Technical Continuing Education","Internet of Things","Android","Semantic Search","Internet",
               "Business Intelligence","Financial Services","Ad Targeting","Development Platforms","Bio-Pharm","Semantic Web",
               "Assisitive Technology","SaaS","Data Visualization","Application Platforms","Social Commerce","Mobile Advertising",
               "Therapeutics","Wireless","Real Time","Finance Technology","Cloud Data Services","Machine Learning","Twitter Applications",
               "Sensors","Developer APIs","Aerospace","iPhone","Information Services","Consumer Goods","Startups","iPad",
               "Stock Exchanges","Computers","Big Data Analytics","Retail","Social Media Monitoring","Reviews and Recommendations",
               "Broadcasting","Trading","Pharmaceuticals","Smart Grid","Nanotechnology","Chemicals","Mobile Security","Databases",
               "Online Dating","Mobile Enterprise","Online Rental","Apps","Facebook Applications","Mobile Devices","Developer Tools",
               "PaaS","Application Performance Monitoring","Mobile Software Tools","QR Codes","Virtualization","Data Integration",
               "Mobile Games","Mobile Payments","Mobile Commerce","Web Tools","Payments","Video Games","Consumer Electronics",
               "Diabetes","Diagnostics","Networking","Tracking","E-Commerce Platforms","Productivity Software","Home Automation",
               "Email","Artificial Intelligence","Tablets","Industrial Automation","SEO","Computer Vision","Augmented Reality",
               "Life Sciences","Service Providers","Web CMS","Enterprises","Video Conferencing","Video Streaming","Photography",
               "Online Shopping","Email Marketing","Social Search","Cyber Security","Risk Management","Hardware","Social Network Media",
               "Information Technology","Cloud Security","Fantasy Sports","Web Development","IT and Cybersecurity","Open Source",
               "Mobile Health","Social Media Marketing","Infrastructure","Semiconductor Manufacturing Equipment","Robotics",
               "Blogging Platforms","Photo Sharing","Social Media Advertising","Image Recognition","3D Technology","E-Books",
               "Tech Field Support","Defense","File Sharing","Private Social Networking","Printing","Social Media Platforms",
               "Customer Support Tools","Biometrics","Navigation","Banking","Communications Infrastructure","Data Centers",
               "iOS","Project Management","Customer Service","Sales Automation","iPod Touch","Electric Vehicles","Social Bookmarking",
               "Google Apps","IaaS","Online Gaming","Proximity Internet","Browser Extensions","Information Security",
               "Meeting Software","Recipes","Online Scheduling","Embedded Hardware and Software","Product Search","Network Security",
               "Consumer Internet","Energy Management","Data Mining","B2B","Cosmetics","Enterprise Application","Data Security",
               "Video on Demand","App Discovery","Drones","Intelligent Systems","Environmental Innovation","Energy Efficiency",
               "Virtual Workforces","Bioinformatics","Renewable Energies","Internet TV","Business Information Systems",
               "Corporate Wellness","Speech Recognition","Retail Technology","Online Video Advertising","Green Consumer Goods",
               "MicroBlogging","Social CRM","Enterprise Resource Planning","Algorithms","Web Browsers","Presentations",
               "3D Printing","Mobile Coupons","Linux","Audio","Online Reservations","Batteries","App Marketing","Corporate Training",
               "Dental","Biotechnology and Semiconductor","Social + Mobile + Local","Human Resource Automation","Internet Infrastructure",
               "Visualization","Mobile Shopping","Consumer Behavior","Business Analytics","Product Development Services",
               "App Stores","Product Design","Virtual Goods","Cyber","Social Innovation","Enterprise Search","Systems",
               "Energy IT","Social Buying","Mac","Mechanical Solutions","Government Innovation","IT Management","Price Comparison",
               "Enterprise 2.0","Virtual Currency","Photo Editing","Google Glass","High Tech","Innovation Engineering",
               "Mobile Infrastructure","Gps","Mining Technologies","Data Center Infrastructure","Data Center Automation",
               "Social Media Management","Internet Technology","Mobile Video","Social Business","Cosmetic Surgery",
               "Social Opinion Platform","Windows Phone 7","Video Chat","Early-Stage Technology","Fraud Detection",
               "Enterprise Purchasing","Electrical Distribution","Advanced Materials","Mobile Analytics","Enterprise Security",
               "Health Services Industry","Reading Apps","CAD","Human Computer Interaction","Text Analytics","RFID",
               "Material Science","Service Industries","Natural Language Processing")
df_usa$market_type <- ifelse(df_usa$market %in% tech_list,"Technology","Non-Technology")

#Checkpoint
#write.csv(df_usa,"C:/Users/apurv/OneDrive/Desktop/df_usa_labelled1.csv", row.names = TRUE)

unique(df_usa$market_type)

dim(df_usa)


#install.packages("lubridate")             
library("lubridate")  
df_usa$diff_funding_months <- interval(df_usa$first_funding_at,df_usa$last_funding_at) %/% months(1) 
df_usa$diff_funding_year <- df_usa$diff_funding_months/12

dim(df_usa)

#Removing all null values -- Left with 12812 values


drop <- c('permalink','homepage_url', 'category_list','market','country_code', 'state_code', 'founded_at', 'founded_month', 'founded_quarter', 'founded_year', 
          'funding_total_usd', 'first_funding_at', 'last_funding_at','diff_funding_months')

df_usa = df_usa[,!(names(df_usa) %in% drop)]
df_usa <- df_usa %>% 
  mutate_all(~ifelse(. %in% c(""," "), NA, .)) %>% 
  na.omit()

dim(df_usa)

#Histogram -- Will take care of this later as EDA : cut in dplyr
#install.packages("ggplot2") 
library("ggplot2")

#install.packages("tidyr") 
library("tidyr")

#install.packages("tidyverse")
library("tidyverse")

names(df_usa)

#Confirming the skewed data or imbalance in dataset
ggplot_df <- ggplot(df_usa, aes(x = funding_rounds,color = status)) +    # Draw each column as histogram
  geom_histogram() + 
  facet_wrap(~ region, scale="free")
ggplot_df

ggplot_df2 <- ggplot(df_usa, aes(x = market_type,color = status)) +    # Draw each column as density
  geom_density() + 
  facet_wrap(~ region, scales = "free")
ggplot_df2

#Creating histogram of all numerical values, all values are very skewed

df_usa$market_type <- ifelse(df_usa$market_type == "Non-Technology", 0, 1)
df_usa$equity_crowdfunding <- ifelse(df_usa$equity_crowdfunding < 1 , 0 , 1)
df_usa$undisclosed <- ifelse(df_usa$undisclosed < 1 , 0 , 1)
df_usa$convertible_note <- ifelse(df_usa$convertible_note < 1 , 0 , 1)
df_usa$debt_financing <- ifelse(df_usa$debt_financing < 1 , 0 , 1)
df_usa$angel <- ifelse(df_usa$angel < 1 , 0 , 1)
df_usa$grant <- ifelse(df_usa$grant < 1 , 0 , 1)
df_usa$private_equity <- ifelse(df_usa$private_equity < 1 , 0 , 1)
df_usa$post_ipo_equity <- ifelse(df_usa$post_ipo_equity < 1 , 0 , 1) 
df_usa$post_ipo_debt <- ifelse(df_usa$post_ipo_debt < 1 , 0 , 1)
df_usa$secondary_market <- ifelse(df_usa$secondary_market < 1 , 0 , 1)
df_usa$product_crowdfunding <- ifelse(df_usa$product_crowdfunding < 1 , 0 , 1)
df_usa$round_A <- ifelse(df_usa$round_A < 1 , 0 , 1)
df_usa$round_B <- ifelse(df_usa$round_B < 1 , 0 , 1)
df_usa$round_C <- ifelse(df_usa$round_C < 1 , 0 , 1)
df_usa$round_D <- ifelse(df_usa$round_D < 1 , 0 , 1)
df_usa$round_E <- ifelse(df_usa$round_E < 1 , 0 , 1)
df_usa$round_F <- ifelse(df_usa$round_F < 1 , 0 , 1)
df_usa$round_G <- ifelse(df_usa$round_G < 1 , 0 , 1)
df_usa$round_H <- ifelse(df_usa$round_H < 1 , 0 , 1)

library(tidyverse)

library(dplyr)

df_usa <- mutate(df_usa,status_label = ifelse(status =='closed',0,1))

unique(df_usa$status_label)

#Creating categories of these numerical values based on the output from the describe data. Also creating new column for the categories
v <- df_usa %>% select(total_investment,diff_funding_year,funding_rounds,seed,venture)
summary(v)

#Creating bins
library(dplyr)
df_usa <- df_usa %>% mutate(total_investment_label = cut(total_investment,breaks=c(0,300000,3125000,22180000,30080000000),labels = c('low','low_medium','high_medium','high'),include.lowest = TRUE))
df_usa <- df_usa %>% mutate(diff_funding_year_label = cut(diff_funding_year,breaks=c(0, 1.322, 27.5),labels = c('low','high'),include.lowest = TRUE))
df_usa <- df_usa %>% mutate(funding_rounds_label = cut(funding_rounds,breaks=c(0,2.114,15),labels = c('low','high'),include.lowest = TRUE))
df_usa <- df_usa %>% mutate(seed_label = cut(seed,breaks=c(0,339905,100000000),labels = c('low','high'),include.lowest = TRUE))
df_usa <- df_usa %>% mutate(venture_label = cut(venture,breaks=c(0,1200000,13100000,1506000000),labels = c('low','medium','high'),include.lowest = TRUE))

#Encoding the labels with into numbers

library(dplyr)


df_usa <- mutate(df_usa,total_investment_cde = ifelse(total_investment_label =='low',0, 
                                                      ifelse(total_investment_label =='low_medium',1,
                                                             ifelse(total_investment_label =='high_medium',2,3))))

df_usa <- mutate(df_usa,diff_funding_year_cde = ifelse(diff_funding_year_label =='low',0,1)) 
df_usa <- mutate(df_usa,funding_rounds_cde = ifelse(funding_rounds_label =='low',0,1)) 
df_usa <- mutate(df_usa,seed_cde = ifelse(seed_label =='low',0,1)) 
df_usa <- mutate(df_usa,venture_cde = ifelse(venture_label =='low',0,
                                             ifelse(venture_label =='medium',1,2))) 


#Installing package ROSE for imbalanced data oversampling
#install.packages("ROSE")
library(ROSE)

library(rpart)
#over sampling
table(df_usa$status_label)

train <- data[ind==1,]
test <- data[ind==2,]

data_balanced_over <- ovun.sample(status_label ~ ., data = df_usa, method = "over",N = 24190)$data
table(data_balanced_over$status_label)

# data_balanced_over is the final dataframe after oversampling

summary(data_balanced_over)

df_inv <- data_balanced_over

#Boxplots
first = boxplot(df_inv$total_investment~df_inv$status_label, outline=FALSE)
second = boxplot(df_inv$funding_rounds~df_inv$status_label, outline=FALSE)
third = boxplot(df_inv$venture~df_inv$status_label, outline=FALSE)
fourth = boxplot(df_inv$seed~df_inv$status_label, outline=FALSE)
fifth = boxplot(df_inv$diff_funding_year~df_inv$status_label, outline=FALSE)


# Modelling

# 1) Single Variable Logistic Regression
#Creation of Training and Test Sets

data_set_size = nrow(df_inv)
print(data_set_size)
test_set_size = round(0.30*data_set_size)
print(test_set_size)
RNGkind(sample.kind = "Rounding")
set.seed(8239)
tickets = sample(data_set_size,test_set_size)
df_Test = df_inv[tickets,]
df_Train = df_inv[-tickets,]
df_Train <- na.omit(df_Train)
df_Test <- na.omit(df_Test)

logistic_fit= glm(status_label ~ funding_rounds, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ total_investment, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ venture, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ seed, data=df_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status_label ~ diff_funding_year, data=df_Train,family='binomial')
summary(logistic_fit)

#Attempt to fit probability vs eta. Our inability to do so made us scrap this model
#from our final project. It is clear here that there isn't a properly defined eta,
#and the graph is interpreting the code as something to draw a line graph of. 
z = seq(from=-5,to=5,length.out=1000)
Fz = exp(z)/(1+exp(z))
plot(z,Fz,type='l',col='blue',lwd=2.5,xlab=expression(eta),ylab='F',cex.lab=1.3)
eta = predict(logistic_fit)
pyx = predict(logistic_fit,type='response')
plot(eta,pyx,type='l',col='blue',lwd=2.5,xlab=expression(eta),ylab='F',cex.lab=1.3)

# 2) Lasso Regression, Ridge Regression

# lasso regression 
library(readr)

data_set_size = nrow(df_inv)
test_set_size = round(0.30*data_set_size)

df_inv$status = as.factor(df_inv$status)

df_inv$region = as.factor(df_inv$region)
df_inv$status = as.factor(df_inv$status)

lm.fit = lm(status_label~  total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, data = df_inv)
summary(lm.fit)

library(glmnet)
set.seed(1)
library(dplyr)
train = sample(1:nrow(df_inv),nrow(df_inv)/2)
test=(-train)
y.test = y[test]

x = model.matrix(status_label ~ + market_type + total_investment_cde + diff_funding_year_cde + funding_rounds_cde + seed_cde + venture_cde, data = df_inv)[, -1]
y = df_inv$status_label + df_inv$total_investment_cde + df_inv$diff_funding_year_cde + df_inv$funding_rounds_cde + df_inv$seed_cde + df_inv$venture_cde + df_inv$market_type

cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)

cv.out$lambda.min

lasso.startup = glmnet(x[train,], y[train], alpha = 1, thresh = 1e-12, lambda = grid)
grid = 10^seq(10, -2, length = 100)
plot(lasso.startup)
summary(lasso.startup)
lasso.pred = predict(lasso.startup, s=cv.out$lambda.min, newx = x[test, ])
sqrt(mean((lasso.pred - y.test)^2))


out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = 'coefficients', s=cv.out$lambda.min)[1:7,]
lasso.coef

# ridge regression 
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
plot(ridge.mod)
dim(coef(ridge.mod))
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
cv.out$lambda.1se
cv.out$lambda.min
ridge.pred = predict(cv.out, s = cv.out$lambda.min, newx = x[test, ], type = 'coefficients')[1:7,]
sqrt(mean((ridge.pred - y.test)^2))
ridge.pred
# 3) Logistic Regression

#Reading in the Data
##needs to be re-read 
#df_inv = read.csv(file = 'StartupSuccessOverSampled.csv')

#changing it to two factor (re run this again)
#df_inv$status[df_inv$status == 'acquired'] <- 'success' 
#df_inv$status[df_inv$status == 'operating'] <- 'success'

df_inv$status = as.factor(df_inv$status)
df_inv$region = as.factor(df_inv$region)
df_inv$status = as.factor(df_inv$status)

max(df_inv$total_investment)
which(df_inv$total_investment == 30079503000)

#removing Verizon
df_inv = df_inv[-c(11243), ]


##getting test data 
data_set_size = nrow(df_inv) 
test_set_size = round(0.30*data_set_size) #I want a 20% validation set.
RNGkind(sample.kind = "Rounding")
set.seed(8239) 
tickets = sample(data_set_size,test_set_size)
df_inv_Test = df_inv[tickets,]
df_inv_Train = df_inv[-tickets,]


#regsubset
library(leaps)
build = regsubsets(status ~ diff_funding_year + market_type + funding_rounds + total_investment_cde + seed_label + venture_label + funding_rounds_label, data = df_inv_Train, method = "backward", nvmax=6)
plot(build, scale="r2")

#different variable choice
logistic_fit= glm(status ~ funding_rounds + total_investment_cde,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment, data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~  funding_rounds  + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ region + funding_rounds + total_investment + seed_label + venture_label + funding_rounds_label,data=df_inv_Train,family='binomial')
summary(logistic_fit)

logistic_fit= glm(status ~ diff_funding_year + market_type + funding_rounds + seed_label + total_investment_cde,data=df_inv_Train,family='binomial')
summary(logistic_fit)



prob_success_pred=predict(logistic_fit,df_inv_Test,type="response")

pred_success = ifelse(prob_success_pred>.55,"success","closed")
table(df_inv_Test$status, pred_success)

install.packages('caret')
library(caret)

confusionMatrix(as.factor(df_inv_Test$status), as.factor(pred_success), mode = "everything")

mean(pred_success==df_inv_Test$status)
mean(pred_success!=df_inv_Test$status)


##ploting lift

install.packages("pROC")
install.packages('ROCR')

library(ROCR)
library(pROC)


ROCR_pred_test <- prediction(prob_success_pred,df_inv_Test$status)

ROCR_perf_test <- performance(ROCR_pred_test,'tpr','fpr')

plot(ROCR_perf_test,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
abline(0,1,lty=2)

auc_value <- roc(df_inv_Test$status, prob_success_pred)
auc(auc_value)

library(MLmetrics)
library(caret)
library(tree)

#Factoring the data
df_startup = subset(df_inv, select = c('status_label','market_type',
                                       'total_investment','diff_funding_year','seed','venture','funding_rounds') )
str(df_startup)
df_startup$market_type <- as.factor(df_startup$market_type)
df_startup$funding_rounds <- as.numeric(df_startup$funding_rounds)
df_startup$seed <- as.numeric(df_startup$seed)
df_startup$status_label <- as.factor(df_startup$status_label)
attach(df_startup)

#Splitting the data set into training and testing data
set.seed (2)
rows=dim(df_startup)[1]
tr <- sample(1:rows, (0.7*rows))
train=data.frame(df_startup)
test_data=data.frame(df_startup)
train=train[tr,]
test_data=test_data[-tr,]

#Predicting using single tree
tree.status <- tree(status_label ~ .-status_label,data=train,mindev=.001)
summary(tree.status)
plot(tree.status)
text(tree.status , pretty = 0)

tree.pred <- predict(tree.status ,test_data ,
                     type = "class")
table(tree.pred , test_data$status_label)
confusionMatrix(tree.pred, test_data$status_label,
                mode = "everything")

#Bagging
#install.packages('randomForest')
library(randomForest)
set.seed (10)
bag.status <- randomForest(status_label ~ ., data = train ,
                           mtry = 6, importance = TRUE)

#Predicting using Bagging
yhat.status <- predict(bag.status , test_data, type = 'class')
confusionMatrix(yhat.status, test_data$status_label,
                mode = "everything")

tree.number = c(200,400,500,1000,2000,3000,5000,6000,7000)
length.tree.number = length(tree.number)
train.errors.tree = rep(NA, length.tree.number)
test.errors.tree = rep(NA, length.tree.number)

for (i in 1:length.tree.number) {
  random.status = randomForest(status_label ~ ., data = train, mtry = 6, ntree = tree.number[i])
  train.pred = predict(random.status, train, n.trees = tree.number[i])
  test.pred = predict(random.status, test_data, n.trees = tree.number[i])
  table.status = table(test.pred , test_data$status_label)
  test.errors.tree[i]=(table.status[1]+table.status[4])/(table.status[1]+table.status[4]+table.status[2]+table.status[3])
}

plot(tree.number, test.errors.tree, type = "b", xlab = "tree number", ylab = "Accuracy", col = "red", pch = 20)
lines(tree.number,test.errors.tree,type = "b",col="blue", pch = 20)
max(test.errors.tree)
tree.number[which.max(test.errors.tree)]


#Random Forest
set.seed (12)
rf.status <- randomForest(status_label ~ ., data = train ,
                          mtry = 4, importance = TRUE)

yhat.rf <- predict(rf.status, test_data, type = 'class')
confusionMatrix(yhat.rf, test_data$status_label,
                mode = "everything")
importance(rf.status)
varImpPlot(rf.status)

#Boosting
library(gbm)
set.seed (19)
train$status_label <- as.character(train$status_label)
boost.startup <- gbm(train$status_label ~ ., data = train,
                     distribution = "bernoulli", n.trees = 4000, shrinkage = .2,
                     interaction.depth = 3)
summary(boost.startup)

pred.boost <- predict.gbm(boost.startup ,
                          test_data, n.trees = 4000,type='response',verbose=FALSE)

pred.boost
pred.boost = factor(ifelse(pred.boost<'0.5',"0","1"))
confusionMatrix(pred.boost, as.factor(test_data$status_label),
                mode = "everything")