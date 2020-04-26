# This dataset contains information on default payments, demographic factors, credit data, history of payment, 
# and bill statements of credit card clients in Taiwan from April 2005 to September 2005.

# these libraries need to be loaded
library(tidyverse)
library(lubridate)
library(caret)

# read the Dataset sheet into “R”. The dataset will be called "data".
# you may need to set your working directory with setwd()
#wd <- getwd()
#fullpath <- paste(wd, "data/UCI_Credit_Card.csv",sep="/")
#data <- read.csv(fullpath)

url <- "https://raw.githubusercontent.com/FenyGeri/CreditCardDefaults/master/UCI_Credit_Card.csv"
data <- read_csv(url)


# structure and meaning
str(data)
# ID: ID of each client
# LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit)
# SEX: Gender (1=male, 2=female)
# EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
# MARRIAGE: Marital status (1=married, 2=single, 3=others)
# AGE: Age in years
# PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, … 
#     8=payment delay for eight months, 9=payment delay for nine months and above)
# PAY_2: Repayment status in August, 2005 (scale same as above)
# PAY_3: Repayment status in July, 2005 (scale same as above)
# PAY_4: Repayment status in June, 2005 (scale same as above)
# PAY_5: Repayment status in May, 2005 (scale same as above)
# PAY_6: Repayment status in April, 2005 (scale same as above)
# BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)
# BILL_AMT2: Amount of bill statement in August, 2005 (NT dollar)
# BILL_AMT3: Amount of bill statement in July, 2005 (NT dollar)
# BILL_AMT4: Amount of bill statement in June, 2005 (NT dollar)
# BILL_AMT5: Amount of bill statement in May, 2005 (NT dollar)
# BILL_AMT6: Amount of bill statement in April, 2005 (NT dollar)
# PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)
# PAY_AMT2: Amount of previous payment in August, 2005 (NT dollar)
# PAY_AMT3: Amount of previous payment in July, 2005 (NT dollar)
# PAY_AMT4: Amount of previous payment in June, 2005 (NT dollar)
# PAY_AMT5: Amount of previous payment in May, 2005 (NT dollar)
# PAY_AMT6: Amount of previous payment in April, 2005 (NT dollar)
# default.payment.next.month: Default payment (1=yes, 0=no)

# problem: preview defaults (default.payment.next.month = 1)

# data exploration
# number of reported cases
all_lines <- dim(data)[1]

# defaults
defaults <- data %>% filter(default.payment.next.month==1) %>% summarise(n=n()) %>% pull(n)

# % of defaults
defaults/all_lines

# number of clients = number of lines (one client cannot be reported twice as data concerns only 6 months)
data %>% select(ID) %>% n_distinct()

##################
# population data
##################
# gender info
##################
data %>% select(SEX) %>% 
  mutate(SEX = case_when(
    .$SEX == 1 ~ "Male",
    .$SEX == 2 ~ "Female")
  ) %>% table()

# in % -> femnale prevalence is 1.5x more important 
data %>% select(SEX) %>%
  mutate(SEX = case_when(
    .$SEX == 1 ~ "Male",
    .$SEX == 2 ~ "Female")
  ) %>% table()  %>% prop.table() 


# gender / default info
data %>% select(SEX,default.payment.next.month) %>% 
  mutate(SEX = case_when(
    .$SEX == 1 ~ "Male",
    .$SEX == 2 ~ "Female")
  ) %>% 
  mutate(default.payment.next.month = case_when(
    .$default.payment.next.month == 0 ~ "No default",
    .$default.payment.next.month == 1 ~ "In default")
  ) %>% table() %>% knitr::kable()

# in %
data %>% select(SEX,default.payment.next.month) %>% 
  mutate(SEX = case_when(
    .$SEX == 1 ~ "Male",
    .$SEX == 2 ~ "Female")
  ) %>% 
  mutate(default.payment.next.month = case_when(
    .$default.payment.next.month == 0 ~ "No default",
    .$default.payment.next.month == 1 ~ "In default")
  ) %>% table() %>% prop.table() 

# in % / gender -> proportionally slightly more males are in default (to treat with care as we have mnore female observations)
data %>% select(SEX,default.payment.next.month) %>% 
  mutate(SEX = case_when(
    .$SEX == 1 ~ "Male",
    .$SEX == 2 ~ "Female")
  ) %>% 
  mutate(default.payment.next.month = case_when(
    .$default.payment.next.month == 0 ~ "No default",
    .$default.payment.next.month == 1 ~ "In default")
  ) %>% table() %>% prop.table(margin = 1) 

#######
# age 
#######
# younger generation with around 30y median age use credit cards mainly
data %>% ggplot(aes(AGE)) + geom_histogram()
data %>% group_by(AGE) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:10)

# basic stats -> positive skew, typical: mode < median < mean, long distribution tail on the right
# for reference: standard normal distribution: mean=0, stdev=1, skew=0, kurtosis=3, mean=mode=median
library(moments)
#function to get the mode of a distribution
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
data %>% select(AGE) %>% summary()
data %>% summarise(avg=mean(AGE), stdev=sd(AGE), SKEW=skewness(AGE), KURTOSIS=kurtosis(AGE), MODE=getmode(AGE))

# defaults per age
data %>% group_by(AGE) %>% summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all) %>% arrange(desc(percent))
# we should filter too small number of observations (regularization can also be used)
# highest
data %>% group_by(AGE) %>% summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all) %>% filter(all>10) %>% arrange(desc(percent))
# lowest
data %>% group_by(AGE) %>% summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all) %>% filter(all>10) %>% arrange(percent)

# seems however there is a connection between age and defaults
# defaults are under average around 30 and above average for younger and older generations
# the points are not randomly distributed around the average
# we should try to use age as a variable
data %>% group_by(AGE) %>% summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all) %>% ggplot(aes(AGE,percent)) + geom_point() + 
  geom_smooth() + geom_hline(yintercept = defaults/all_lines) + 
  geom_text(aes(20,defaults/all_lines,label="average default",vjust=1, hjust=0.3))

# create age intervals -> round to 10 y
data %>% mutate(AGE=as.integer(round(AGE/10))*10)  %>% group_by(AGE) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all) %>% filter(all>10) %>% arrange(percent) 

# we can see much better the tendancy
data %>% mutate(AGE=as.integer(round(AGE/10))*10)  %>% group_by(AGE) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all) %>% filter(all>10) %>% 
  ggplot(aes(AGE,percent)) + geom_point(size=4) + geom_hline(aes(yintercept = defaults/all_lines)) + geom_smooth() + 
  geom_text(aes(20,defaults/all_lines,label="average default",vjust=-1, hjust=-6))

################
# education
################
# mainly higher education people (graduate + university)
# the low number of observations in some groups does not allow general conclusions
data %>% select(EDUCATION) %>% table()
data %>% ggplot(aes(EDUCATION)) + geom_bar()

# defaults per group
# should retain only group 1-2-3 and conclude that higher education lowers default probability
# in other groups we do not have enough observations
data %>% group_by(EDUCATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(percent)

#############################################################
# MARRIAGE: Marital status (1=married, 2=single, 3=others)
#############################################################
# lower for single, higher form married and other
data %>% group_by(MARRIAGE) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(percent)

##########################################################################################################################
# PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, … 
#     8=payment delay for eight months, 9=payment delay for nine months and above)
##########################################################################################################################
# we can observe a clear relationship between delayed payments and defaults
data %>% group_by(PAY_0) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(percent)

data %>% group_by(PAY_0) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% ggplot(aes(PAY_0,percent)) + geom_point() + geom_smooth(method=lm)


########################################################################
# BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)
########################################################################
# most people have a low bill amount, but we cannot clearly see on a simple linear x-axis
data %>% ggplot(aes(BILL_AMT1)) + geom_histogram(bins=20) 

# we can be tempted to use a log transformation on x-axis, but we lose 0 values (log10 of 0 = -Inf)
data %>% ggplot(aes(BILL_AMT1)) + geom_histogram(bins=20) + scale_x_log10()

# by transforming 0 amounts to 1 we can treat this issue
data %>%  mutate(BILL_AMT1 = case_when(
  .$BILL_AMT1 == 0 ~ 1,
  .$BILL_AMT1 != 0  ~ .$BILL_AMT1)
) %>% ggplot(aes(BILL_AMT1)) + geom_histogram(bins=20) + scale_x_log10()

# is there a relationship between bill amount and default probability?
# we create two groups, higher and lower then median and examine default probability
# no evidence when we only have two groups
data %>% select(BILL_AMT1, default.payment.next.month) %>%
  mutate(BILL_AMT1 = case_when(
    .$BILL_AMT1 >= median(data$BILL_AMT1) ~ "More",
    .$BILL_AMT1 < median(data$BILL_AMT1) ~ "Less")
  ) %>% table()  %>% prop.table(margin = 1) 

# when we create more groups we see some evidence (rounded to 10k)
data %>% mutate(BILL_AMT1=as.integer(round(BILL_AMT1/10000))) %>% 
  mutate(BILL_AMT1=as.integer(round(BILL_AMT1/10)*10)) %>% 
  group_by(BILL_AMT1) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(desc(percent))

# we should exclude classes with too few observations then plot
# higher amount means higher risk
data %>% mutate(BILL_AMT1=as.integer(round(BILL_AMT1/10000))) %>% 
  mutate(BILL_AMT1=as.integer(round(BILL_AMT1/10)*10)) %>% 
  group_by(BILL_AMT1) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% 
  filter(all>10) %>% ggplot(aes(BILL_AMT1,percent)) + geom_point() +
  geom_smooth(method = "lm")

########################################################################
# PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)
# we should check the proportion to the bill amount (BILL_AMT1/PAY_AMT1)
# we need to filter out where PAY_AMT1 = 0
# then we compare the proportion  for defaults and no defaults
# for defaults this proportion is significantly higher (> 2x) -> this could be a signal of default
# but to treat with caution as the stdev is huge compared to the average values
########################################################################
data %>% filter(PAY_AMT1!=0) %>%   
  mutate(prop=BILL_AMT1/PAY_AMT1) %>% group_by(default.payment.next.month) %>%
  summarise(avg=mean(prop), stdev=sd(prop))

# visually -> we cannot clearly distinguish the two boxes
data %>% filter(PAY_AMT1!=0) %>%   
  mutate(prop=BILL_AMT1/PAY_AMT1, def=factor(default.payment.next.month)) %>% 
  ggplot(aes(def,prop)) +
  geom_boxplot() + scale_y_continuous(trans="log10")

########################################################################
# LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit)
# we should check the proportion to the bill amount (BILL_AMT1/LIMIT_BAL)
# closer to the limit means higher risk, higher default probability
# we don't need to filter out where LIMIT_BAL = 0 because it has no meaning
# then we compare the proportion  for defaults and no defaults
# for defaults this proportion is a littlebit higher (+20%) -> this could be a signal of default
# but to treat with caution as the stdev is significant compared to the average values
########################################################################
data %>% 
  mutate(prop=BILL_AMT1/LIMIT_BAL) %>% group_by(default.payment.next.month) %>%
  summarise(avg=mean(prop), stdev=sd(prop))

# visually -> we cannot clearly distinguish the two boxes
data %>% 
  mutate(prop=BILL_AMT1/LIMIT_BAL, def=factor(default.payment.next.month)) %>% 
  ggplot(aes(def,prop)) +
  geom_boxplot() + scale_y_continuous(trans="log2")

########################################################################
# PAY_6: Repayment status in April, 2005 (scale same as above) -> older data
# is it less significative ?
########################################################################
# we can observe a clear relationship between delayed payments and defaults
# results are comparable with PAY_0
data %>% group_by(PAY_6) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(percent)

data %>% group_by(PAY_6) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% ggplot(aes(PAY_6,percent)) + geom_point() + geom_smooth(method=lm)

# we should examine status migrations over months 
# migration is commmon as we can see by examining the five first cleints
data %>% filter(ID<=5) %>%
  select(ID, PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6) %>%
  gather(PAY,STATUS, PAY_0:PAY_6) %>% 
  mutate(MONTHS = case_when(
    .$PAY == "PAY_0" ~ -1,
    .$PAY == "PAY_2" ~ -2,
    .$PAY == "PAY_3" ~ -3,
    .$PAY == "PAY_4" ~ -4,
    .$PAY == "PAY_5" ~ -5,
    .$PAY == "PAY_6" ~ -6)
  ) %>% 
  ggplot(aes(MONTHS, STATUS, group=ID)) + geom_line() +
  facet_grid(. ~ ID)

# the proper solution would be fitting a line for each user and use the slope as a sign to determine whether 
# the financial status is better or worse
# for simplicity we can take simply the delta between PAY_6 and PAY_0 (MIGRATION = PAY_0-PAY_6)
data %>% mutate(MIGRATION = PAY_0-PAY_6) %>% group_by(MIGRATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(percent)

data %>% mutate(MIGRATION = PAY_0-PAY_6) %>% group_by(MIGRATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% ggplot(aes(MIGRATION,percent)) + geom_point() + geom_smooth(method=lm)

# we saw no evidence but if we filter MIRATION classes with too few observations results can be better
data %>% mutate(MIGRATION = PAY_0-PAY_6) %>% group_by(MIGRATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% 
  filter(all>50) %>% arrange(percent)

data %>% mutate(MIGRATION = PAY_0-PAY_6) %>% group_by(MIGRATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% 
  filter(all>50) %>% ggplot(aes(MIGRATION,percent)) + geom_point() + geom_smooth(method=lm)

# stable ratings seem to make less frequently default -> use abs(MIGRATION)
# the line fits much better, we see a clear relationship
data %>% mutate(MIGRATION = abs(PAY_0-PAY_6)) %>% group_by(MIGRATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% arrange(percent)

data %>% mutate(MIGRATION = abs(PAY_0-PAY_6)) %>% group_by(MIGRATION) %>% 
  summarise(all=n(),default=sum(default.payment.next.month)) %>%
  mutate(percent=default/all)  %>% ggplot(aes(MIGRATION,percent)) + geom_point() + geom_smooth(method=lm)

# our final formula will be MIGRATION = abs(PAY_0-PAY_6)


################################
# division to train + test set
################################
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data$default.payment.next.month, times = 1, p = 0.1, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]

# guessing:
y_hat <- factor(sample(0:1,3000,TRUE,c((all_lines-defaults)/all_lines,defaults/all_lines)))

##########################################################################
# test knn with all columns except customer id, using default parameters
##########################################################################
train <- train %>% select(-ID)
test <- test %>% select(-ID)
train <- train %>% mutate(default.payment.next.month=factor(default.payment.next.month))
test <- test %>% mutate(default.payment.next.month=factor(default.payment.next.month))
train_knn <- train(default.payment.next.month ~ ., method = "knn", data = train)
y_hat_knn <- predict(train_knn, test, type = "raw")
confusionMatrix(y_hat_knn, test$default.payment.next.month)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, test$default.payment.next.month)$byClass[["Sensitivity"]]
confusionMatrix(y_hat_knn, test$default.payment.next.month)$byClass[["Specificity"]]
confusionMatrix(y_hat_knn, test$default.payment.next.month)$byClass[["Balanced Accuracy"]]
# accuracy is quite high, sensitivity is high , but specificity is poor
# good accuracy is obtained, because prevalence of no default (0) is high and we previewed well the no defaults
# but defaults (default.payment.next.month=1) were poorly previewed, we previewed no default in much more cases
# balanced accuracy is not high, we are not really doing better than guessing with p=0.2 the default
# k used by default = 9 that gave the best accuracy
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel
# we keep results
results <- data_frame(method = "knn", 
                      Accuracy = confusionMatrix(y_hat_knn, test$default.payment.next.month)$overall[["Accuracy"]],
                      Sensitivity = confusionMatrix(y_hat_knn, test$default.payment.next.month)$byClass[["Sensitivity"]],
                      Specificity = confusionMatrix(y_hat_knn, test$default.payment.next.month)$byClass[["Specificity"]],
                      Balanced_Accuracy = confusionMatrix(y_hat_knn, test$default.payment.next.month)$byClass[["Balanced Accuracy"]],
                      )
######################################################################################
# test regression tree with all columns except customer id, using default parameters
######################################################################################
train_rpart <- train(default.payment.next.month ~ ., 
                     method = "rpart",
                     data = train)
ggplot(train_rpart)
train_rpart$bestTune
train_rpart$finalModel
y_hat_rpart <- predict(train_rpart, test, type = "raw")
confusionMatrix(y_hat_rpart, test$default.payment.next.month)
# we have better results, but we still predict with a relatively low specificity, failing to predict default in 2/3 of the cases
# the autumatically produced regression tree used payment status of last and 3rd month
# as we could expect, the PAY_0 column had the most impact (the payment status of the last month)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
# we keep results
results <- bind_rows(results,data_frame(method = "rpart", 
                      Accuracy = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$overall[["Accuracy"]],
                      Sensitivity = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$byClass[["Sensitivity"]],
                      Specificity = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$byClass[["Specificity"]],
                      Balanced_Accuracy = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$byClass[["Balanced Accuracy"]],
))

# for max flexibility we can set cp=0
train_rpart <- train(default.payment.next.month ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0, len = 1)),
                     data = train)
y_hat_rpart <- predict(train_rpart, test, type = "raw")
confusionMatrix(y_hat_rpart, test$default.payment.next.month)
# we have lower accuracy, but we predict with a slightly better specificity, but making more errors predicting 0
# the regression tree becomes really complicated
# as we could expect, the PAY_0 column had still the most impact (the payment status of the last month)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
 
var_imp <- data_frame(var = train_rpart$finalModel$frame$var[[1]],
                      importance = train_rpart$finalModel$variable.importance[[1]]
                      )

for(i in 2:10){
  var_imp <- bind_rows(var_imp,data_frame(var = train_rpart$finalModel$frame$var[[i]],
                        importance = train_rpart$finalModel$variable.importance[[i]]
  ))
}

var_imp %>% knitr::kable()

# we keep results
results <- bind_rows(results,data_frame(method = "rpart cp=0", 
                                        Accuracy = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$overall[["Accuracy"]],
                                        Sensitivity = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$byClass[["Sensitivity"]],
                                        Specificity = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$byClass[["Specificity"]],
                                        Balanced_Accuracy = confusionMatrix(y_hat_rpart, test$default.payment.next.month)$byClass[["Balanced Accuracy"]],
))

######################################################################################
# randomforest with all columns except customer id, using default parameters
######################################################################################
library(randomForest)
set.seed(1, sample.kind="Rounding")
train_rf <- randomForest(default.payment.next.month ~ ., data=train)
y_hat_rf <- predict(train_rf, test)
confusionMatrix(y_hat_rf,test$default.payment.next.month)
# we have even better results using random forest trees, specificity and balanced accuracy are both highest
results <- bind_rows(results,data_frame(method = "rforest", 
                                        Accuracy = confusionMatrix(y_hat_rf, test$default.payment.next.month)$overall[["Accuracy"]],
                                        Sensitivity = confusionMatrix(y_hat_rf, test$default.payment.next.month)$byClass[["Sensitivity"]],
                                        Specificity = confusionMatrix(y_hat_rf, test$default.payment.next.month)$byClass[["Specificity"]],
                                        Balanced_Accuracy = confusionMatrix(y_hat_rf, test$default.payment.next.month)$byClass[["Balanced Accuracy"]],
))

results %>% knitr::kable()

######################################################################################
# test of the same 3 algorithm with selected columns:
# SEX
# EDUCATION
# MARRIAGE
# AGE
# PAY_0
# PROP1 = BILL_AMT1/PAY_AMT1
# PROP2 = BILL_AMT1/LIMIT_BAL
# MIGRATION = abs(PAY_0-PAY_6)
######################################################################################
# add new columns + select columns to use + remove NA, Inf, -Inf
train2 <- train %>% mutate(PROP1=BILL_AMT1/PAY_AMT1, PROP2=BILL_AMT1/LIMIT_BAL, MIGRATION = abs(PAY_0-PAY_6)) %>% 
  select(SEX,EDUCATION,MARRIAGE,AGE,PAY_0,PROP1,PROP2,MIGRATION,default.payment.next.month)
train2 <- rbind(train2 %>% filter(is.na(PROP1)) %>% mutate(PROP1=0),train2 %>% filter(!is.na(PROP1)))
train2 <- rbind(train2 %>% filter(PROP1==Inf) %>% mutate(PROP1=1000),train2 %>% filter(PROP1!=Inf))
train2 <- rbind(train2 %>% filter(PROP1==-Inf) %>% mutate(PROP1=-1000),train2 %>% filter(PROP1!=-Inf))

test2 <- test %>% mutate(PROP1=BILL_AMT1/PAY_AMT1, PROP2=BILL_AMT1/LIMIT_BAL, MIGRATION = abs(PAY_0-PAY_6)) %>% 
  select(SEX,EDUCATION,MARRIAGE,AGE,PAY_0,PROP1,PROP2,MIGRATION,default.payment.next.month)
test2 <- rbind(test2 %>% filter(is.na(PROP1)) %>% mutate(PROP1=0),test2 %>% filter(!is.na(PROP1)))
test2 <- rbind(test2 %>% filter(PROP1==Inf) %>% mutate(PROP1=1000),test2 %>% filter(PROP1!=Inf))
test2 <- rbind(test2 %>% filter(PROP1==-Inf) %>% mutate(PROP1=-1000),test2 %>% filter(PROP1!=-Inf))

# let's start with rpart to identify the top 3 most important indicators and use them
# rpart -> forcing R to use more columns with a cp close to 0
train_rpart <- train(default.payment.next.month ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.001, len = 25)),
                     data = train2)

ggplot(train_rpart)
train_rpart$bestTune
y_hat_rpart <- predict(train_rpart, test2, type = "raw")
confusionMatrix(y_hat_rpart, test2$default.payment.next.month)
# we still predict with a relatively low specificity, failing to predict default in 2/3 of the cases
# forcing a cp close to 0 created a regression tree with PAY_0, MIGRATION, PROP2 and EDUCATION columns
# as we could expect, the PAY_0 column had the most impact (the payment status of the last month):
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# let's stick with the top 3 columns and use them with all algorithms 
# and results are superior in all means
# we still force cp close to 0 to use all the columns
train_rpart <- train(default.payment.next.month ~ PAY_0+PROP2+MIGRATION, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.001, len = 25)),
                     data = train2)

ggplot(train_rpart)
train_rpart$bestTune
y_hat_rpart <- predict(train_rpart, test2, type = "raw")
confusionMatrix(y_hat_rpart, test2$default.payment.next.month)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)


# add results
results2 <- data_frame(method = "rpart", 
                      Accuracy = confusionMatrix(y_hat_rpart, test2$default.payment.next.month)$overall[["Accuracy"]],
                      Sensitivity = confusionMatrix(y_hat_rpart, test2$default.payment.next.month)$byClass[["Sensitivity"]],
                      Specificity = confusionMatrix(y_hat_rpart, test2$default.payment.next.month)$byClass[["Specificity"]],
                      Balanced_Accuracy = confusionMatrix(y_hat_rpart, test2$default.payment.next.month)$byClass[["Balanced Accuracy"]],
)



# knn
train_knn <- train(default.payment.next.month ~ PAY_0+PROP2+MIGRATION, method = "knn", data = train2)
y_hat_knn <- predict(train_knn, test2, type = "raw")
confusionMatrix(y_hat_knn, test2$default.payment.next.month)
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

# results are much better than with the default knn executed on all columns
# we should try to train with different k values, using cross validation
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(default.payment.next.month ~ PAY_0+MIGRATION+PROP2, 
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(1,100,5)),
                   trControl = control,
                   data = train2)
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
y_hat_knn <- predict(train_knn, test2, type = "raw")
confusionMatrix(y_hat_knn, test2$default.payment.next.month)
results2 <- bind_rows(results2,data_frame(method = "knn", 
                      Accuracy = confusionMatrix(y_hat_knn, test2$default.payment.next.month)$overall[["Accuracy"]],
                      Sensitivity = confusionMatrix(y_hat_knn, test2$default.payment.next.month)$byClass[["Sensitivity"]],
                      Specificity = confusionMatrix(y_hat_knn, test2$default.payment.next.month)$byClass[["Specificity"]],
                      Balanced_Accuracy = confusionMatrix(y_hat_knn, test2$default.payment.next.month)$byClass[["Balanced Accuracy"]],
))



# randomforest
set.seed(1, sample.kind="Rounding")
train_rf <- randomForest(default.payment.next.month ~ PAY_0+MIGRATION+PROP2, data=train2)
y_hat_rf <- predict(train_rf, test2)
confusionMatrix(y_hat_rf,test2$default.payment.next.month)
# we have even better results using random forest trees, specificity an d balanced accuracy are both highest
results2 <- bind_rows(results2,data_frame(method = "rforest", 
                                        Accuracy = confusionMatrix(y_hat_rf, test2$default.payment.next.month)$overall[["Accuracy"]],
                                        Sensitivity = confusionMatrix(y_hat_rf, test2$default.payment.next.month)$byClass[["Sensitivity"]],
                                        Specificity = confusionMatrix(y_hat_rf, test2$default.payment.next.month)$byClass[["Specificity"]],
                                        Balanced_Accuracy = confusionMatrix(y_hat_rf, test2$default.payment.next.month)$byClass[["Balanced Accuracy"]],
))


# let's create an ensemble with the 3 models
y_hat_ensemble <- factor(ifelse(round((as.numeric(y_hat_knn) + as.numeric(y_hat_rpart) + as.numeric(y_hat_rf))/3)==1,0,1))
confusionMatrix(y_hat_ensemble,test2$default.payment.next.month)
results2 <- bind_rows(results2,data_frame(method = "ensemble", 
                                          Accuracy = confusionMatrix(y_hat_ensemble, test2$default.payment.next.month)$overall[["Accuracy"]],
                                          Sensitivity = confusionMatrix(y_hat_ensemble, test2$default.payment.next.month)$byClass[["Sensitivity"]],
                                          Specificity = confusionMatrix(y_hat_ensemble, test2$default.payment.next.month)$byClass[["Specificity"]],
                                          Balanced_Accuracy = confusionMatrix(y_hat_ensemble, test2$default.payment.next.month)$byClass[["Balanced Accuracy"]],
))

# compare results -> good results with the ensemble, but cannot beat the randomforset (rf can be already seen as an ensemble)
results2 %>% knitr::kable()

# creating an ensemble with some more models (takes time)
set.seed(1, sample.kind = "Rounding")
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "Rborist", "qda", "rf")

fits <- lapply(models, function(model){ 
  print(model)
  train(default.payment.next.month ~ PAY_0+MIGRATION+PROP2, method = model, data = train2)
}) 
names(fits) <- models

#create test previsons
t <- sapply(fits,function(x){
  y_hat <- predict(x,test2, type = "raw")
})

#dim of results
dim(t)

#accuracy, speciifcity, sensitivity and BA of each model
a <- sapply(seq(1,9),function(i){
  c(confusionMatrix(data=factor(t[,i]),reference = test2$default.payment.next.month)$overall[["Accuracy"]],
  confusionMatrix(data=factor(t[,i]),reference = test2$default.payment.next.month)$byClass[["Sensitivity"]],
  confusionMatrix(data=factor(t[,i]),reference = test2$default.payment.next.month)$byClass[["Specificity"]],
  confusionMatrix(data=factor(t[,i]),reference = test2$default.payment.next.month)$byClass[["Balanced Accuracy"]])
})
colnames(a) <- models
rownames(a) <- c("Accuracy", "Sensitivity","Specificity", "Balanced Accuracy")
a %>% knitr::kable()

#majority vote
y_hat <- factor(ifelse(rowMeans(t=="1")>0.5,"1","0"))
confusionMatrix(data=y_hat,reference = test2$default.payment.next.month)$overall["Accuracy"]
confusionMatrix(data=y_hat,reference = test2$default.payment.next.month)$byClass["Sensitivity"]
confusionMatrix(data=y_hat,reference = test2$default.payment.next.month)$byClass["Specificity"]
confusionMatrix(data=y_hat,reference = test2$default.payment.next.month)$byClass["Balanced Accuracy"]

