training <- read.csv("/Users/herrickmak/Documents/Paidy_interview/GiveMeSomeCredit/cs-training.csv")
test <- read.csv("/Users/herrickmak/Documents/Paidy_interview/GiveMeSomeCredit/cs-test.csv")
original_training <- training
original_test <- test
library(tidyverse)
library(gplots)
library(caTools)
library(e1071)
# summary
summary(training)

#===============================================================
#data wrangling
# replace MonthlyIncome NA's to mean
training[is.na(training[,7]),7] <- mean(training[,7],na.rm=TRUE)

# replace NumberOfDependents NA's to mode
a <- table(training$NumberOfDependents)
training[is.na(training[,12]),12] <- which.max(a)

# replace MonthlyIncome NA's to mean
test[is.na(test[,7]),7] <- mean(test[,7],na.rm=TRUE)

# replace NumberOfDependents NA's to mode
b <- table(test$NumberOfDependents)
test[is.na(test[,12]),12] <- which.max(b)

# remove first column as it is not useful for test
training = training[2:12]
test = test[2:12]

#data wrangling end
#===============================================================

#===============================================================
#heatmap

training.corr <- cor(training)
colnames(training.corr) <- c("X","SD2","RevUtilUnsecLines","age","#30-59","DebtRatio","MonthyIncome",
                         "#CreditLines&Loans","#90dayslate","#RealEstateLoans/lines","#60-89","#Dependents")
rownames(training.corr) <- c("X","SD2","RevUtilUnsecLines","age","#30-59","DebtRatio","MonthyIncome",
                         "#CreditLines&Loans","#90dayslate","#RealEstateLoans/lines","#60-89","#Dependents")
corrplot(training.corr, method="color")




# cast SeriousDlqin2yrs into categorical factor
training$SeriousDlqin2yrs <- factor(training$SeriousDlqin2yrs, levels=c(0,1))

# split the dataset to training and test
set.seed(123)
split <- sample.split(training$SeriousDlqin2yrs, SplitRatio = 0.8) 

training_set <- subset(training, split == TRUE) 
test_set <- subset(training, split == FALSE)


# Feature scaling to z
training_set[c(-1)] = scale(training_set[c(-1)])
test_set[c(-1)] = scale(test_set[c(-1)])

#==========================================================
# Fitting SVM to the Training set 


classifier <- svm(formula = SeriousDlqin2yrs ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear')

y_pred = predict(classifier, newdata = test_set[-1])

#Making the Confusion Matrix 
cm <- table(test_set[, 1], y_pred)



# vector 
y_pred_test <- predict(classifier, newdata=test[-1])

# replace NA with result
test$SeriousDlqin2yrs <- y_pred_test

# predict probability
prob_pred <- predict(classifier, newdata=test[-1],type="prob")

#classifier end
#==========================================================

#==========================================================
# Naive Bayes
nb <- naiveBayes(training_set$SeriousDlqin2yrs~.,
                 data = training_set[,-1])
nb_predict <- predict(nb, test, type="raw")
nb_predict_plot <- as.data.frame(nb_predict)

# stack the dataset
stacked_nb_predict <- stack(nb_predict_plot)

# plot
stacked_nb_predict %>% filter(ind==1) %>% 
  ggplot(aes(x=values)) + geom_histogram(bins = 50) + ggtitle("Naive Bayes") +
  theme(plot.title=element_text(hjust=0.5))


#Naive Bayes end
#==========================================================

#==========================================================
#Random Forest
library(randomForest)
rf <- randomForest(training_set$SeriousDlqin2yrs~.,
                 data = training_set[,-1])
rf_predict <- predict(rf, test, type="prob")
rf_predict_plot <- as.data.frame(rf_predict)

# stack the dataset
stacked_rf_predict <- stack(rf_predict_plot)

# plot
stacked_rf_predict %>% filter(ind==1) %>% 
  ggplot(aes(x=values)) + geom_histogram(bins = 50) + ggtitle("Random Forest") + 
  theme(plot.title=element_text(hjust=0.5))


# Random Forest End
#==========================================================

#age distribution
ggplot(data=cs-training,aes(x=age))+geom_bar()+
  ggtitle("Age distribution")+theme(plot.title=element_text(hjust=0.5))

#debtratio distribution
ggplot(data=cs-training,aes(x=log2(DebtRatio)))+geom_density()

#income distrubution
ggplot(data=cs-training,aes(x=MonthlyIncome))+geom_density()
       
