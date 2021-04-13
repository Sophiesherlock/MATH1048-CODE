#This script is designed to create a model to predict diabetes

# loading libraries -------------------------------------------------------

library(PerformanceAnalytics)
library(ggplot2)
library(GGally)
library(caret)


# import data -------------------------------------------------------------
diabetes_data <- read.csv("C:/Users/sophi/OneDrive/Desktop/uni year 3/MATH1048 - Research Project/individual project/Final Project/diabetes_data.csv", stringsAsFactors = F)

# cleaning data - part 1 -----------------------------------------------------------

colnames(diabetes_data)[7] <- "DPF"
colnames(diabetes_data)[9] <- "Diabetes"

#change the diabetes column so no=0 and yes=1, these are as factors
diabetes_data$Diabetes <- as.factor(diabetes_data$Diabetes)

#create another version of the data that we will clean - can compare this to old data
diabetes_data_clean <- diabetes_data

#we have 0's for some of the minimum entries which don't make sense, so need to replace these with NA
diabetes_data_clean$Glucose[diabetes_data_clean$Glucose == 0] <- NA
diabetes_data_clean$BloodPressure[diabetes_data_clean$BloodPressure == 0] <- NA
diabetes_data_clean$SkinThickness[diabetes_data_clean$SkinThickness == 0] <- NA
diabetes_data_clean$Insulin[diabetes_data_clean$Insulin == 0] <- NA
diabetes_data_clean$BMI[diabetes_data_clean$BMI == 0] <- NA
# 
# 
# #replacing the null values with either mean or median depending on whether they are even or skewed
# diabetes_data_clean$Glucose[is.na(diabetes_data_clean$Glucose)] <- median(diabetes_data_clean$Glucose, na.rm = TRUE)
# diabetes_data_clean$BloodPressure[is.na(diabetes_data_clean$BloodPressure)] <- mean(diabetes_data_clean$BloodPressure, na.rm = TRUE)
# diabetes_data_clean$SkinThickness[is.na(diabetes_data_clean$SkinThickness)] <- median(diabetes_data_clean$SkinThickness, na.rm = TRUE)
# diabetes_data_clean$Insulin[is.na(diabetes_data_clean$Insulin)] <- median(diabetes_data_clean$Insulin, na.rm = TRUE)
# diabetes_data_clean$BMI[is.na(diabetes_data_clean$BMI)] <- mean(diabetes_data_clean$BMI, na.rm = TRUE)
# 
# #create standardised data set for certain model
# preproc1 <- preProcess(diabetes_data_clean[, c(1:8)], method = c("center", "scale"))
# stand_data <- predict(preproc1, diabetes_data_clean[,c(1:8)])
# stand_data$Diabetes <- diabetes_data_clean$Diabetes
# 
# #create normalised data set for certain model
# preproc2 <- preProcess(diabetes_data_clean[, c(1:8)], method = c("range"))
# norm_data <- predict(preproc2,diabetes_data_clean[,c(1:8)] )
# norm_data$Diabetes <- diabetes_data_clean$Diabetes

#note we now have diabetes data clean, stand data and norm data

# EDA ---------------------------------------------------------------------

#understand the data so we can determine how to replace the NA values

#plot histograms for the predictors containing NA/0 values
library(Hmisc)
par(mar=c(2,2,2,2))
hist(diabetes_data$Glucose)
hist(diabetes_data$BloodPressure)
hist(diabetes_data$SkinThickness)
hist(diabetes_data$Insulin)
hist(diabetes_data$BMI)
#from the histogram, can see that some distributions are even whilst others are skewed

#plot density plots to determine curve for these predictors
ggplot(data = diabetes_data, aes(x = Glucose))+
  geom_density()
ggplot(data = diabetes_data, aes(x = BloodPressure))+
  geom_density()
ggplot(data = diabetes_data, aes(x = SkinThickness))+
  geom_density()
ggplot(data = diabetes_data, aes(x = Insulin))+
  geom_density()
ggplot(data = diabetes_data, aes(x = BMI))+
  geom_density()
#looking at the density plots confirms even distribution for BP and BMI, skewed distribution for other predictors

library(ggplot2)
library(GGally)

#look at the relations between each variable including diabetes in more detail
ggpairs(diabetes_data, aes(color=Diabetes, alpha=0.75), lower=list(continuous="smooth"))+
  theme_bw()+
  labs(title="Correlation Plot of Variance (diabetes)")+
  theme(plot.title=element_text(face='bold', color='black', hjust=0.5, size=12))

#shows the correlation between the variable with some colour - not needed - just extra graph to consider
ggcorr(diabetes_data[,-9], name="corr", label = TRUE)+
  theme(legend.position = "none")+
  labs(title = "Correlation Plot of Variance")+
  theme(plot.title = element_text(face='bold', color='black', hjust=0.5, size=12))

# cleaning data - part 2 ----------------------------------------------------------

#now we know the distributions of the NA predictors, the NA/0 values vcan be replaced with mean or median:

#replacing the null values with either mean or median depending on whether they are even or skewed
diabetes_data_clean$Glucose[is.na(diabetes_data_clean$Glucose)] <- median(diabetes_data_clean$Glucose, na.rm = TRUE)
diabetes_data_clean$BloodPressure[is.na(diabetes_data_clean$BloodPressure)] <- mean(diabetes_data_clean$BloodPressure, na.rm = TRUE)
diabetes_data_clean$SkinThickness[is.na(diabetes_data_clean$SkinThickness)] <- median(diabetes_data_clean$SkinThickness, na.rm = TRUE)
diabetes_data_clean$Insulin[is.na(diabetes_data_clean$Insulin)] <- median(diabetes_data_clean$Insulin, na.rm = TRUE)
diabetes_data_clean$BMI[is.na(diabetes_data_clean$BMI)] <- mean(diabetes_data_clean$BMI, na.rm = TRUE)

#create standardised data set for certain model
preproc1 <- preProcess(diabetes_data_clean[, c(1:8)], method = c("center", "scale"))
stand_data <- predict(preproc1, diabetes_data_clean[,c(1:8)])
stand_data$Diabetes <- diabetes_data_clean$Diabetes

#create normalised data set for certain model
preproc2 <- preProcess(diabetes_data_clean[, c(1:8)], method = c("range"))
norm_data <- predict(preproc2,diabetes_data_clean[,c(1:8)] )
norm_data$Diabetes <- diabetes_data_clean$Diabetes

#note we now have diabetes data clean, stand data and norm data

# Baseline Model ----------------------------------------------------------

#baseline model - should not use a model whose accuracy is less than the baseline model
table(diabetes_data_clean$Diabetes)
baseline <- round(500/nrow(diabetes_data_clean), 2)
baseline


# create training and testing data ----------------------------------------

#clean diabetes data
nrows <- NROW(diabetes_data_clean)
set.seed(123) #fix random value otherwise will change each time
index <- sample(1:nrows, 0.75*nrows) #shuffle and divide
train_clean <- diabetes_data_clean[index,] # training data
test_clean <- diabetes_data_clean[-index,] # testing data
#check the proportion of diabetes in the training and testing set
prop.table(table(train_clean$Diabetes))
prop.table(table(test_clean$Diabetes))

#standardised data
nrows_stand <- NROW(stand_data)
set.seed(123)
index_stand <- sample(1:nrows_stand, 0.75*nrows_stand)
train_stand <- stand_data[index_stand,]
test_stand <- stand_data[-index_stand,] 
prop.table(table(train_stand$Diabetes))
prop.table(table(test_stand$Diabetes))

#normalised data
nrows_norm <- NROW(norm_data)
set.seed(123)
index_norm <- sample(1:nrows_norm, 0.75*nrows_norm)
train_norm <- norm_data[index_norm,]
test_norm <- norm_data[-index_norm,] 
prop.table(table(train_norm$Diabetes))
prop.table(table(test_norm$Diabetes))


# Creating Models ---------------------------------------------------------

# Naive Bayes Model -------------------------------------------------------------

#this model uses the diabetes_data_clean data

library(e1071)
library(clipr)

#building and training model

#create objects which hold the predictor variables and response variables
x=train_clean[,-9]
y=train_clean$Diabetes

nb_model <- train(x,y,'nb', trControl = trainControl(method = 'cv', number = 10))
confusionMatrix(nb_model)

#evaluating model - predict testing set
nb_predict <- predict(nb_model, newdata = test_clean)
confusionMatrix(nb_predict, test_clean$Diabetes)

#plot shows how each predictor variable is independently responsible for predicting the outcome
X <- varImp(nb_model)
plot(X)


# Logistic Regression Model -----------------------------------------------------

#this model uses the diabetes_data_clean data

library(broom)
library(tidyverse)
library(modelr)
library(ISLR)

#building and training the model
tbl_data <- as_tibble(diabetes_data_clean)
log_model <- glm(Diabetes ~ Glucose + Pregnancies + BloodPressure + SkinThickness + Insulin + BMI + DPF + Age,  train_clean, family = "binomial")
summary(log_model)
#this tells us that the significant parameters are constant, glucose, pregnancies, BMI and DPF (AIC = 542.42)

#remove the insignificant predictors to make model better
#try this twice with the 2 different versions
log_model_2 <- glm(Diabetes ~ Glucose + Pregnancies + BMI + DPF,  train_clean, family = "binomial")
summary(log_model_2)
#this gives AIC VALUE 537.33

#so log_model_2 is the better version of the model to go with

#compare the models against each other
anova(log_model,log_model_2,test = "Chisq")
#0.5727>0.05 so confirms that the second model is better

#compute mcfaddens rsquared, close to 0 means no predictive power, over 0.4 shows it fits the data well
library(pscl)
pR2(log_model_2)["McFadden"]

#different way of predicting
log_model_test_2 <- predict(log_model_2, newdata = test_clean, type = "response")
summary(log_model_test_2)
log_model_test_2 <- ifelse(log_model_test_2 > 0.5, 1, 0)
misClasificError <- mean(log_model_test_2 != test_clean$Diabetes)
print(paste('Accuracy', 1-misClasificError))

# Support Vector Machine SVM Model ----------------------------------------------

#this model uses standardised data

#train control method
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#linear model
svm_linear <- train(Diabetes~., data = train_stand, method="svmLinear", trControl=trctrl,tuneLength=10)
confusionMatrix(svm_linear)

#test linear model
svm_predict_linear <- predict(svm_linear, newdata = test_stand)
svm_predict_linear
confusionMatrix(table(svm_predict_linear, test_stand$Diabetes))

#tuning linear model
grid <- expand.grid(C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
svm_linear_Grid <- train(Diabetes ~., data = train_stand, method = "svmLinear",
                         trControl=trctrl,
                         tuneGrid = grid,
                         tuneLength = 10)
svm_linear_Grid
plot(svm_linear_Grid)
#best accuracy at C=0.01

#test tuned linear model
svm_predict_linear_grid <- predict(svm_linear_Grid, newdata = test_stand)
svm_predict_linear
confusionMatrix(table(svm_predict_linear_grid, test_stand$Diabetes))


#radial model
svm_radial <- train(Diabetes~., data = train_stand, method="svmRadial", trControl=trctrl,tuneLength=10)
svm_radial
plot(svm_radial)
confusionMatrix(svm_radial)

#test radial model
svm_predict_radial <- predict(svm_radial, newdata = test_stand)
svm_predict_radial
confusionMatrix(table(svm_predict_radial, test_stand$Diabetes))

#tuning radial model
grid_radial <- expand.grid(sigma = c(0, 0.01, 0.02, 0.025,0.03, 0.04, 0.05,0.06, 0.07, 0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9), 
                           C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
svm_radial_Grid <- train(Diabetes ~., data = train_stand, method = "svmRadial",
                         trControl=trctrl,
                         tuneGrid = grid_radial,
                         tuneLength = 10)
svm_radial_Grid
#sigma is 0.02 and C = 0.25
plot(svm_radial_Grid)

#test tuned radial model
svm_predict_radial_grid <- predict(svm_radial_Grid, newdata = test_stand)
svm_predict_radial_grid
confusionMatrix(table(svm_predict_radial_grid, test_stand$Diabetes))



# Random Forest Model -----------------------------------------------------------

library(randomForest)

#this allows you to look at the correlation between the parameters
table(diabetes_data_clean[,c('Diabetes', 'Age')])

# Training using 'random forest' algorithm
rf_model <- train(Diabetes ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DPF + Age,
               data = train_clean, method = 'rf', trControl = trainControl(method = 'cv', number = 5))
rf_model
confusionMatrix(rf_model)

rf_predict <- predict(rf_model, newdata = test_clean)
rf_predict

confusionMatrix(table(rf_predict, test_clean$Diabetes))


# KNN Model ---------------------------------------------------------------------

#this model uses normalised data

norm_data.n <- norm_data[1:8]
train_norm <- norm_data.n[index_norm,]
test_norm <- norm_data.n[-index_norm,] 
train_norm_labels <- norm_data[index_norm,9]
test_norm_labels <- norm_data[-index_norm,9]

library(class)

NROW(train_norm_labels)
#sqrt of 576 = 24 so will have 3 models
#one with k=23 and one with k=24, k=25

knn_23 <- knn(train = train_norm, test = test_norm, cl=train_norm_labels, k=23)
knn_24 <- knn(train = train_norm, test = test_norm, cl=train_norm_labels, k=24)
knn_25 <- knn(train = train_norm, test = test_norm, cl=train_norm_labels, k=25)

#model evaluation
ACC.23 <- 100*sum(test_norm_labels == knn_23)/NROW(test_norm_labels)
ACC.24 <- 100*sum(test_norm_labels == knn_24)/NROW(test_norm_labels)
ACC.25 <- 100*sum(test_norm_labels == knn_25)/NROW(test_norm_labels)

confusionMatrix(table(knn_23, test_norm_labels))
confusionMatrix(table(knn_24, test_norm_labels))
confusionMatrix(table(knn_25, test_norm_labels))

#can opitmise model by finding optimal value for k
i=1
k.optm=1
for (i in 1:30){
  knn.mod <- knn(train=train_norm, test=test_norm, cl=train_norm_labels, k=i)
  k.optm[i] <- 100 * sum(test_norm_labels == knn.mod)/NROW(test_norm_labels)
  k=i
  cat(k,'=',k.optm[i],'')
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#optimal value for k is 11 which all have same value

#rerun the model for k =11
knn_11 <- knn(train = train_norm, test = test_norm, cl=train_norm_labels, k=11)
ACC.11 <- 100*sum(test_norm_labels == knn_11)/NROW(test_norm_labels)
confusionMatrix(table(knn_11, test_norm_labels))

#now have an accuracy of 78%
