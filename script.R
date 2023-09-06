library(dplyr)
library(readxl)
library(moments)
library(corrplot)
library(kableExtra)
library(tidyr)
library(caret)
library(data.table)
library(mltools)
library(ROSE)
library(leaps)
library(nnet)
library(randomForest)
library(ROCit)
library(e1071)

data <- read.table("card.csv",sep=",",skip=2, header = FALSE)
header <- scan("card.csv",sep=",",nlines=2,what=character())
set.seed(9999)
n = length(data$V1)

## Exploratory Data Analysis

V4 <- data %>% group_by(V4) %>% count
kable(V4, caption = "Distribution of Education (V4)") %>%
  kable_styling(latex_options = "HOLD_position")

data <- data %>% mutate(V4 = ifelse(V4 == 0 | V4 == 5 | V4 == 6, 4, V4))

V5 <- data %>% group_by(V5) %>% count
kable(V5, caption = "Distribution of Marital Status (V5)") %>%
  kable_styling(latex_options = "HOLD_position")

data <- data %>% mutate(V5 = ifelse(V5 == 0, 3, V5))

negativebillamt <- data %>% filter(V13 < 0 | V14 < 0 | V15 < 0 | V16 < 0 | V17 < 0 | V18 < 0) %>% count

kable(negativebillamt, caption = "Frequency of observation with negative bill amount") %>%
  kable_styling(latex_options = "HOLD_position")

## Visualisation of Observations

#Pie chart for default and non-defaulter
defaultFreq <- data %>% count(V25)
defaultFreq$Percentage <- NA
totalSum <- sum(defaultFreq$n)
for (i in 1:length(defaultFreq)-1) {
  defaultFreq$Percentage[i] = defaultFreq$n[i]/totalSum * 100
}

kable(defaultFreq, caption = "Frequency of Credit Card Holder based on Default Status (0 = Non-defaulter, 1 = Defaulter)") %>% kable_styling(latex_options = "HOLD_position")

#credit histogram
options(scipen=999)
h.credit <-hist(data$V2, 
                main="Histogram of Credit given to Customers",
                xlab="Amount of Credit given",
                ylab="No. of Customers",
                col=c("darkorange") ,
                ylim = c(0,8000),
                labels=TRUE)

#creating the data for credit & default
credit <- data %>% select(V2, V25) %>% mutate(V2 = ifelse(V2 >0 & V2 <=50000, 1, ifelse(V2 > 50000 & V2 <= 100000, 2, ifelse(V2 > 100000 & V2 <= 150000, 3, ifelse(V2 > 150000 & V2 <= 200000, 4, ifelse(V2 > 200000 & V2 <= 250000, 5, ifelse(V2 > 250000 & V2 <= 300000, 6, ifelse(V2 > 300000 & V2 <= 350000, 7, ifelse(V2 > 350000 & V2 <= 400000, 8, ifelse(V2 > 400000 & V2 <= 450000, 9, ifelse(V2 > 450000 & V2 <= 500000, 10, ifelse(V2 > 500000 & V2 <= 550000, 11, ifelse(V2 > 550000 & V2 <= 600000, 12, ifelse(V2 > 600000 & V2 <= 650000, 13, ifelse(V2 > 650000 & V2 <= 700000, 14, ifelse(V2 > 700000 & V2 <= 750000, 15, ifelse(V2 > 750000 & V2 <= 800000, 16, ifelse(V2 >800000 & V2 <= 850000, 17, ifelse( V2 > 850000 & V2 <= 900000, 18, ifelse(V2>900000 & V2<=950000, 19, 20)))))))))))))))))))) %>% group_by(V2, V25) %>% count

credit.spread<- credit %>% spread(key=V2,value=n)
barmatrix.credit.spread<-as.matrix(credit.spread[,c(2:9)]) 
barmatrix.credit.spread2<-as.matrix(credit.spread[,c(10:18)]) 
bar_Col1<-c("green","red")

barplot(barmatrix.credit.spread, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Credit and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0,6000),
        beside=TRUE,
        cex.names = 0.5)
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

barplot(barmatrix.credit.spread2, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Credit and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0,1000),
        beside=TRUE,
        cex.names = 0.5)
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

#gender
genderFreq <- data %>% count(V3)
genderFreq$Percentage <- NA
totalSum <- sum(genderFreq$n)
for (i in 1:length(genderFreq)-1) {
  genderFreq$Percentage[i] = genderFreq$n[i]/totalSum * 100
}

kable(genderFreq, caption = "Frequency of Credit Card Holder by Gender (1 = Male, 2 = Female)") %>% kable_styling(latex_options = "hold_position")

#barplot for gender and defaulter 
gender <- data %>% group_by(V3, V25) %>% tally()
gender.spread<- gender %>% spread(key=V3,value=n)
gender.spread <- gender.spread %>% rename("Male" = "1", "Female" = "2")
barmatrix.gender<-as.matrix(gender.spread[,c(2:3)])
bar_Col1<-c("green","red")

barplot(barmatrix.gender, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Gender and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0, 20000),
        beside=TRUE) 
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

#bargraph for education level
education_level <- data %>% group_by(V4) %>% count %>% mutate(V4 = ifelse(V4 == "1", "Graduate School", ifelse( V4 == "2", "University", ifelse(V4 == "3", "High School", "Others"))))
kable(education_level, caption = "Frequency of Credit Card Holders by Educational Level")
educationbp <- barplot(education_level$n, names.arg=education_level$V4, col="skyblue", main="Frequency of Credit Card Holder by Educational Level", xlab="Education Level", ylim =c(0,16000), las=1) 
text(x=educationbp, y=education_level$n, col="black", education_level$n, pos=3)

#barplot graph for education and default status
education <- data %>% group_by(V4, V25) %>% tally()
education.spread<- education %>% spread(key=V4,value=n)
education.spread <- education.spread %>% rename("Graduate School" = "1", "University" = "2", "High School" = "3", "Others" = "4")
barmatrix.education<-as.matrix(education.spread[,c(2:5)]) 
bar_Col1<-c("green","red")

barplot(barmatrix.education, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Education and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0, 12000),
        beside=TRUE)
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

#bar graph for marriage
marriage_status <- data %>% group_by(V5) %>% count %>% mutate(V5 = ifelse(V5 == "1", "Married", ifelse(V5 == "2", "Single", "Others")))
kable(marriage_status, caption = "Frequency of Credit Card Holders by Marriage Status")
marriagebp <- barplot(marriage_status$n, names.arg=marriage_status$V5, col="skyblue", main="Frequency of Credit Card Holder by Marital Status", xlab="Marital Status", las=1, ylim=c(0,20000)) 
text(x=marriagebp, y=marriage_status$n, col="black", marriage_status$n, pos=3)

#barplot for marriage and default status
marital <- data %>% group_by(V5, V25) %>% tally()
marital.spread<- marital %>% spread(key=V5,value=n)
marital.spread <- marital.spread %>% rename("Married" = "1", "Single" = "2", "Others" = "3")
barmatrix.marital<-as.matrix(marital.spread[,c(2:4)]) 
bar_Col1<-c("green","red")

barplot(barmatrix.marital, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Marital and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0, 14000),
        beside=TRUE)
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

#age histogram
h.age<-hist(data$V6, 
            main="Histogram of Credit Card Holder Age",
            xlab="Age of Credit Card Holder",
            ylab="No. of Credit Card Holder",
            col=c("darkorange") ,
            ylim = c(0,8000),
            labels=TRUE)

# extract frequency table from hist()
Age.Group<-cut(data$V6,h.age$breaks)
t.emp<-table(Age.Group)
kable(t.emp, caption = "Frequency distribution by Age")

#barplot for age and default status
age <- data %>% select(V6, V25) %>% mutate(V6 = ifelse(V6 >20 & V6 <=25, 1, ifelse(V6 > 25 & V6 <= 30, 2, ifelse(V6 > 30 & V6 <= 35, 3, ifelse(V6 > 35 & V6 <= 40, 4, ifelse(V6 > 40 & V6 <= 45, 5, ifelse(V6 > 45 & V6 <= 50, 6, ifelse(V6 > 50 & V6 <= 55, 7, ifelse(V6 > 55 & V6 <= 60, 8, ifelse(V6 > 60 & V6 <= 65, 9, ifelse(V6 > 65 & V6 <= 70, 10, ifelse(V6 > 70 & V6 <= 75, 11, 12)))))))))))) %>% group_by(V6, V25) %>% count

age.spread<- age %>% spread(key=V6,value=n)
age.spread <- age.spread %>% rename("Age 21 to 25" = "1", "Age 26 to 30" = "2", "Age 31 to 35" = "3", "Age 36 to 40" = "4", "Age 41 to 45" = "5", "Age 46 to 50" = "6", "Age 51 to 55" = "7", "Age 56 to 60" = "8", "Age 61 to 65" = "9", "Age 65 to 70" = "10", "Age 71 to 75" = "11", "Age 75 to 80" = "12")
barmatrix.age.spread<-as.matrix(age.spread[,c(2:7)]) 
bar_Col1<-c("green","red")

barplot(barmatrix.age.spread, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Age and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0, 7000),
        beside=TRUE,
        cex.names = 0.5)
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

barmatrix.age.spread2 <-as.matrix(age.spread[,c(8:13)]) 
barplot(barmatrix.age.spread2, 
        col=bar_Col1,  
        main="Frequncy of Credit Card holder by Age and Default Status",
        ylab="No. of Credit Card holder", 
        ylim = c(0, 1200),
        beside=TRUE,
        cex.names = 0.5)
legend("topright", cex=0.6, fill=bar_Col1, legend=c("Non-Defaulter","Defaulter"))

V7 <- data %>% group_by(V7, V25) %>% count %>% spread(key = V25, value = n)
V7$Proportion <- NA
for (i in 1:nrow(V7)) {
  V7$Proportion[i] = V7$`1`[i]/(V7$`1`[i] + V7$`0`[i])
}

V8 <- data %>% group_by(V8, V25) %>% count %>% spread(key = V25, value = n)
V8$Proportion <- NA
for (i in 1:nrow(V8)) {
  V8$Proportion[i] = V8$`1`[i]/(V8$`1`[i] + V8$`0`[i])
}

V9 <- data %>% group_by(V9, V25) %>% count %>% spread(key = V25, value = n)
V9$Proportion <- NA
for (i in 1:nrow(V9)) {
  V9$Proportion[i] = V9$`1`[i]/(V9$`1`[i] + V9$`0`[i])
}

V10 <- data %>% group_by(V10, V25) %>% count %>% spread(key = V25, value = n)
V10$Proportion <- NA
for (i in 1:nrow(V10)) {
  V10$Proportion[i] = V10$`1`[i]/(V10$`1`[i] + V10$`0`[i])
}

V11 <- data %>% group_by(V11, V25) %>% count %>% spread(key = V25, value = n)
V11$Proportion <- NA
for (i in 1:nrow(V11)) {
  V11$Proportion[i] = V11$`1`[i]/(V11$`1`[i] + V11$`0`[i])
}

V12 <- data %>% group_by(V12, V25) %>% count %>% spread(key = V25, value = n)
V12$Proportion <- NA
for (i in 1:nrow(V12)) {
  V12$Proportion[i] = V12$`1`[i]/(V12$`1`[i] + V12$`0`[i])
}

#V11 & V12 doesnt have "1" in their data so need to reformat
V11_data <- NA
V11_data[1:3] <- V11$Proportion[c(1:3)]
V11_data[4] <- 0 
V11_data[5:11] <- V11$Proportion[c(4:10)]

V12_data <- NA
V12_data[1:3] <- V12$Proportion[c(1:3)]
V12_data[4] <- 0 
V12_data[5:11] <- V12$Proportion[c(4:10)]


combined_table <- cbind(V7$V7, V7$Proportion,V8$Proportion,V9$Proportion,V10$Proportion, V11_data, V12_data) %>% replace_na(0)
colnames(combined_table) <- c("Data observed", "V7", "V8", "V9", "V10" , "V11", "V12")

kable(combined_table, caption = "Proportion of Deafulter in PAY0, PAY2 to PAY6 (V7 to V12)") %>%
  kable_styling(latex_options = "HOLD_position")

## Data Pre-processing

index <- 1:nrow(data)
testindex <- sample(index, trunc(n)/4)
test.data <- data[testindex,]
train.data <- data[-testindex,]

#scaling train data set
raw_train_data <- train.data
scaled.train.data <- train.data
df1 <- preProcess(scaled.train.data, method=c("range")) 
scaled.train.data <- predict(df1, as.data.frame(scaled.train.data))

#scaling test data set
raw_test_data <- test.data
scaled.test.data <- test.data
df2 <- preProcess(scaled.test.data, method=c("range")) 
scaled.test.data <- predict(df2, as.data.frame(scaled.test.data))

## Feature Selection

# correlation matrix 
contd <- cor(scaled.train.data[c(2,3,13,14,15,16,17,18,19,20,21,22,23,24,25)])
corrplot(contd, method = "number",addCoef.col = 1, number.cex = 0.4, tl.cex = 0.4)

dist_before <- table(scaled.train.data$V25)

processed_balanced_train <- ROSE(V25 ~ ., data = scaled.train.data,
                                 seed = 9999)$data

dist_after <- table(processed_balanced_train$V25)

kable(dist_before, caption = "Distribution of V25 in train set before processing the data") %>%
  kable_styling(latex_options = "HOLD_position")

kable(dist_after, caption = "Distribution of V25 after train set processing the data") %>%
  kable_styling(latex_options = "HOLD_position")

# wrapper method (forward and backward stepwise)
outbackward <- regsubsets(V25 ~ 
                            V2 + V3 + V4 + V5 +
                            V6 + V7 + V8 + V9 + V10 +
                            V11 + V12 + V13 + V19 + V20 +
                            V21 + V22 + V23 + V24, data = processed_balanced_train, method = "backward")
summary(outbackward)
plot(outbackward,scale="r2", main = "Backward stepwise feature selection")

# wrapper method (forward and backward stepwise)

outforward <- regsubsets(V25 ~ 
                           V2 + V3 + V4 + V5 +
                           V6 + V7 + V8 + V9 + V10 +
                           V11 + V12 + V13 + V19 + V20 +
                           V21 + V22 + V23 + V24, data = processed_balanced_train, method = "forward")
summary(outforward)
plot(outforward,scale="r2", main = "Forward stepwise feature selection")

# random forest

model_rf <- randomForest(V25 ~ 
                           V2 + V3 + V4 + V5 +
                           V6 + V7 + V8 + V9 + V10 +
                           V11 + V12 + V13 + V19 + V20 +
                           V21 + V22 + V23 + V24, data = processed_balanced_train, importance=TRUE)

model_rf

important_vars <- importance(model_rf)
important_vars %>%
  as.data.frame() %>%
  arrange(desc(`%IncMSE`))

kable(important_vars %>%
        as.data.frame() %>%
        arrange(desc(`%IncMSE`)), caption = "Random Forest Feature Seletion") %>% kable_styling(latex_options = "HOLD_position")

## Model Selection

#Linear Regression

# USING VARIABLES SELECTED FROM BACKWARD AND FORWARD FEATURE SELECTION

set.seed(9999)

bf.logreg <- glm(V25 ~ V2 + V6 + V7 + V8 + V9 + V10 + V13 + V19, data=processed_balanced_train, family=binomial)

summary(bf.logreg)

bf.logreg.pred <- predict(bf.logreg, scaled.test.data[,-25], type="response")

bf.logreg.test_result <- rocit(bf.logreg.pred, scaled.test.data$V25)
bf.logreg.auc <- bf.logreg.test_result$AUC
bf.logreg.auc #0.7195834

bf.logreg.ksplot <- ksplot(bf.logreg.test_result)
bf.logreg.ksplot$`KS Cutoff`#0.5518764


logreg.bf.results <- ifelse(bf.logreg.pred > bf.logreg.ksplot$`KS Cutoff`, 1, 0)
bf.logreg.accuracy <- mean(logreg.bf.results == scaled.test.data$V25)
bf.logreg.accuracy #0.7698667

bf.glm.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=logreg.bf.results), 2, rev)), 2, rev)

bf.glm.confusion_matrix

bf.glm.sensitivity <- bf.glm.confusion_matrix[1] / (bf.glm.confusion_matrix[1] + bf.glm.confusion_matrix[3])
bf.glm.specificity <- bf.glm.confusion_matrix[4] / (bf.glm.confusion_matrix[4] + bf.glm.confusion_matrix[2])

bf.glm.balanced_accuracy <- (bf.glm.sensitivity + bf.glm.specificity) / 2

bf.glm.balanced_accuracy #0.6904977

bf.glm.precision <- bf.glm.confusion_matrix[1] / (bf.glm.confusion_matrix[1] + bf.glm.confusion_matrix[2])
bf.glm.recall <- bf.glm.confusion_matrix[1] / (bf.glm.confusion_matrix[1] + bf.glm.confusion_matrix[3])
bf.glm.f1 <- 2 * (bf.glm.precision * bf.glm.recall) / (bf.glm.precision + bf.glm.recall)
bf.glm.average_class_accuracy <- ((bf.glm.confusion_matrix[1] / (bf.glm.confusion_matrix[1] + bf.glm.confusion_matrix[3])) + (bf.glm.confusion_matrix[2] / (bf.glm.confusion_matrix[2] + bf.glm.confusion_matrix[4]))) / 2

bf.glm.precision
bf.glm.recall
bf.glm.f1
bf.glm.average_class_accuracy

kable(bf.glm.confusion_matrix, caption = "Confusion Matrix for logistic regression model with backward-forward feature selection", col.names = c("1 (Predicted)", "0 (Predicted)")) %>% kable_styling(latex_options = "HOLD_position")

# USING VARIABLES SELECTED FROM RANDOM FOREST FEATURE SELECTION

set.seed(9999)

rf.logreg <- glm(V25 ~ V20 + V19 + V7 + V8 + V9 + V10 + V11 + V12, data=processed_balanced_train, family=binomial)

summary(rf.logreg)

rf.logreg

rf.logreg.pred <- predict(rf.logreg, scaled.test.data[,-25], type="response")

rf.logreg.test_result <- rocit(rf.logreg.pred, scaled.test.data$V25)
rf.logreg.auc <- rf.logreg.test_result$AUC
rf.logreg.auc #AUC = 0.7139351

rf.logreg.ksplot <- ksplot(rf.logreg.test_result)
rf.logreg.ksplot$`KS Cutoff` #0.501456

rf.logreg.results <- ifelse(rf.logreg.pred > rf.logreg.ksplot$`KS Cutoff`, 1, 0)
rf.logreg.accuracy <- mean(rf.logreg.results == scaled.test.data$V25)
rf.logreg.accuracy #0.7968


rf.glm.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=rf.logreg.results), 2, rev)), 2, rev)
rf.glm.sensitivity <- rf.glm.confusion_matrix[1] / (rf.glm.confusion_matrix[1] + rf.glm.confusion_matrix[3])
rf.glm.specificity <- rf.glm.confusion_matrix[4] / (rf.glm.confusion_matrix[4] + rf.glm.confusion_matrix[2])

rf.glm.balanced_accuracy <- (rf.glm.sensitivity + rf.glm.specificity ) / 2

rf.glm.balanced_accuracy #0.6987934

rf.glm.precision <- rf.glm.confusion_matrix[1] / (rf.glm.confusion_matrix[1] + rf.glm.confusion_matrix[2])
rf.glm.recall <- rf.glm.confusion_matrix[1] / (rf.glm.confusion_matrix[1] + rf.glm.confusion_matrix[3])
rf.glm.f1 <- 2 * (rf.glm.precision * rf.glm.recall) / (rf.glm.precision + rf.glm.recall)
rf.glm.average_class_accuracy <- ((rf.glm.confusion_matrix[1] / (rf.glm.confusion_matrix[1] + rf.glm.confusion_matrix[3])) + (rf.glm.confusion_matrix[2] / (rf.glm.confusion_matrix[2] + rf.glm.confusion_matrix[4]))) / 2

rf.glm.precision
rf.glm.recall
rf.glm.f1
rf.glm.average_class_accuracy 

kable(rf.glm.confusion_matrix, caption = "Confusion Matrix for logistic regression model with random forest feature selection" , col.names = c("1 (Predicted)", "0 (Predicted)")) %>% kable_styling(latex_options = "HOLD_position")

fmla <- as.formula(as.factor(V25) ~ V2 + V6 + V7 + V8 + V9 + V10 + V13 + V19)

# Support Vector Machine

set.seed(9999)

#general SVM model

svm.model <- svm(fmla, data=processed_balanced_train, type="C-classification", kernel="linear")

svm.model

svm.model.pred <- predict(svm.model, scaled.test.data[,-25])

svm.model.test_result <- rocit(as.numeric(svm.model.pred), scaled.test.data$V25)
svm.model.auc <- svm.model.test_result$AUC
svm.model.auc 

accuracy_svm.model <- mean(svm.model.pred == scaled.test.data$V25)
accuracy_svm.model 

svm.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=svm.model.pred), 2, rev)), 2, rev)

svm.sensitivity <- svm.confusion_matrix[1] / (svm.confusion_matrix[1] + svm.confusion_matrix[3])
svm.specificity <- svm.confusion_matrix[4] / (svm.confusion_matrix[4] + svm.confusion_matrix[2])

svm.balanced_accuracy <- (svm.sensitivity + svm.specificity) / 2

svm.balanced_accuracy 

svm.precision <- svm.confusion_matrix[1] / (svm.confusion_matrix[1] + svm.confusion_matrix[2])
svm.recall <- svm.confusion_matrix[1] / (svm.confusion_matrix[1] + svm.confusion_matrix[3])
svm.f1 <- 2 * (svm.precision * svm.recall) / (svm.precision + svm.recall)
svm.average_class_accuracy <- ((svm.confusion_matrix[1] / (svm.confusion_matrix[1] + svm.confusion_matrix[3])) + (svm.confusion_matrix[2] / (svm.confusion_matrix[2] + svm.confusion_matrix[4]))) / 2

svm.precision
svm.recall
svm.f1
svm.average_class_accuracy 

kable(svm.confusion_matrix, caption = "Confusion Matrix for C-Classification SVM model", col.names = c("1 (Predicted)", "0 (Predicted")) %>% kable_styling(latex_options = "HOLD_position")

#SVM with cross-validation

svm.cross <- svm(fmla, data=processed_balanced_train, type="C-classification", kernel="linear", cross=10, cost=1)

svm.cross

svm.cross.pred <- predict(svm.cross, scaled.test.data[,-25])

svm.cross.test_result <- rocit(as.numeric(svm.cross.pred), scaled.test.data$V25)
svm.cross.auc <- svm.cross.test_result$AUC
svm.cross.auc 

svm.cross.accuracy <- mean(svm.cross.pred== scaled.test.data$V25)
svm.cross.accuracy 

svm.cross.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=svm.cross.pred), 2, rev)), 2, rev)

svm.cross.sensitivity <- svm.cross.confusion_matrix[1] / (svm.cross.confusion_matrix[1] + svm.cross.confusion_matrix[3])
svm.cross.specificity <- svm.cross.confusion_matrix[4] / (svm.cross.confusion_matrix[4] + svm.cross.confusion_matrix[2])

svm.cross.balanced_accuracy <- (svm.cross.sensitivity + svm.cross.specificity) / 2

svm.cross.balanced_accuracy 

svm.cross.precision <- svm.cross.confusion_matrix[1] / (svm.cross.confusion_matrix[1] + svm.cross.confusion_matrix[2])
svm.cross.recall <- svm.cross.confusion_matrix[1] / (svm.cross.confusion_matrix[1] + svm.cross.confusion_matrix[3])
svm.cross.f1 <- 2 * (svm.cross.precision * svm.cross.recall) / (svm.cross.precision + svm.cross.recall)
svm.cross.average_class_accuracy <- ((svm.cross.confusion_matrix[1] / (svm.cross.confusion_matrix[1] + svm.cross.confusion_matrix[3])) + (svm.cross.confusion_matrix[2] / (svm.cross.confusion_matrix[2] + svm.cross.confusion_matrix[4]))) / 2

svm.cross.precision
svm.cross.recall
svm.cross.f1
svm.cross.average_class_accuracy 

kable(svm.cross.confusion_matrix, caption = "Confusion Matrix for C-Classification with 10-fold cross validation SVM model", col.names = c("1 (Predicted)", "0 (Predicted)")) %>% kable_styling(latex_options = "HOLD_position")

#SVM with cross-validation and weights that give a larger penalty for wrongly classifying customers who default as customers who do not default

class_counts <- table(processed_balanced_train$V25)
class_weights <- 1 / prop.table(class_counts)

svm.weighted <- svm(fmla, data=processed_balanced_train, type="C-classification", kernel="linear", cross=10, cost=1, class.weights=class_weights)

svm.weighted

svm.weighted.pred <- predict(svm.weighted, scaled.test.data[,-25])

svm.weighted.test_result <- rocit(as.numeric(svm.weighted.pred), scaled.test.data$V25)
svm.weighted.auc <- svm.weighted.test_result$AUC
svm.weighted.auc 

svm.weighted.accuracy<- mean(svm.weighted.pred == scaled.test.data$V25)

svm.weighted.accuracy

svm.weighted.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=svm.weighted.pred), 2, rev)), 2, rev)

svm.weighted.sensitivity <- svm.weighted.confusion_matrix[1] / (svm.weighted.confusion_matrix[1] + svm.weighted.confusion_matrix[3])
svm.weighted.specificity <- svm.weighted.confusion_matrix[4] / (svm.weighted.confusion_matrix[4] + svm.weighted.confusion_matrix[2])

svm.weighted.balanced_accuracy <- (svm.weighted.sensitivity + svm.weighted.specificity) / 2

svm.weighted.balanced_accuracy 

svm.weighted.precision <- svm.weighted.confusion_matrix[1] / (svm.weighted.confusion_matrix[1] + svm.weighted.confusion_matrix[2])
svm.weighted.recall <- svm.weighted.confusion_matrix[1] / (svm.weighted.confusion_matrix[1] + svm.weighted.confusion_matrix[3])
svm.weighted.f1 <- 2 * (svm.weighted.precision * svm.weighted.recall) / (svm.weighted.precision + svm.weighted.recall)
svm.weighted.average_class_accuracy <- ((svm.weighted.confusion_matrix[1] / (svm.weighted.confusion_matrix[1] + svm.weighted.confusion_matrix[3])) + (svm.weighted.confusion_matrix[2] / (svm.weighted.confusion_matrix[2] + svm.weighted.confusion_matrix[4]))) / 2

svm.weighted.precision
svm.weighted.recall
svm.weighted.f1
svm.weighted.average_class_accuracy 

kable(svm.weighted.confusion_matrix, caption = "Confusion Matrix for C-Classification with 10-fold cross validation and weighted SVM model", col.names = c("1 (Predicted)", "0 (Predicted)")) %>% kable_styling(latex_options = "HOLD_position")

#Neural Network Model

set.seed(9999)

nn <- nnet(fmla,data=processed_balanced_train,maxit=1000,size=20, decay=0.2)

nn

nn.pred <- predict(nn, newdata = scaled.test.data[,-25], type = c("class"))

nn.test_result <- rocit(as.numeric(nn.pred), scaled.test.data$V25)
nn.auc <- nn.test_result$AUC 
nn.auc 

nn.accuracy <- mean(scaled.test.data$V25 == nn.pred) 
nn.accuracy 

nn.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=nn.pred), 2, rev)), 2, rev)
nn.sensitivity <- nn.confusion_matrix[1] / (nn.confusion_matrix[1] + nn.confusion_matrix[3])
nn.specificity <- nn.confusion_matrix[4] / (nn.confusion_matrix[4] + nn.confusion_matrix[2])

nn.balanced_accuracy <- (nn.sensitivity  + nn.specificity) / 2

nn.balanced_accuracy 

nn.precision <- nn.confusion_matrix[1] / (nn.confusion_matrix[1] + nn.confusion_matrix[2])
nn.recall <- nn.confusion_matrix[1] / (nn.confusion_matrix[1] + nn.confusion_matrix[3])
nn.f1 <- 2 * (nn.precision * nn.recall) / (nn.precision + nn.recall)
nn.average_class_accuracy <- ((nn.confusion_matrix[1] / (nn.confusion_matrix[1] + nn.confusion_matrix[3])) + (nn.confusion_matrix[2] / (nn.confusion_matrix[2] + nn.confusion_matrix[4]))) / 2

nn.precision
nn.recall
nn.f1
nn.average_class_accuracy

kable(nn.confusion_matrix, caption = "Confusion Matrix for neural network model", col.names = c("1 (Predicted)", "0 (Predicted)")) %>% kable_styling(latex_options = "HOLD_position")

# Random Forest Model

set.seed(9999)

rf.model <- randomForest(fmla, data = processed_balanced_train)

rf.pred <- predict(rf.model, scaled.test.data)

rf.result <- rocit(as.numeric(rf.pred), scaled.test.data$V25)
rf.auc <- rf.result$AUC
rf.auc 

rf.accuracy <- mean(scaled.test.data$V25 == rf.pred)
rf.accuracy

rf.confusion_matrix <- apply(t(apply(table(actual=scaled.test.data$V25, pred=rf.pred), 2, rev)), 2, rev)
rf.sensitivity <- rf.confusion_matrix[4] / (rf.confusion_matrix[4] + rf.confusion_matrix[2])
rf.specificity <- rf.confusion_matrix[1] / (rf.confusion_matrix[1] + rf.confusion_matrix[3])

rf.balanced_accuracy <- (rf.sensitivity  + rf.specificity) / 2

rf.balanced_accuracy 

rf.precision <- rf.confusion_matrix[1] / (rf.confusion_matrix[1] + rf.confusion_matrix[2])
rf.recall <- rf.confusion_matrix[1] / (rf.confusion_matrix[1] + rf.confusion_matrix[3])
rf.f1 <- 2 * (rf.precision * rf.recall) / (rf.precision + rf.recall)
rf.average_class_accuracy <- ((rf.confusion_matrix[1] / (rf.confusion_matrix[1] + rf.confusion_matrix[3])) + (rf.confusion_matrix[2] / (rf.confusion_matrix[2] + rf.confusion_matrix[4]))) / 2

rf.precision
rf.recall
rf.f1
rf.average_class_accuracy 

kable(rf.confusion_matrix, caption = "Confusion Matrix for random forest model", col.names = c("1 (Predicted)", "0 (Predicted)")) %>% kable_styling(latex_options = "HOLD_position")

# Model Evaluation

model_results <- data.frame(Model = c("Logistic Regression", "SVM", "SVM w/ K-fold", "SVM w/ weighted K-fold", "Neural Network", "Random Forest"),
                            Acc = c(rf.logreg.accuracy, svm.weighted.accuracy, svm.cross.accuracy, svm.weighted.accuracy, nn.accuracy, rf.accuracy),
                            Balance_Acc= c(rf.glm.balanced_accuracy, svm.weighted.balanced_accuracy, svm.cross.balanced_accuracy, svm.weighted.balanced_accuracy, nn.balanced_accuracy, rf.balanced_accuracy),
                            Avg_Class_Acc = c(rf.glm.average_class_accuracy, svm.average_class_accuracy, svm.cross.average_class_accuracy, svm.weighted.average_class_accuracy, nn.average_class_accuracy, rf.average_class_accuracy),
                            Recall = c(rf.glm.recall, svm.recall, svm.cross.recall, svm.weighted.recall, nn.recall, rf.recall),
                            Precision = c(rf.glm.precision, svm.precision, svm.cross.precision, svm.weighted.precision, nn.precision, rf.precision),
                            F1_Score = c(rf.glm.f1, svm.f1, svm.cross.f1, svm.weighted.f1, nn.f1, rf.f1),
                            AUC = c(rf.logreg.auc, svm.model.auc, svm.cross.auc, svm.weighted.auc, nn.auc, rf.auc))

kable(model_results, caption = "Model Performance Metrics") %>% kable_styling(latex_options = "HOLD_position", full_width = T, font_size = 10)