Attrition <- read.csv("E:/Mnadar/fwddocumentsfornewbatchdsp16/Project/r/Attrition (1).csv")
View(Attrition)

# R Packagesrequired
library(data.table)
#install.packages("entropy")
library(entropy)
library(ggplot2)
library(caTools)
library(ROCR)
library(rpart)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)
#install.packages("pROC")
library(pROC)


# Checking the dimension of data

dim(Attrition)
# Total number of Rows 1470 & Total number of column 35

# Now check the null values in dataset

sum(is.na(Attrition))
# There is 0 null value availabe in dataset.


#Observed the dataset & find that the attributes EmployeeNumber, Over18, EmployeeCount and StandardHours all carry the same value for each observation.therefore drop these columns.

Attrition$EmployeeNumber=Attrition$Over18=Attrition$EmployeeCount=Attrition$StandardHours = NULL


# Now  we check the Types of features in dataset

str(Attrition)
summary(Attrition)
# Here we observed that there few Categorical data which needs to be factories.

categorical <- c(2,3,5,7:10,12:16,19,21,22,23,26)
Attrition[,names] <- lapply(Attrition[,names] , factor)
as.factor(as.numeric(Attrition$Attrition)) -> Attrition$Attrition


# Now check summary of data to know about the distribution of data

summary(Attrition)



### % attrition across categorical variables


freq_tbl <-  apply(Attrition[,categorical],2, function(x) table(Attrition$Attrition,x))
freq_tbl <- lapply(freq_tbl,function(x) as.data.frame.matrix(x))

perc_attrition_plot <- list()
i =0
for(name in names(freq_tbl)[-1]){
  i <- i +1
  var_data <- data.frame(apply(freq_tbl[name][[1]],2, function(x) x[2]/sum(x)))
  colnames(var_data) <- name
  my_plot <- ggplot(data=var_data, aes(x=row.names(var_data), y=var_data[,name])) +  geom_bar(stat="identity",fill='red') +
    ylim(0.0,1.0) + ylab("%attrition") + xlab(name) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(my_plot)
  remove(my_plot)
}


## From the above plots we see that percentage churn is more for employees who are single, do over time, travel frequently and have less worklife balance satisfaction.


plottable1=table(Attrition$Attrition,Attrition$JobLevel)

plottable2=table(Attrition$Attrition,Attrition$Education)

plottable3=table(Attrition$Attrition,Attrition$EnvironmentSatisfaction)

plottable4=table(Attrition$Attrition,Attrition$JobInvolvement)

plottable5=table(Attrition$Attrition,Attrition$PercentSalaryHike)

plottable6=table(Attrition$Attrition,Attrition$PerformanceRating)

plottable7=table(Attrition$Attrition,Attrition$StockOptionLevel)

plottable8=table(Attrition$Attrition,Attrition$YearsAtCompany)

plottable9=table(Attrition$Attrition,Attrition$YearsInCurrentRole)

plottable10 = table(Attrition$Attrition, Attrition$Department)

plottable11 = table(Attrition$Attrition, Attrition$MaritalStatus)

plottable12 = table(Attrition$Attrition, Attrition$Age)



barplot(plottable1, main="Employees left vs Job Level", xlab="JobLevel",col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable2, main="Employees left vs Education", xlab="Education",col=c("Red","Yellow"),legend=rownames(plottable2),beside = TRUE)

barplot(plottable3, main="Employees left vs Environment Satisfaction", xlab="JobLevel", col=c("Red","Yellow"),beside = TRUE)

barplot(plottable4, main="Employees left vs Job Involvement", xlab="Job Involvement", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable5, main="Employees left vs salary hike", xlab="salary hike in %", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable6, main="Employees left vs Performance Rating", xlab="PerformanceRating",col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable7, main="Employees left vs stock option level", xlab="Stock Option Level", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable8, main="Employees left vs Num of Years at Company", xlab="Num of Years", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable9, main="Employees left vs Years in current Role", xlab="Years In Current Role ", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable10, main="Employees left vs Department", xlab="Department ", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable11, main="Employees left vs MaritalStatus", xlab="MaritalStatus ", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)

barplot(plottable12, main="Employees left vs Age", xlab="Age", col=c("Red","Yellow"),legend=rownames(plottable1),beside = TRUE)




#Dummy variables

dummy = dataset

dummy$Attrition=as.numeric(dummy$Attrition)

dummy$BusinessTravel=as.numeric(dummy$BusinessTravel)

dummy$Department=as.numeric(dummy$Department)

dummy$EducationField=as.numeric(dummy$EducationField)

dummy$Gender=as.numeric(dummy$Gender)

dummy$JobRole=as.numeric(dummy$JobRole)

dummy$MaritalStatus=as.numeric(dummy$MaritalStatus)

dummy$OverTime=as.numeric(dummy$OverTime)



corTable=cor(dummy)

corr=melt(corTable)

corTable

corr


#A correlation plot

corrplot( cor(as.matrix(dummy), method = "pearson", use = "complete.obs") ,is.corr = FALSE, type = "lower", order = "hclust", tl.col = "black", tl.srt = 360)

#After carefully examining the correlation plot and table, find that there are a lot of correlated features.

# We run the Boruta to examine which features are to be dropped from the pair of each correlated features.

library(Boruta)

A1 <- Boruta(Attrition~., data = Attrition)

A1$finalDecision

# After using Boruta we came to know that there are total 16 significant features(Age,JobRole,JobSatisfaction,NumCompaniesWorked etc), so we'll take these features only for future.

# Now we drop all other features & keep only those who are significant for Attrition

Atr <- Attrition[, c(1,2,9,12:17,19,20,24,25,27,28,29,31)]
summary(Atr)


# Now we spliting data into training and testing using Stratified sampling

set.seed(3031)
Sample1 <- sample(2,nrow(Atr),replace = TRUE, prob = c(.7,.3))

Atr_Train <- Atr[Sample1==1,]
Atr_Test <- Atr[Sample1==2,]

table(Atr_Train$Attrition)


#Implementing Machine Learning Algorithms

Atr_Log =glm(Attrition~., Atr_Train,family = binomial)
summary(Atr_Log)

# Predicting the Test data

Atr_pre <- predict(Atr_Log, Atr_Test, type="response")


Atr_act <- data.frame(Atr_pre,Atr_Test$Attrition)

colnames(Atr_act) <- c('pred', 'actual')

# THRESHOLDING : Outcome of a logistic regression model is a probability. Often, we want to make an actual prediction. We can convert the probabilities to predictions using what's called a threshold value, t. If the probability of attrition is greater than this threshold value, t, we predict that employee will churn. But if the probability of attrition is less than the threshold value, t, then we predict that employee will not churn.

# If we pick a large threshold value t,then we will predict actual churn rarely, since the probability of attrition has to be really large to be greater than the threshold. This means that we will make more errors where we say that employee will not churn , but it's actually churning case.

# On the other hand, if the threshold value, t, is small,we predict churn (Yes) frequently, and we predict non churn (No) rarely. This means that we will make more errors where we say that employee will not churn, but it's actually that employee will churn.

#A model with a higher threshold will have a lower sensitivity and a higher specificity. A model with a lower threshold will have a higher sensitivity and a lower specificity.


# Threshold - 0.1

print("Confusion matrix for threshold 0.1")

thershold= 0.1

confusion_mat <- table(Atr_Test$Attrition, Atr_pre > thershold)
confusion_mat
#    FALSE TRUE
# 1   219   117
# 2    14   61

# Accuracy

acc <- (sum(diag(confusion_mat))/sum(confusion_mat)) * 100
acc   # 68.126


# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp <- confusion_mat[4]
tp_plus_fn <- confusion_mat[4] + confusion_mat[2]

sensitivity <- tp/tp_plus_fn
print(c("sensitivity",sensitivity))  # 0.8133

# specificity tnr--> specificity = tn/(tn+FP)
tn <- confusion_mat[1]
tn_plus_fp <- confusion_mat[1] + confusion_mat[3]

specificity <- tn/tn_plus_fp
print(c("specificity",specificity))   # 0.6517



# Threshold - 0.3

print("Confusion matrix for threshold 0.3")

thershold= 0.3

confusion_mat1 <- table(Atr_Test$Attrition, Atr_pre > thershold)
confusion_mat1
#    FALSE TRUE
# 1   304   32
# 2    32   43

acc1 <- (sum(diag(confusion_mat1))/sum(confusion_mat1)) * 100
acc1   # 84.428

# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp1 <- confusion_mat1[4]
tp_plus_fn1 <- confusion_mat1[4] + confusion_mat1[2]

sensitivity1 <- tp1/tp_plus_fn1
sensitivity1  # 0.5733

# specificity tnr--> specificity = tn/(tn+FP)
tn1 <- confusion_mat1[1]
tn_plus_fp1 <- confusion_mat1[1] + confusion_mat1[3]

specificity1 <- tn1/tn_plus_fp1
specificity1   # 0.9047




# Threshold - 0.5

print("Confusion matrix for threshold 0.5")

thershold= 0.5

confusion_mat2 <- table(Atr_Test$Attrition, Atr_pre > thershold)
confusion_mat2
#    FALSE TRUE
# 1   322   14
# 2    43   32

acc2 <- (sum(diag(confusion_mat2))/sum(confusion_mat2)) * 100
acc2   # 86.131

# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp2 <- confusion_mat2[4]
tp_plus_fn2 <- confusion_mat2[4] + confusion_mat2[2]

sensitivity2 <- tp2/tp_plus_fn2
sensitivity2  # 0.4266

# specificity tnr--> specificity = tn/(tn+FP)
tn2 <- confusion_mat2[1]
tn_plus_fp2 <- confusion_mat2[1] + confusion_mat2[3]

specificity2 <- tn2/tn_plus_fp2
specificity2   # 0.9583



## Plotting Receiver operator characteristics curve to decide better on threshold
rocr_pred_logistic_best_treshold = prediction(Atr_pre ,Atr_Test$Attrition)
rocr_perf_logistic_best_treshold = performance(rocr_pred_logistic_best_treshold,'tpr','fpr')
plot(rocr_perf_logistic_best_treshold,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))


## From this plot we can say that 0.3 is the best threshold. Now we will evaluate model performance witn this threshold.


thershold_best_log = 0.3

conf_mat_log <- table(Atr_Test$Attrition ,Atr_pre > thershold_best_log)
conf_mat_log

#Accuracy
ac <- (sum(diag(conf_mat_log))/sum(conf_mat_log)) * 100
ac   # 84.42822

# sensitivity tpr --> sensitivity = tp/(tp+FN)
tpa <- conf_mat_log[4]
tp_plus_fna <- conf_mat_log[4] + conf_mat_log[2]

sensitivitya <- tpa/tp_plus_fna
sensitivitya  #0.5679012

# specificity tnr--> specificity = tn/(tn+FP)
tna <- conf_mat_log[1]
tn_plus_fpa <- conf_mat_log[1] + conf_mat_log[3]

specificitya <- tna/tn_plus_fpa
specificitya   # 0.9121212

# 'Confusion matrix for threshold 

#       FALSE TRUE
#  1    304   32
#  2     32   43

#'Model Performance'
#[1] "Accuracy"          "84.428"
#[1] "sensitivity"       "0.5733"
#[1] "specificity"       "0.9047"


## Now we try to reduce the complexity of the model by selecting important features for the model based on p-value. Lower the value impotant the feature is.

Atr_Log1=glm(Attrition~ Age+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+OverTime+WorkLifeBalance+NumCompaniesWorked+StockOptionLevel+YearsWithCurrManager+JobRole+JobLevel , Atr_Train,family = binomial)

Atr_pre1 <- predict(Atr_Log1, Atr_Test, type="response")

Atr_act1 <- data.frame(Atr_pre1,Atr_Test$Attrition)

colnames(Atr_act1) <- c('pred', 'actual')

actual1 <- mutate(Atr_act1,pred = ifelse(pred<=.3,1,2))

taba <- table(actual1$pred, actual1$actual)
taba

acc4 <- (sum(diag(taba))/sum(taba)) * 100
acc4   # 84.42822

# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp4 <- taba[4]
tp_plus_fn4 <- taba[4] + taba[2]

sensitivity4 <- tp4/tp_plus_fn4
sensitivity4  #0.5679012

# specificity tnr--> specificity = tn/(tn+FP)
tn4 <- taba[1]
tn_plus_fp4 <- taba[1] + taba[3]

specificity4 <- tn4/tn_plus_fp4
specificity4   # 0.9121212

# 'Confusion matrix for threshold 

#        1     2
#  1    301   29
#  2     35   46

'Model Performance'
#[1] "Accuracy"          "84.428"
#[1] "sensitivity"       "0.5679012"
#[1] "specificity"       "0.9121212"





##### Decision Tree

Atr_Dt <- rpart(Attrition ~ ., method="class", data= Atr_Train,)

Dt_pre = as.data.frame.matrix(predict(Atr_Dt,newdata = Atr_Test,type = "prob"))
Dt_pre <- Dt_pre$`2`

rocr_Dt_pre = prediction(Dt_pre ,Atr_Test$Attrition)
rocr_perf_Dt = performance(rocr_Dt_pre,'tpr','fpr')
plot(rocr_perf_Dt,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))

# We see that threshold 0.3 is the best threshold here.

Atr_act2 <- data.frame(Dt_pre,Atr_Test$Attrition)

colnames(Atr_act2) <- c('pred', 'actual')

actual2 <- mutate(Atr_act2,pred = ifelse(pred<=.3,1,2))

taba1 <- table(actual2$pred, actual2$actual)
taba1

acc5 <- (sum(diag(taba1))/sum(taba1)) *100
acc5  #85.15815


# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp5 <- taba1[4]
tp_plus_fn5 <- taba1[4] + taba1[2]

sensitivity5 <- tp5/tp_plus_fn5
sensitivity5  #0.7058824

# specificity tnr--> specificity = tn/(tn+FP)
tn5 <- taba1[1]
tn_plus_fp5 <- taba1[1] + taba1[3]

specificity5 <- tn5/tn_plus_fp5
specificity5   # 0.8647215

# 'Confusion matrix for threshold 

#        1     2
#  1    326   51
#  2     10   24

'Model Performance'
#[1] "Accuracy"          "85.15815"
#[1] "sensitivity"       "0.7058824"
#[1] "specificity"       "0.8647215"



# Top 10 important variables
sort(Atr_Dt$variable.importance,decreasing = TRUE)[1:10]

# We can observe that 6 out of 10 imp variables are more significance, so we'll process with those 6 variables.


Dt_imp_vars <- names(sort(Atr_Dt$variable.importance,decreasing = TRUE)[1:6])



Art_Dt1 <- rpart(Attrition ~ ., method="class", data=Atr_Train[,c("Attrition",Dt_imp_vars)])


Dt_pre1 = as.data.frame.matrix(predict(Art_Dt1,newdata = Atr_Test[,c("Attrition",Dt_imp_vars)],type = "prob"))

Dt_pre1 <- Dt_pre1$`2`

rocr_Dt_pre1 = prediction(Dt_pre1 ,Atr_Test$Attrition)
rocr_perf_Dt1 = performance(rocr_Dt_pre1,'tpr','fpr')
plot(rocr_perf_Dt1,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))


# We see that threshold 0.2 is the best threshold here.

threshold_Dt_pre1 <- 0.2

taba2 <- table(Atr_Test$Attrition ,Dt_pre1 > threshold_Dt_pre1)
taba2

#Accuracy

acc6 <- (sum(diag(taba2))/sum(taba2)) *100
acc6  #86.13139


# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp6 <- taba2[4]
tp_plus_fn6 <- taba2[4] + taba2[2]

sensitivity6 <- tp6/tp_plus_fn6
sensitivity6  # 0.3333333

# specificity tnr--> specificity = tn/(tn+FP)
tn6 <- taba2[1]
tn_plus_fp6 <- taba2[1] + taba2[3]

specificity6 <- tn6/tn_plus_fp6
specificity6   #  0.9791667

# 'Confusion matrix for threshold 

#        1     2
#  1    329    7
#  2     50   25

'Model Performance'
#[1] "Accuracy"          "85.40146"
#[1] "sensitivity"       "0.3333333"
#[1] "specificity"       "0.9702381"


prp(Art_Dt1)



####################################

## Random Forest
Atr_RF <- randomForest(Attrition ~ ., Atr_Train, ntree = 500)

RF_pre <- as.data.frame.matrix(predict(Atr_RF,Atr_Test,type = "prob"))

RF_pre <- RF_pre$`2`

rocr_RF_pre = prediction(RF_pre ,Atr_Test$Attrition)
rocr_perf_RF1 = performance(rocr_RF_pre,'tpr','fpr')
plot(rocr_perf_RF1,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))

# We see that threshold 0.3 is the best threshold here.

threshold_RF_pre1 <- 0.3

taba3 <- table(Atr_Test$Attrition ,RF_pre > threshold_RF_pre1)
taba3

#Accuracy

acc7 <- (sum(diag(taba3))/sum(taba3)) *100
acc7  #84.18491


# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp7 <- taba3[4]
tp_plus_fn7 <- taba3[4] + taba3[2]

sensitivity7 <- tp7/tp_plus_fn7
sensitivity7  # 0.453333

# specificity tnr--> specificity = tn/(tn+FP)
tn7 <- taba3[1]
tn_plus_fp7 <- taba3[1] + taba3[3]

specificity7 <- tn7/tn_plus_fp7
specificity7   #  0.9285714

# 'Confusion matrix for threshold 

#        1     2
#  1    312   24
#  2     41   34

'Model Performance'
#[1] "Accuracy"          "84.18491"
#[1] "sensitivity"       "0.453333"
#[1] "specificity"       "0.9285714"


# RF With imp variables

Atr_RF1 <- randomForest(Attrition ~ ., method="class", data=Atr_Train[,c("Attrition",Dt_imp_vars)], ntree = 500)

RF_pre1 <- as.data.frame.matrix(predict(Atr_RF1,newdata = Atr_Test[,c("Attrition",Dt_imp_vars)],type = "prob"))

RF_pre1 <- RF_pre1$`2`

rocr_RF_pre1 = prediction(RF_pre1 ,Atr_Test$Attrition)
rocr_perf_RF2 = performance(rocr_RF_pre1,'tpr','fpr')
plot(rocr_perf_RF2,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))

# We see that threshold 0.3 is the best threshold here.

threshold_RF_pre1 <- 0.3

taba5 <- table(Atr_Test$Attrition ,RF_pre1 > threshold_RF_pre1)
taba5

#Accuracy

acc9 <- (sum(diag(taba5))/sum(taba5)) *100
acc9  #83.21168


# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp9 <- taba5[4]
tp_plus_fn9 <- taba5[4] + taba5[2]

sensitivity9 <- tp9/tp_plus_fn9
sensitivity9  # 0.36

# specificity tnr--> specificity = tn/(tn+FP)
tn9 <- taba5[1]
tn_plus_fp9 <- taba5[1] + taba5[3]

specificity9 <- tn9/tn_plus_fp9
specificity9  #  0.9375

# 'Confusion matrix for threshold 

#        1     2
#  1    311   25
#  2     45   30

'Model Performance'
#[1] "Accuracy"          "82.96837"
#[1] "sensitivity"       "0.4"
#[1] "specificity"       "0.9255952"


#############################################

## Now let us try and improve the model by engineering new features.


Atr1 <- mutate(Atr,is_first_company=ifelse(NumCompaniesWorked == 1,TRUE,FALSE))

Atr1<- mutate(Atr, loyalty=(Atr$YearsAtCompany)/(Atr$TotalWorkingYears))

## lower the number more volatile is the employee

volatility = (Attrition1$TotalWorkingYears)/(Attrition1$NumCompaniesWorked)
volatility[which(is.infinite(volatility))] <- Attrition1$TotalWorkingYears[which(is.infinite(volatility))]

Atr1$volatility <- volatility

summary(Atr1)
Atr1$is_first_company <- factor(Atr1$is_first_company)

set.seed(3031)
Sample1 <- sample(2,nrow(Atr1),replace = TRUE, prob = c(.7,.3))

New_Atr_Train <- Atr1[Sample1==1,]
New_Atr_Test <- Atr1[Sample1==2,]

# Modeling with new features

Atr_Log2=glm(Attrition~ Age+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+OverTime+WorkLifeBalance+NumCompaniesWorked+StockOptionLevel+YearsWithCurrManager+JobRole+JobLevel+is_first_company+loyalty+volatility , New_Atr_Train,family = binomial)
summary(Atr_Log2)

Atr_pre4 <- predict(Atr_Log2, New_Atr_Test, type = 'response')

Atr_act4 <- data.frame(Atr_pre4,New_Atr_Test$Attrition)

colnames(Atr_act4) <- c('pred', 'actual')

rocr_Atr_pre4 = prediction(Atr_pre4 ,New_Atr_Test$Attrition)
rocr_perf_pre4 = performance(rocr_Atr_pre4,'tpr','fpr')
plot(rocr_perf_pre4,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))


threshold_pre4 = 0.3

taba4 <- table(New_Atr_Test$Attrition ,Atr_pre4 > threshold_pre)
taba4

#Accuracy

acc8 <- (sum(diag(taba4))/sum(taba4)) *100
acc8  #81.26521


# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp8 <- taba4[4]
tp_plus_fn8 <- taba4[4] + taba4[2]

sensitivity8 <- tp8/tp_plus_fn8
sensitivity8  # 0.3866667

# specificity tnr--> specificity = tn/(tn+FP)
tn8 <- taba4[1]
tn_plus_fp8 <- taba4[1] + taba4[3]

specificity8 <- tn8/tn_plus_fp8
specificity8   #  0.9077381

# 'Confusion matrix model with only important variable and new features'
#        1     2
#  1    305   31
#  2     46   29

'Model Performance'
#[1] "Accuracy"          "81.26521"
#[1] "sensitivity"       "0.3866667"
#[1] "specificity"       "0.9077381"

# We see that adding new feature has decreased accuracy.


################################
## Model Selection

## There are many metrics which can be used to select the best model, choice of that metric is often done by cosidering its impact on a business KPI. For some business presision is important ,for others recall and in some cases overall acurracy might be important.

Model <- data.frame(list("model_name" = c("DT all variables","DT important variables","logistic all variables","logistic important variables","Logistic with feature engineering","RF with all variables","RF important variables"),
                "Sensitivity" = c(sensitivity5,sensitivity6,sensitivitya,sensitivity4,sensitivity8,sensitivity7,sensitivity9),
                "Specificity" = c(specificity5,specificity6,specificitya,specificity4,specificity8,specificity7,specificity9),
                "Accuracy" = c(acc5,acc6,ac,acc4,acc8,acc7,acc9)))




Model



#################
#From the above statistics we conclude that for this use case:
  
 # - Decision Tree with all variables outperforms other models in terms of overall accuracy.
#- Inclusion of new features after feature engineering decreased the accuracy of Logistic regression model







plot(roc(Atr_Test$Attrition, Dt_pre), print.auc=TRUE)

plot(roc(Atr_Test$Attrition, Dt_pre1), print.auc = TRUE,col = "green", print.auc.y = .1, add = TRUE)

plot(roc(Atr_Test$Attrition, Atr_pre1), print.auc = TRUE,col = "blue", print.auc.y = .2, add = TRUE)

plot(roc(Atr_Test$Attrition, Atr_pre), print.auc = TRUE,col = "red", print.auc.y = .3, add = TRUE)

plot(roc(New_Atr_Test$Attrition, Atr_pre4), print.auc = TRUE,col = "pink", print.auc.y = .4, add = TRUE)




