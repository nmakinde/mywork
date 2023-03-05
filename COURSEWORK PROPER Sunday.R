#*********MACHINE LEARNING COURSE WORK - GROUP 1**********#

#********LOADING NEEDED LIBRARIES***********#

library(tidyverse)
library(caret)
library(ggplot2)
library(recipes)
library(dplyr)
library(lattice)
library(caret)
library(e1071)
library(Hmisc)
library(readxl)
library(survival)
library(mltools)
library(C50)
library(ggcorrplot)
library(rpart)
library(rpart.plot)
library(Cubist)
library(psych)
library(gmodels)
library(ROSE)

#****************************
# STEP 1 - DATA COLLECTION
# ***************************

# READ THE DATA
Data1 <- read.csv("PowerData1.csv")
Data2 <- read.csv("PowerData2.csv")
Data3 <- read.csv("PowerData3.csv")

################################################################################
#pre-processing price dataset (Data2) before merging - Average Price was used for all customers
################################################################################
price_data <- Data2 %>%
  group_by(id) %>%
  summarise_at(vars(PE1, PE2, PE3, PP1, PP2, PP3), list(name = mean))

################################################################################
#MERGE DATASETS - DATA 1 & PRICE_DATA
################################################################################

Merged_Data <- merge(Data1,price_data, by="id")


#Merge merged data with 3 using id as unique identifier
powerco_maindata <- merge(Merged_Data, Data3, by ="id")

#Now power_data set is the final merged data set to be used for further steps
#export the new price data file

write.table(powerco_maindata, file = "Final_merged_all_data.csv", sep=",", row.names = F)

PD <- powerco_maindata # Copy of Data to another variable for checking change in actual data set

#*********BASIC DATA EXPLORATION*****
#An overview of our Data Set

summary(powerco_maindata) #summary of the data set
describe(powerco_maindata) #Statistics description of the data set
str(powerco_maindata) #structure of our data set
attr <- attributes(powerco_maindata)


#****************************
# STEP 2 - DATA PREPARATION
# ***************************

# Pre-processing the data set to convert all the features to numeric value 
#calculating ASCII values

for (i in 1:(ncol(powerco_maindata)-1)) {
  if (is.character(powerco_maindata[, i])==TRUE){
    for(j in 1:nrow(powerco_maindata)){
      ascis <- as.numeric(charToRaw(powerco_maindata[j, i]))
      powerco_maindata[ j, i] <- sum(ascis)
    }
  }
  powerco_maindata[,i] <- as.numeric(powerco_maindata[,i])
}

# after converting the whole data set (except last column) to numeric, converting the 
#last column to factor to be used as class label

powerco_maindata[,ncol(powerco_maindata)] <- as.factor(powerco_maindata[,ncol(powerco_maindata)])

#Confirm that data set has been converted

sapply(powerco_maindata, class)

#Create variable "x" and attach to it the input attributes of the data set

x_main <- powerco_maindata[,1:ncol(powerco_maindata)-1] # a variable attached to the input variables

#Create variable "y" and attach to it the input attributes of the data set

y_main <- powerco_maindata[,ncol(powerco_maindata)] #a variable attached to the output variable

#****************************
# STEP 3 - DATA EXPLORATION
# ***************************

#Whisker Plot for some of the input variables



#Correlation Matrix to determine variables to explore

correlation_matrix1 <- cor(x)

ggcorrplot(correlation_matrix1)

featurePlot(x=x_main, y=y_main, plot = "box")


# Create a whisker plot for some of the input variables of the data set
par(mfrow=c(2,6))

boxplot(powerco_maindata[,2], main=names(powerco_maindata)[2])
boxplot(powerco_maindata[,11], main=names(powerco_maindata)[11])
boxplot(powerco_maindata[,4], main=names(powerco_maindata)[4])
boxplot(powerco_maindata[,5], main=names(powerco_maindata)[5])
boxplot(powerco_maindata[,6], main=names(powerco_maindata)[6])

#Distribution of our Target Variable

prop.table(table(powerco_maindata$churn)) * 100

plot(powerco_maindata$churn) #plot of our target class



# Scatter plot matrix of some columns of the dataset.

featurePlot(powerco_maindata[,5:12],powerco_maindata[,ncol(powerco_maindata)], plot = if(is.factor(powerco_maindata[,ncol(powerco_maindata)]))
  "strip" else "scatter")

#Dropping variables based on Domain Knowledge and Missing Values

powerco_maindata <- powerco_maindata[-1] #Dropping ID 

#Missing Values Approach

colSums(is.na(powerco_maindata)) # Identify the total number of missing values per columnn

powerco_no_NA <- powerco_maindata[-c(2,12,13,14,15)] # Dropping columns with high number of Missing Values (NA)

summary(powerco_no_NA)

#Using imputation replace the columns with NAs with their mean

for(i in 1:ncol(powerco_no_NA)){
  powerco_no_NA[is.na(powerco_no_NA[,i]), i] <- mean(powerco_no_NA[,i], na.rm = TRUE)
}

summary(powerco_no_NA)
describe(powerco_no_NA)

#Create variable "x" and attach to it the input attributes of the data set after dropping variables with missing value

x_na <- powerco_no_NA[,1:ncol(powerco_no_NA)-1] # a variable attached to the input variables

#Create variable "y" and attach to it the input attributes of the data set

y_na <- powerco_no_NA[,ncol(powerco_no_NA)] #a variable attached to the output variable



#Due to the difference in Range of Values of the Dataset Normalisation is done

powerco_no_NA[,1:(ncol(powerco_no_NA)-1)] <- scale(powerco_no_NA[,1:(ncol(powerco_no_NA)-1)])


#*****************************************
# STEP 4a - TRAINING A MODEL ON THE DATA
# ****************************************


#displaying the performance metrics on screen
message("\t Folds \ t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \ t Precision% \t Recall%")

#create a blank data frame to store the performance metrics scores
pf = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc <- 0 #-performance frame counter

#To create the training and test dataset
FD <- 5
Folds <- createFolds(powerco_no_NA$churn, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){ 
  pfc <- pfc+1
  
  Held_Out_Indices = Folds[[i]]
  Training_Set = powerco_no_NA [Held_Out_Indices,]
  Testing_Set = powerco_no_NA [-Held_Out_Indices,]
}

#To build the classifier
TrainedClassifier <- C5.0(churn~., data = Training_Set)

TrainedClassifier

summary(TrainedClassifier)






#*********************************************
# STEP 4b - EVALUATING THE MODEL PERFORMANCE - RAW DATASET
# ********************************************

Predictions <- predict(TrainedClassifier, Testing_Set)

cm <- confusionMatrix(Testing_Set$churn, Predictions) 

CrossTable(Testing_Set$churn, Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))






#*********************************************
# STEP 5 - PERFORMANCE IMPROVEMENT - Adaptive Boosting
# ********************************************

AB <- C5.0(churn~., Training_Set, trials = 10)

AB

summary(AB)

AB_Predictions <- predict(AB, Testing_Set)


AB_CM <- confusionMatrix(Testing_Set$churn, AB_Predictions) 

CrossTable(Testing_Set$churn, AB_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#*********************************************
# STEP 5 - PERFORMANCE IMPROVEMENT - Cost Matrix
# ********************************************
cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

CostMatrix <- C5.0(churn~., Training_Set, costs=cost_mat)

CostMatrix

summary(CostMatrix)

CM_Predictions <- predict(CostMatrix, Testing_Set)

CM_CM <- confusionMatrix(Testing_Set$churn, CM_Predictions) 

CrossTable(Testing_Set$churn, CM_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#*********************************************************************************************
# STEP 6 - TRAINING A MODEL ON THE DATASET BALANCE 60-40
# ********************************************************************************************
library(ROSE)
both <- ovun.sample(churn~., data=Training_Set, method = "both",
                    p = 0.4,
                    seed = 222,
                    N = 12877)$data

table(both$churn)

prop.table(table(both$churn)) * 100

#model creation for 60- 40


TC60 <-  C5.0(churn~., data = both)

TC60

summary(TC60)

#Evaluate the model performance on the Test dataset

TC60_Predictions <- predict(TC60, Testing_Set)

cm <- confusionMatrix(Testing_Set$churn, TC60_Predictions) 

CrossTable(Testing_Set$churn, TC60_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 60-40 balancing by Adaptive boosting

AB60 <- C5.0(churn~., both, trials = 10)

AB60

summary(AB60)

AB60_Predictions <- predict(AB60, Testing_Set)


AB60_CM <- confusionMatrix(Testing_Set$churn, AB60_Predictions) 

CrossTable(Testing_Set$churn, AB60_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 60-40 balancing by Cost matrix

cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

CostMatrix60 <- C5.0(churn~., both, costs=cost_mat)

CostMatrix60

summary(CostMatrix60)

CM60_Predictions <- predict(CostMatrix60, Testing_Set)

CM60_CM <- confusionMatrix(Testing_Set$churn, CM60_Predictions) 

CrossTable(Testing_Set$churn, CM60_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#*********************************************************************************************
# STEP 6 - TRAINING A MODEL ON THE DATASET BALANCE 70-30
# ********************************************************************************************


both70 <- ovun.sample(churn~., data=Training_Set, method = "both",
                    p = 0.3,
                    seed = 222,
                    N = 12877)$data

table(both70$churn)

prop.table(table(both70$churn)) * 100

#model creation for 70 - 30


TC70 <-  C5.0(churn~., data = both70)

TC70

summary(TC70)

#Evaluate the model performance on the Test dataset

TC70_Predictions <- predict(TC70, Testing_Set)

CM70 <- confusionMatrix(Testing_Set$churn, TC70_Predictions) 

CrossTable(Testing_Set$churn, TC70_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#Improving the performance of 70-30 balancing by Adaptive boosting

AB70 <- C5.0(churn~., both70, trials = 10)

AB70

summary(AB70)

AB70_Predictions <- predict(AB70, Testing_Set)


AB70_CM <- confusionMatrix(Testing_Set$churn, AB70_Predictions) 

CrossTable(Testing_Set$churn, AB70_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 70-30 balancing by Cost Matrix

cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

CostMatrix70 <- C5.0(churn~., both70, costs=cost_mat)

CostMatrix70

summary(CostMatrix70)

CM70_Predictions <- predict(CostMatrix70, Testing_Set)

CM70_CM <- confusionMatrix(Testing_Set$churn, CM70_Predictions) 

CrossTable(Testing_Set$churn, CM70_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#*********************************************************************************************
# STEP 7 - TRAINING A MODEL ON THE DATASET BALANCE 80-20
# ********************************************************************************************

both80 <- ovun.sample(churn~., data=Training_Set, method = "both",
                      p = 0.2,
                      seed = 222,
                      N = 12877)$data

table(both80$churn)

prop.table(table(both80$churn)) * 100


#model creation for 80 - 20


TC80 <-  C5.0(churn~., data = both80)

TC80

summary(TC80)


#Evaluate the model performance on the Test dataset

TC80_Predictions <- predict(TC80, Testing_Set)

CM80 <- confusionMatrix(Testing_Set$churn, TC80_Predictions) 

CrossTable(Testing_Set$churn, TC80_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#Improving the performance of 80-20 balancing by Adaptive boosting

AB80 <- C5.0(churn~., both80, trials = 10)

AB80

summary(AB80)

AB80_Predictions <- predict(AB80, Testing_Set)


AB80_CM <- confusionMatrix(Testing_Set$churn, AB80_Predictions) 

CrossTable(Testing_Set$churn, AB80_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 80-20 balancing by Cost Matrix

cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

CostMatrix80 <- C5.0(churn~., both80, costs=cost_mat)

CostMatrix80

summary(CostMatrix80)

CM80_Predictions <- predict(CostMatrix80, Testing_Set)

CM80_CM <- confusionMatrix(Testing_Set$churn, CM80_Predictions) 

CrossTable(Testing_Set$churn, CM80_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#********************************************************************************************************
#FEATURES CREATION
#*********************************************************************************************************
powerco_maindata$TP1 <- powerco_maindata$PE1_name + powerco_maindata$PP1_name
powerco_maindata$TP2 <- powerco_maindata$PE2_name + powerco_maindata$PP2_name
powerco_maindata$TP3 <- powerco_maindata$PE3_name + powerco_maindata$PP3_name

summary(powerco_maindata)

View(powerco_maindata)


#**************************************************************************************************************
#FEATURES SELECTION
#***************************************************************************************************************

powerco_final_features <- powerco_maindata[-c(2,12,13,14,15,7,8,9,10,11,30,32,33,34,35,36,37)] #where 32-37 variables are the old price given in the data


summary(powerco_final_features)

View(powerco_final_features)

#using imputation to replace the remaining columns with NAs with their mean

for(i in 1:ncol(powerco_final_features)){powerco_final_features[is.na(powerco_final_features[,i]), i] <- mean(powerco_final_features[,i], na.rm =TRUE)
}

summary(powerco_final_features)

sapply(powerco_final_features, class)

#Relocating churn to the last column
powerco_final_features1 <- powerco_final_features%>% relocate(TP1,TP2, TP3, .before = churn)


#Due to the difference in Range of Values of the Dataset Normalisation is done

powerco_final_features1[,1:(ncol(powerco_final_features1)-1)] <- scale(powerco_final_features1[,1:(ncol(powerco_final_features1)-1)])



#Create variable "x" and attach to it the input attributes of the data set after dropping variables with missing value

x_FS <- powerco_final_features1[,1:ncol(powerco_final_features1)-1] # a variable attached to the input variables

#Create variable "y" and attach to it the input attributes of the data set

y_FS <- powerco_final_features1[,ncol(powerco_final_features1)] #a variable attached to the output variable






#Checking the predictor importance**************************************************************

library(randomforest)

set.seed(2018)

PI_RF <- randomForest(x=powerco_final_features1[,-ncol(powerco_final_features1)], y=powerco_final_features1$churn, ntree=100, importance=TRUE)

IMP_RF <- importance(PI_RF)

IMP_DF <- data.frame(Variables = row.names(IMP_RF), MSE =IMP_RF[,1])

IMP_DF <- IMP_DF[order(IMP_DF$MSE, decreasing = TRUE),]



#To visualize the predictor importance

ggplot(IMP_DF[1:23,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat= "identity", fill="pink") + labs(x="Variables", y= "%increase in MSE if variable is randomly permuted", title = "Feature Importance using RF") + theme(plot.title = element_text(size = 10, face ="bold", hjust = 0.5)) + coord_flip() + theme(legend.position="none")

top_features_23 <- IMP_DF$Variables[1:23]

selected_features_23 <- powerco_final_features1[,top_features_23]

selected_features_23$churn <- powerco_final_features1$churn

View(selected_features_23)

#correlation Matrix of the feature importance
correlationmatrix <- cor(powerco_final_features1[,1:ncol(powerco_final_features1)-1])
ggcorrplot(correlationmatrix)

#**************************************************************************************************
#FEATURES SELECTION AFTER CHECKING FOR MULTICOLINERAITY
#***********************************************************************************************

powerco_final_features2 <- powerco_final_features1[-c(5,7,10,11,12,16,22,23)] 


summary(powerco_final_features2)

#Create variable "x" and attach to it the input attributes of the data set after dropping variables with missing value

x_mc <- powerco_final_features2[,1:ncol(powerco_final_features2)-1] # a variable attached to the input variables

#Create variable "y" and attach to it the input attributes of the data set

y_mc <- powerco_final_features2[,ncol(powerco_final_features2)] #a variable attached to the output variable


#model creation for new dataset Powerco_final_Selection2

sapply(powerco_final_features2, class)


#displaying the performance metrics on screen
message("\t Folds \ t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \ t Precision% \t Recall%")

#create a blank data frame to store the performance metrics scores
pf = data.frame(matrix(
  vector(), 5, 6, dimnames=list(c("Fold-1", "Fold-2", "Fold-3", "Fold-4", "Fold-5"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
  stringsAsFactors=F)

pfc <- 0 #-performance frame counter

#To create the training and test dataset for the new dataset with 9 variables
FD <- 5
Folds <- createFolds(powerco_final_features2$churn, k = FD, list = TRUE, returnTrain = TRUE)
for (i in 1:FD){ 
  pfc <- pfc+1
  
  Held_Out_Indices = Folds[[i]]
  FS_Training_Set = powerco_final_features2 [Held_Out_Indices,]
  FS_Testing_Set = powerco_final_features2 [-Held_Out_Indices,]
}


#To check the balancing of the new dataset
prop.table(table(powerco_final_features2$churn)) * 100


#
#*********************************************************************************************
# STEP 8 - TRAINING A MODEL ON THE FEATURE SELECTION DATASET BALANCE 60-40
# ********************************************************************************************
library(ROSE)
FS_both60 <- ovun.sample(churn~., data=FS_Training_Set, method = "both",
                    p = 0.4,
                    seed = 222,
                    N = 12877)$data

table(FS_both60$churn)

prop.table(table(FS_both60$churn)) * 100

#model creation for Feature Selection 60- 40


FS60 <-  C5.0(churn~., data = FS_both60)

FS60

summary(FS60)

#Evaluate the model performance on the Test dataset

FS60_Predictions <- predict(FS60, FS_Testing_Set)

FS60_cm <- confusionMatrix(FS_Testing_Set$churn, FS60_Predictions) 

CrossTable(FS_Testing_Set$churn, FS60_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#Improving the performance of 60-40 FEATURE SELECTION balancing by Adaptive boosting

FS_AB60 <- C5.0(churn~., FS_both60, trials = 10)

FS_AB60

summary(FS_AB60)

FS_AB60_Predictions <- predict(FS_AB60, FS_Testing_Set)


FS_AB60_CM <- confusionMatrix(FS_Testing_Set$churn, FS_AB60_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_AB60_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 60-40 FEATURES SELECTION balancing by Cost matrix

cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

FS_CostMatrix60 <- C5.0(churn~., FS_both60, costs=cost_mat)

FS_CostMatrix60

summary(FS_CostMatrix60)

FS_CM60_Predictions <- predict(FS_CostMatrix60, FS_Testing_Set)

FS_CM60_CM <- confusionMatrix(FS_Testing_Set$churn, FS_CM60_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_CM60_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#*********************************************************************************************
# STEP 6 - TRAINING THE FS MODEL ON THE DATASET BALANCE 70-30
# ********************************************************************************************


FS_both70 <- ovun.sample(churn~., data=FS_Training_Set, method = "both",
                      p = 0.3,
                      seed = 222,
                      N = 12877)$data

table(FS_both70$churn)

prop.table(table(FS_both70$churn)) * 100


#model creation for  FEATURES SELECTION DATASET 70 - 30


FS_TC70 <-  C5.0(churn~., data = FS_both70)

FS_TC70

summary(FS_TC70)

#Evaluate the model performance on the Test dataset

FS_TC70_Predictions <- predict(FS_TC70, FS_Testing_Set)

FS_CM70 <- confusionMatrix(FS_Testing_Set$churn, FS_TC70_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_TC70_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#Improving the performance of 70-30 FEATURE SELECTION balancing by Adaptive boosting

FS_AB70 <- C5.0(churn~., FS_both70, trials = 10)

FS_AB70

summary(FS_AB70)

FS_AB70_Predictions <- predict(FS_AB70, FS_Testing_Set)


FS_AB70_CM <- confusionMatrix(FS_Testing_Set$churn, FS_AB70_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_AB70_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 70-30 FEATURES SELECTION balancing by Cost matrix

cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

FS_CostMatrix70 <- C5.0(churn~., FS_both70, costs=cost_mat)

FS_CostMatrix70

summary(FS_CostMatrix70)

FS_CM70_Predictions <- predict(FS_CostMatrix70, FS_Testing_Set)

FS_C760_CM <- confusionMatrix(FS_Testing_Set$churn, FS_CM70_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_CM70_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#*********************************************************************************************
# STEP 7 - TRAINING A MODEL ON THE FEATURES SET DATASET BALANCE 80-20
# ********************************************************************************************

FS_both80 <- ovun.sample(churn~., data=FS_Training_Set, method = "both",
                      p = 0.2,
                      seed = 222,
                      N = 12877)$data

table(FS_both80$churn)

prop.table(table(FS_both80$churn)) * 100


#model creation for 80 - 20 FEATURES SELECTION


FS_TC80 <-  C5.0(churn~., data = FS_both80)

FS_TC80

summary(FS_TC80)

#Evaluate the model performance on the Test dataset

FS_TC80_Predictions <- predict(FS_TC80, FS_Testing_Set)

FS_CM80 <- confusionMatrix(FS_Testing_Set$churn, FS_TC80_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_TC80_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


#Improving the performance of 80-20 balancing by Adaptive boosting

FS_AB80 <- C5.0(churn~., FS_both80, trials = 10)

FS_AB80

summary(FS_AB80)

FS_AB80_Predictions <- predict(FS_AB80, FS_Testing_Set)


FS_AB80_CM <- confusionMatrix(FS_Testing_Set$churn, FS_AB80_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_AB80_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))

#Improving the performance of 80-20 balancing by Cost Matrix

cost_mat <- matrix(c(0,1,4,0), nrow = 2)

rownames(cost_mat) <- colnames(cost_mat) <- c(0,1)

FS_CostMatrix80 <- C5.0(churn~., FS_both80, costs=cost_mat)

FS_CostMatrix80

summary(FS_CostMatrix80)

FS_CM80_Predictions <- predict(FS_CostMatrix80, FS_Testing_Set)#predicting the model

FS_CM80_CM <- confusionMatrix(FS_Testing_Set$churn, FS_CM80_Predictions) 

CrossTable(FS_Testing_Set$churn, FS_CM80_Predictions,prop.chisq = FALSE, prop.c =  FALSE, prop.r =  FALSE, dnn =  c("actual churn", " predicted churn"))


