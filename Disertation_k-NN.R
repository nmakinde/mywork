df <- read.csv("EPLDataset.csv")

df

library(dplyr)
library(tidyverse)
library(caret)
library(ggplot2)
library(recipes)
library(lattice)
library(e1071)
library(Hmisc)
library(readxl)
library(survival)
library(mltools)
library(ggcorrplot)
library(psych)
library(class)
library(gmodels)
library(scatterplot3d)
library(ggcorrplot)
library(gtsummary)
library(varImp)
library(randomForest)
library(ROSE)
library(naivebayes)
library(party)
library(xgboost)
library(Matrix)
library(magrittr)

ncol(df) # To check for dataset column

nrow(df) #To check for dataset row

head(df)

tail(df)

#************DATA EXPLORATION*********************

describe(df) #To check for data description

summary(df) #To check the summary of the variables in the dataset

tbl_summary(dfa)


duplicated(dfa) #To check for duplicate rows

which(duplicated(dfa))#To check for duplicate rows

library(EnvStats)
#test <- rosnerTest(dfa$home_goals,
                   k = 5
#)
test






sapply(df,class) # To check the class of variables in the dataset

str (df) # To have an understanding of the data

dfa = subset(df, select = -c(matchweek,season)) #To drop the last two variables matchweek and season

dfa

table(dfa$winner) #To check the distribution of the target variable

#To get the percentage of the target variable

round (prop.table(table(dfa$winner))*100,)

barplot(prop.table(table(dfa$winner))*100, main="Target Distribution", xlab = "Winner", ylab = "Count")

summary(dfa)

D <- dfa

atr <- attributes(dfa)


# Pre-processing the dataset

for (i in 1:(ncol (dfa) -1)){
  if (is.character(dfa[, i]) ==TRUE) {
    for(j in 1:nrow(dfa)) {
      ascis <- as.numeric(charToRaw(dfa[j, i]))
      dfa[j, i] <- sum(ascis)
    }
  }
  dfa[, i] <- as.numeric(dfa[,i])
}

#coding the factor variable

dfa [, ncol (dfa)] <- as.factor (dfa[,ncol(dfa)])

#To create variable x to attach to the input

x <- dfa [, 1 :ncol(dfa) -1 ]

#To create variable y and attach to the output
y <- dfa [, ncol(dfa)]

#************Data Visualization***********************


#To create a whisker plot for the variable of the home_team
boxplot(dfa[,3])

#To create a whisker plot for the variable of the away_team 
boxplot(dfa[,8])

#Creating box plot for the some variables field from field (1 - 5)
par(mfrow=c(3,7))
for(i in 3:7) {
  boxplot(x[,i], main=names(dfa) [i])
}


boxplot(dfa$home_pos, dfa$away_pos,dfa$home_goals,dfa$away_goals,dfa$home_shots_on_target,dfa$away_shots_on_target, main="Boxplot visualisation",
         xlab="Variables", ylab="Values")





#To create a bar plot to breakdown my output attribute
plot(y)

head(dfa) 

#Finding Correlation OF the variables
correlationmatrix <- cor(dfa[,1:ncol(dfa)-1])
ggcorrplot(correlationmatrix)


#To explore the relationship of the variables between the target variable***************

ggplot(dfa, aes(home_goals,winner)) +geom_point()

featurePlot(x= x, y=y)

#**********Transforming the dataset*******************
normalize <- function(x) {
  return ((x- min(x))/ (max(x)- min(x)))
}

#normalize (c(1, 2, 3, 4, 5))

#Using lappyly function to apply normalize to all the features
#dfa_n <- as.data.frame(lapply(dfa[1:28], normalize))

#To find missing values in dataset
sum(is.na(dfa))


#Summarize data set to apply machine learning

library(caret)

#To use normalized dataset

dfa[,1:(ncol(dfa)-1)] <- scale(dfa[,1:(ncol(dfa)-1)])

dfa

summary(dfa)

#*************To create train and test data set on all variables on the data set*********************
set.seed(123)

dfa_n_train <- dfa[1:3467,]

dfa_n_test <- dfa[3468:4938,]




#******To apply the algorithm******

set.seed(123)

dfa_n_pred <- knn(dfa_n_train, dfa_n_test, dfa[1:3467,29], k=68)

dfa_n_pred

table(dfa_n_pred, dfa[3468:4938,29])

#**********Confusion Matrix***************

cm <- confusionMatrix(dfa_n_pred, dfa[3468:4938,29])

cm

#*********Model Improvement***************************


#********Checking for Predictor Importance*********
set.seed(2018)
quick_RF <- randomForest(x=dfa[,-ncol(dfa)],
                         y=dfa$winner,
                         ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:28,], aes(x=reorder(Variables, MSE),
                          y=MSE, fill=MSE)) +
  geom_bar(stat = 'identity', fill = "pink") +
  labs(x = 'Variables',
       y= '% increase in MSE if variable is randomly permuted',
       title = "Feature importance using RF") +
  theme(plot.title =
          element_text(size = 10, face = "bold", hjust = 0.5)) +
  coord_flip() + theme(legend.position="none")

top_features <- imp_DF$Variables[1:28]

selected_features <- dfa[,top_features]

selected_features$winner <-  dfa$winner

#View(selected_features)



#*****************Features Selection********************

dfa_FS = subset(dfa, select = -c(home_team,away_team,home_touch,home_pass,away_tackles,home_off,away_off,home_red,away_red,home_fouls,away_fouls))

dfa_FS                                  
                                   

dfa_FS2 = subset(dfa, select = -c(home_team,away_team,home_off,away_off,home_red,away_red,home_fouls,home_corner,away_corner,home_yellow,away_yellow))

dfa_FS2 

                                   
#*************To create train and test data set For Feature Selection*********************
set.seed(123)

dfa_FS_train <- dfa_FS[1:3467,]

dfa_FS_test <- dfa_FS[3468:4938,]                                   

set.seed(123)

dfa_FS2_train <- dfa_FS2[1:3467,]

dfa_FS2_test <- dfa_FS2[3468:4938,]   


                                   
                                
#******To apply the algorithm ON Feature Selection******

set.seed(123)

dfa_FS_pred <- knn(dfa_FS_train, dfa_FS_test, dfa[1:3467,29], k=68)

dfa_FS_pred

table(dfa_FS_pred, dfa[3468:4938,29])

set.seed(123)

dfa_FS2_pred <- knn(dfa_FS2_train, dfa_FS2_test, dfa[1:3467,29], k=68)

dfa_FS2_pred

table(dfa_FS2_pred, dfa[3468:4938,29])



#**********Confusion Matrix***************

CM_FS <- confusionMatrix(dfa_FS_pred, dfa[3468:4938,29])

CM_FS                                   



CM_FS2 <- confusionMatrix(dfa_FS2_pred, dfa[3468:4938,29])

CM_FS2   



#****Model Improvement using z- standardization ***************                                   
                                   
#dfa_z <- as.data.frame(scale(dfa_FS))                                   
                                   
#*************To create train and test data set For Feature Selection*********************
#dfa_z_train <- dfa_z[1:3454,]

#dfa_z_test <- dfa_z[3455:4938,]  

#******To apply the algorithm ON Feature Selection******

#dfa_z_pred <- knn(dfa_z_train, dfa_z_test, dfa[1:3454,29], k=68)

#dfa_z_pred

#table(dfa_z_pred, dfa[3455:4938,29])

#**********Confusion Matrix***************

#CM_Z <- confusionMatrix(dfa_z_pred, dfa[3455:4938,29])

#CM_Z

#APPLYING RANDOMFOREST ALGORITHM ON THE DATASET*****************

#************Splitting the data to Training and Test data**********

set.seed(123)

ind <- sample(2,nrow(dfa), replace = TRUE, prob = c(0.7, 0.3))

dfa_RF_train <- dfa[ind==1,]

dfa_RF_test <- dfa[ind==2,]

#******To apply the algorithm******

set.seed(222)
dfa_RFM <- randomForest(winner~.,data = dfa_RF_train)

dfa_RFM


#**************Algorithm with 200 ntree**********
set.seed(222)
dfa_RFM2 <- randomForest(winner~.,data = dfa_RF_train,
                         ntree = 200,
                         mtry = 3,
                         importance = TRUE,
                         PROXIMITY = TRUE)

dfa_RFM2





#attributes(dfa_RFM)

#**********Confusion Matrix***************

dfa_RFM_pred <- predict(dfa_RFM,dfa_RF_test)


CM_RF <- confusionMatrix(dfa_RFM_pred,dfa_RF_test$winner)

CM_RF

#confusion matrix with Pruning of ntree 200

dfa_RFM_pred2 <- predict(dfa_RFM2,dfa_RF_test)


CM_RF2 <- confusionMatrix(dfa_RFM_pred2,dfa_RF_test$winner)

CM_RF2





#********Error Rate********
plot(dfa_RFM)

plot(dfa_RFM2)

#To improve the model of RF

#*******Tune the RF Tree********************

t <- tuneRF(dfa_RF_train[,-29],dfa_RF_train[,29],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            trace = TRUE,
            improve = 0.05)


#********No. of nodes for the trees
hist(treesize(dfa_RFM),main='No. of Nodes for the Trees', col = 'green')


hist(treesize(dfa_RFM2),main='No. of Nodes for the Trees', col = 'green')


#********Variable Importance*********
varImpPlot(dfa_RFM)

varImpPlot(dfa_RFM,
          sort = T,
          n.var = 17,
          main = "Top 17 - Variable Importance")

importance(dfa_RFM)

varUsed(dfa_RFM)

#********Variable Importance 200 ntree*********

varImpPlot(dfa_RFM2,
           sort = T,
           n.var = 17,
           main = "Top 17 - Variable Importance")

importance(dfa_RFM)

varUsed(dfa_RFM)



#APPLYING NAVES BAYES ALGORITHM ON THE DATASET*****************

set.seed(123)

indNB <- sample(2,nrow(dfa), replace = TRUE, prob = c(0.7, 0.3))

dfa_NB_train <- dfa[indNB==1,]

dfa_NB_test <- dfa[indNB==2,]


#******To apply the algorithm******

set.seed(222)
dfa_NB <- naive_bayes(winner~.,data = dfa_NB_train,laplace=1)

dfa_NB

#**********Confusion Matrix***************

dfa_NB_pred <- predict(dfa_NB,dfa_NB_test)


CM_NB <- confusionMatrix(dfa_NB_pred,dfa_NB_test$winner)

CM_NB


#***********Applying the model with FS************

dfa_FSNB = subset(dfa, select = -c(home_team,away_team,home_off,away_off,home_red,away_red,home_fouls,home_corner,away_corner,home_yellow,away_yellow))

dfa_FSNB


#Create a dataset for the first 10 predictor variable to be ave to plotpairs.panel

DFP = subset(dfa, select = -c(home_team,away_team,home_off,away_off,home_red,away_red,home_fouls,home_corner,away_corner,home_yellow,away_yellow,home_shots,away_shots,home_pos,away_pos,home_tackles,away_tackles,away_fouls))

DFP

pairs.panels(DFP)

#************Splitting the data to Training and Test data**********

set.seed(123)

indFSNB <- sample(2,nrow(dfa_FSNB), replace = TRUE, prob = c(0.7, 0.3))

dfa_FSNB_train <- dfa_FSNB[indFSNB==1,]

dfa_FSNB_test <- dfa_FSNB[indFSNB==2,]

#******To apply the algorithm******

set.seed(222)
dfaNB_FS <- naive_bayes(winner~.,data = dfa_FSNB_train,laplace=1)

dfaNB_FS

#**********Confusion Matrix***************

dfa_FSNB_pred <- predict(dfaNB_FS,dfa_FSNB_test)


CM_FSNB <- confusionMatrix(dfa_FSNB_pred,dfa_FSNB_test$winner)

CM_FSNB


#APPLYING DECISION TREE ALGORITHM ON THE DATASET*****************

set.seed(123)

indDT <- sample(2,nrow(dfa), replace = TRUE, prob = c(0.7, 0.3))

dfa_DT_train <- dfa[indDT==1,]

dfa_DT_test <- dfa[indDT==2,]


#******To apply the algorithm******

set.seed(222)
dfa_DT <- ctree(winner~.,data = dfa_DT_train)

dfa_DT
plot(dfa_DT)



#attributes(dfa_DT)


#*******To prune the tree********
set.seed(222)
dfa_DT2 <- ctree(winner~.,data = dfa_DT_train, controls = ctree_control(mincriterion = 0.99,minsplit = 500))

dfa_DT2
plot(dfa_DT2)



#**********Confusion Matrix***************

dfa_DT_pred <- predict(dfa_DT,dfa_DT_test)


CM_DT <- confusionMatrix(dfa_DT_pred,dfa_DT_test$winner)

CM_DT



#****Confusion Matrix with control***********


dfa_DT_pred2 <- predict(dfa_DT2,dfa_DT_test)


CM_DT2 <- confusionMatrix(dfa_DT_pred2,dfa_DT_test$winner)

CM_DT2




#***********Applying the model with FS************

dfa_FSDT = subset(dfa, select = -c(home_team,away_team,home_off,away_off,home_red,away_red,home_fouls,home_corner,away_corner,home_yellow,away_yellow))

dfa_FSDT 


#************Splitting the data to Training and Test data**********

set.seed(123)

indFSDT <- sample(2,nrow(dfa_FSDT), replace = TRUE, prob = c(0.7, 0.3))

dfa_FSDT_train <- dfa_FSDT[indFSDT==1,]

dfa_FSDT_test <- dfa_FSDT[indFSDT==2,]


#******To apply the algorithm******

set.seed(222)
dfaDT_FS <- ctree(winner~.,data = dfa_FSDT_train)

dfaDT_FS
plot(dfaDT_FS)


#******To apply the algorithm with controls*********
set.seed(222)
dfaDT_FS2 <- ctree(winner~.,data = dfa_FSDT_train,controls = ctree_control(mincriterion = 0.99,minsplit = 500))

dfaDT_FS2
plot(dfaDT_FS2)


#**********Confusion Matrix***************

dfa_FSDT_pred <- predict(dfaDT_FS,dfa_FSDT_test)


CM_FSDT <- confusionMatrix(dfa_FSDT_pred,dfa_FSDT_test$winner)

CM_FSDT


#**********Confusion Matrix with control***************

dfa_FSDT_pred2 <- predict(dfaDT_FS2,dfa_FSDT_test)


CM_FSDT2 <- confusionMatrix(dfa_FSDT_pred2,dfa_FSDT_test$winner)

CM_FSDT2




#Objective 2

#Using the algorithms with the highest accuracy to predict Fantasy Premier League

FP <- read.csv("Fantasydataset.csv")

head(FP)

tail(FP)

summary(FP)

sapply(FP, class)

describe(FP)

which(duplicated(FP))

sum(is.na(FP))

table(FP$total_points)

plot(table(FP$total_points))

round (prop.table(table(FP$total_points))*100,)




#To create a new target for the dataset


FP1<-  mutate(FP,target_point = case_when(total_points > 0 ~ 'win',
                            TRUE ~ 'lose' ))





View(FP1)

summary(FP1)

sapply(FP1, class)

# Pre-processing the dataset

for (i in 1:(ncol (FP1) -1)){
  if (is.character(FP1[, i]) ==TRUE) {
    for(j in 1:nrow(FP1)) {
      ascis <- as.numeric(charToRaw(FP1[j, i]))
      FP1[j, i] <- sum(ascis)
    }
  }
  FP1[, i] <- as.numeric(FP1[,i])
}

#coding the factor variable

FP1 [, ncol (FP1)] <- as.factor (FP1[,ncol(FP1)])

#To create variable x to attach to the input

xFP1 <- FP1 [, 1 :ncol(FP1) -1 ]

#To create variable y and attach to the output
yFP1 <- FP1 [, ncol(FP1)]


plot(yFP1)


round (prop.table(table(FP1$total_point)),)

barplot(prop.table(table(FP1$total_point)), main="Target Distribution", xlab = "total_point", ylab = "Count")

table(FP1$total_points)

plot(table(FP1$total_points))

#To get the percentage of the target variable

round (prop.table(table(FP1$target_point))*100,)

barplot(prop.table(table(FP1$target_point))*100, main="Target Distribution", xlab = "target_point", ylab = "Count")



summary(FP1)

sapply(FP1, class)

#To use normalized dataset

FP1[,1:(ncol(FP1)-1)] <- scale(FP1[,1:(ncol(FP1)-1)])

FP1

summary(FP1)


FPL = subset(FP1, select = -c(total_points)) #To drop the last  variables matchweek and season

FPL


#APPLYING RANDOMFOREST ALGORITHM ON THE DATASET*****************
  
#************Splitting the data to Training and Test data**********
  
set.seed(123)

indFPL <- sample(2,nrow(FPL), replace = TRUE, prob = c(0.7, 0.3))

dfa_FPL_train <- FPL[ind==1,]

dfa_FPL_test <- FPL[ind==2,]

#******To apply the algorithm******

set.seed(222)
dfa_FPL <- randomForest(target_point~.,data = dfa_FPL_train)

dfa_FPL


#**************Algorithm with 200 ntree**********
set.seed(222)
dfa_FPL2 <- randomForest(target_point~.,data = dfa_FPL_train,
                         ntree = 200,
                         mtry = 3,
                         importance = TRUE,
                         PROXIMITY = TRUE)


dfa_FPL2

#********Error Rate********
plot(dfa_FPL)

plot(dfa_FPL2)




#**********Confusion Matrix***************

dfa_FPL_pred <- predict(dfa_FPL,dfa_FPL_test)


CM_FPL <- confusionMatrix(dfa_FPL_pred,dfa_FPL_test$target_point)

CM_FPL

#Confusion Matrix with ntree 200

dfa_FPL2_pred <- predict(dfa_FPL2,dfa_FPL_test)


CM_FPL2 <- confusionMatrix(dfa_FPL2_pred,dfa_FPL_test$target_point)

CM_FPL2

#*****Variable Importance*****
varImpPlot(dfa_FPL2)

varImpPlot(dfa_FPL2,
           sort = T,
           n.var = 17,
           main = "Top 17 - Variable Importance")

importance(dfa_FPL2)

varUsed(dfa_FPL2)


#*******************Feature Selection
