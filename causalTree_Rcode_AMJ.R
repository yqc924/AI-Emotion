#install and import causal tree package
rm(list = ls())

install.packages("devtools")
install_github("susanathey/causalTree")

library(devtools) 
library(causalTree)

set.seed(1)

#data importion and cleaning
data <- read.csv("E:/Research Projects/AI/AI & emotion/Data/study 2/Data2/Data-Study2-20210220.csv", sep=",", header=TRUE)
data$female.f <-factor(data$female)
data$agent.f <-factor(data$agent)

#data segmentation based on emotion assignment and delinquency severity
data_happyminor <- subset(data,expectedemotion==2&delinquency==0)
data_angryminor <- subset(data,expectedemotion==1&delinquency==0)
data_happysevere <- subset(data,expectedemotion==2&delinquency==1)
data_angrylate <- subset(data,expectedemotion==1&delinquency==1)


##########Causal Tree Part##########

#####I.Required to display a happy emotion to serve customers with light delinquency#####

#1.1 Causal Tree Model Training
tree_happyearly <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_happyearly, treatment = data_happyearly$agent,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                   xval = 10, cp = 0, minsize = 15, propensity = 0.5)

#1.2 Causal Tree Model Fitting/Prediction
opcp <- tree_happyearly$cptable[,0][which.min(tree_happyearly$cptable[,4])]
opfit <- prune(tree_happyearly, cp=1.113011e-05)

#1.3 Causal Tree Visualization
rpart.plot(opfit)




#####II.Required to display an angry emotion to serve customers with light delinquency#####

#2.1 Causal Tree Model Training
tree_angryearly <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_angryearly, treatment = data_angryearly$agent,
                              split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                              xval = 10, cp = 0, minsize = 15, propensity = 0.5)

#2.2 Causal Tree Model Fitting/Prediction
opcp <- tree_angryearly$cptable[,0][which.min(tree_angryearly$cptable[,4])]
opfit <- prune(tree_angryearly, cp=1.113011e-05)

#2.3 Causal Tree Visualization
rpart.plot(opfit)




#####III.Required to display a happy emotion to serve customers with severe delinquency#####

#3.1 Causal Tree Model Training
tree_happylate <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_happylate, treatment = data_happylate$agent,
                              split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                              xval = 10, cp = 0, minsize = 15, propensity = 0.5)

#3.2 Causal Tree Model Fitting/Prediction
opcp <- tree_happylate$cptable[,0][which.min(tree_happylate$cptable[,4])]
opfit <- prune(tree_happylate, cp=1.113011e-05)

#3.3 Causal Tree Visualization
rpart.plot(opfit)




#####IV.Required to display an angry emotion to serve customers with severe delinquency#####

#4.1 Causal Tree Model Training
tree_angrylate <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_angrylate, treatment = data_angrylate$agent,
                             split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                             xval = 10, cp = 0, minsize = 15, propensity = 0.5)

#4.2 Causal Tree Model Fitting/Prediction
opcp <- tree_angrylate$cptable[,0][which.min(tree_angrylate$cptable[,4])]
opfit <- prune(tree_angrylate, cp=1.113011e-05)

#4.3 Causal Tree Visualization
rpart.plot(opfit)

