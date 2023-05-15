#install and import causal tree package
rm(list = ls())
install.packages("devtools")
install_github("susanathey/causalTree")
library(devtools) 
library(causalTree)

set.seed(1)

#data importion and cleaning
data <- read.csv("data.csv", sep=",", header=TRUE)
data$female.f <-factor(data$female)
data$agent.f <-factor(data$agent)

#data segmentation based on emotion assignment and delinquency severity
data_happyminor <- subset(data,expectedemotion==2&delinquency==0)
data_angryminor <- subset(data,expectedemotion==1&delinquency==0)
data_happysevere <- subset(data,expectedemotion==2&delinquency==1)
data_angrysevere <- subset(data,expectedemotion==1&delinquency==1)


##########Causal Tree Part##########

#####I.Required to display a happy emotion to serve customers with light delinquency#####
#Causal Tree Model Training
tree_happyminor <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_happyminor, treatment = data_happyminor$agent,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                   xval = 10, cp = 0, minsize = 15, propensity = 0.5)
#Causal Tree Model Fitting/Prediction
opcp <- tree_happyminor$cptable[,0][which.min(tree_happyminor$cptable[,4])]
opfit <- prune(tree_happyminor, cp=1.113011e-05)
#Causal Tree Visualization
rpart.plot(opfit)


#####II.Required to display an angry emotion to serve customers with light delinquency#####
#Causal Tree Model Training
tree_angryminor <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_angryminor, treatment = data_angryminor$agent,
                              split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                              xval = 10, cp = 0, minsize = 15, propensity = 0.5)
#Causal Tree Model Fitting/Prediction
opcp <- tree_angryminor$cptable[,0][which.min(tree_angryminor$cptable[,4])]
opfit <- prune(tree_angryminor, cp=1.113011e-05)
#Causal Tree Visualization
rpart.plot(opfit)

#####III.Required to display a happy emotion to serve customers with severe delinquency#####
#Causal Tree Model Training
tree_happysevere <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_happysevere, treatment = data_happysevere$agent,
                              split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                              xval = 10, cp = 0, minsize = 15, propensity = 0.5)
#Causal Tree Model Fitting/Prediction
opcp <- tree_happysevere$cptable[,0][which.min(tree_happysevere$cptable[,4])]
opfit <- prune(tree_happysevere, cp=1.113011e-05)
#Causal Tree Visualization
rpart.plot(opfit)

#####IV.Required to display an angry emotion to serve customers with severe delinquency#####
#Causal Tree Model Training
tree_angrysevere <- causalTree(repayment ~ gender + age + edu + creditcardspending + onlineshopping + onlinedebt, data = data_angrysevere, treatment = data_angrysevere$agent,
                             split.Rule = "CT", cv.option = "CT", split.Honest = T, split.alpha=0.5, cv.Honest = T, split.Bucket = T, 
                             xval = 10, cp = 0, minsize = 15, propensity = 0.5)
#Causal Tree Model Fitting/Prediction
opcp <- tree_angrysevere$cptable[,0][which.min(tree_angrysevere$cptable[,4])]
opfit <- prune(tree_angrysevere, cp=1.113011e-05)
#Causal Tree Visualization
rpart.plot(opfit)

