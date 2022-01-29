# NN
##clean memory
rm(list=ls())
##install package
library(survival)
rats <- rats
library("plyr")
rats$sex <- revalue(rats$sex,c("m"="0","f"= "1"))
rats$sex<- as.numeric(rats$sex)
##partition data to test data and training data set.
set.seed(1000)
Part <- sample(2, nrow(rats), replace = T, prob = c(.8, .2))
training <- rats[Part==1,]
test <- rats[Part==2, ]
trainingtarget <- rats[Part==1,4]
testtarget <- rats[Part==2,4]
str(trainingtarget)
str(testtarget)
##Scaling: The neural network requires normalized values for better prediction, 
##just normalize or scale the predictor variables based on below function
Mean <- colMeans(training)
SD <- apply(training, 2, sd)
training <- scale(training, center = Mean, scale = SD)
test <- scale(test, center = Mean, scale = SD)
# fit neural network
library(neuralnet)
set.seed(10)
ANN_Rats <- neuralnet(status~rx+sex+time, data=training, hidden=3, act.fct = "logistic", linear.output = FALSE)
summary(ANN_Rats)
# plot neural network
plot(ANN_Rats)
## Prediction using neural network
Predict <- compute(ANN_Rats,test)
Prob <- Predict$net.result
