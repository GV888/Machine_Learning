library(caret); library(kernlab); 
library(foreach);
#install.packages("randomForest");
#install.packages("doParallel");
library(doParallel);
library(randomForest);

if(!exists("training")){
  training <- read.csv("c:/Users/Viktor/workspace/coursera/Machine_Learning/pml-training.csv", header=T, sep=',', na.strings=c("#DIV/0!","NA",""))
}

if(!exists("evaluation")){
  evaluation <- read.csv("c:/Users/Viktor/workspace/coursera/Machine_Learning/pml-testing.csv", header=T, sep=',', na.strings=c("#DIV/0!","NA",""))
}

feature_set <- training[,6:160]
evaluation_set <- evaluation[,6:160]


feature_set<-feature_set[,colSums(is.na(feature_set)) == 0]
evaluation_set<-evaluation_set[,colSums(is.na(evaluation_set)) == 0]

for(i in c(1:ncol(feature_set))) {feature_set[,i] = as.numeric((feature_set[,i]))}
for(i in c(1:ncol(evaluation_set))) {evaluation_set[,i] = as.numeric((evaluation_set[,i]))}

#nsv <- nearZeroVar(feature_set,saveMetrics=TRUE)
#dim(feature_set)
#training_set = feature_set[,!nsv$zeroVar]
#dim(training_set)

idx <- createDataPartition(y=feature_set$classe, p=0.75, list=FALSE )
training_set <- feature_set[idx,]
testing_set <- feature_set[-idx,]

testing_set$classe <- factor(testing_set$classe)

set.seed(32343)
registerDoParallel()
modelFit <- train(y=as.factor(training_set$classe), x=training_set[-ncol(training_set)], tuneGrid=data.frame(mtry=3), trControl=trainControl(method="none"), method="parRF")
prediction <- predict(modelFit, newdata=testing_set)
confusionMatrix(prediction, testing_set$classe)

#print(plot(varImp(modelFit, scale = FALSE)))




