library(caret); library(kernlab); 
#install.packages("RANN"); 
library(foreach);
#install.packages("randomForest");
#install.packages("doParallel");
library(doParallel);
library(randomForest);

if(!exists("training")){
  training <- read.csv("R/machinelearning/pml-training.csv", header=T, sep=',', na.strings=c("#DIV/0!","NA",""))
}

if(!exists("evaluation")){
  evaluation <- read.csv("R/machinelearning/pml-testing.csv", header=T, sep=',', na.strings=c("#DIV/0!","NA",""))
}

feature_set <- training[,6:160]
evaluation_set <- evaluation[,6:160]

#dummies <- dummyVars(classe ~ new_window, data = feature_set)
#head(predict(dummies,newdata=test))

feature_set<-feature_set[,colSums(is.na(feature_set)) == 0]
evaluation_set<-evaluation_set[,colSums(is.na(evaluation_set)) == 0]

for(i in c(1:ncol(feature_set))) {feature_set[,i] = as.numeric((feature_set[,i]))}
for(i in c(1:ncol(evaluation_set))) {evaluation_set[,i] = as.numeric((evaluation_set[,i]))}

nsv <- nearZeroVar(feature_set,saveMetrics=TRUE)

dim(feature_set)
training_set = feature_set[,!nsv$zeroVar]
dim(training_set)

idx <- createDataPartition(y=feature_set$classe, p=0.75, list=FALSE )
training_set <- feature_set[idx,]
testing_set <- feature_set[-idx,]


#head(training_set[,-ncol(training_set)])

set.seed(32343)
#preProcValues <- preProcess(training_set[-ncol(training_set)],method=c("center","scale","knnImpute"))
#trainTransformed <- predict(preProcValues, training_set)
#testTransformed <- predict(preProcValues, testing_set)
treeFit <- train(y=as.factor(training_set$classe), x=training_set[-ncol(training_set)],preProcess=c("center","scale","knnImpute"))

registerDoParallel()
classe <- training_set$classe
variables <- training_set[-ncol(training_set)]

rf <- foreach(ntree=rep(100, 4), .combine=randomForest::combine, .packages='randomForest') %dopar% {
  randomForest(variables, classe, ntree=ntree) 
}


predictions <- predict(rf, newdata=training_set)
confusionMatrix(predictions, factor(training_set$classe))

dim(evaluation_set)
levels(training_set)
levels(evaluation_set)
#####################################

hist(training$roll_belt)
hist(dummies$roll_belt)


featurePlot(x=training[,c("roll_belt","pitch_belt","yaw_belt")],
            y = training$classe,
            plot="pairs")

preObj <- preProcess(training[,-1],method=c("center","scale","knnImpute"))
testCapAveS <- predict(preObj,training[,-1])$capitalAve
mean(testCapAveS)

modelFit <- train(classe ~.,data=training, method="lda", preProcess=preObj)

modelFit <- train(classe ~.,data=training, method="lda", preProcess=c("center","scale","knnImpute"))



dim(training)
summary(training)




summary(preObj)
modelFit