library(caret); library(kernlab);

if(!exists("training")){
  training <- read.csv("R/machinelearning/pml-training.csv", header=T, sep=',', na.strings="NA", comment.char="", quote='\"')
}

if(!exists("testset")){
  testset <- read.csv("R/machinelearning/pml-testing.csv", header=T, sep=',', na.strings="NA", comment.char="", quote='\"')
}

set.seed(32343)
modelFit <- train(classe ~.,data=training, method="glm")
modelFit