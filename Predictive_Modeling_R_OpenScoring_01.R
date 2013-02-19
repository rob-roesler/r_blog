require(ggplot2) # for visualization
require(pmml) # for storing the model in xml
require(rpart) # use decision trees 

source("OpenScoring_R_Client_01.R")

#-------------------------------------------------------
# Example 1 - categorical target with numeric inputs
#-------------------------------------------------------

# create artificial dataset
set.seed(1234)
gX <- rnorm(100, mean = 0, sd = 0.5)
gY <- rnorm(100, mean = 1, sd = 0.5)
rX <- rnorm(100, mean = 1, sd = 0.5)
rY <- rnorm(100, mean = 0, sd = 0.5)
dataset <- data.frame(X = c(gX, rX), Y = c(gY, rY), Col = c(rep("red", 100), rep("green", 100)))

bp <- ggplot(dataset, aes(X, Y, colour = Col)) + geom_point()
bp + scale_colour_manual(name = "Col", values = c("green", "red"), labels = c("GREEN", "RED"), breaks = c("green", "red"))

# create decision tree model
treeModel <- rpart(Col ~ ., data = dataset)

# export as pmml 
localFilenameAndPath = "/tmp/treeModel_numericalInput_categTarget.xml" # place to store the xml file
saveXML(pmml(treeModel, model.name="TreeModel", app.name="RR/PMML", dataset=dataset),
		file = localFilenameAndPath) # save model using pmml

# prediction
prediction1 <- predictPMMLModel(dataset = dataset, 					
		transformTargetAttribute = factor,
		modelURL = paste("file://",localFilenameAndPath,sep=""),
		applServerURL = "http://localhost:8080/OpenScoring/Scoring")
table(dataset$Col, prediction1) # tabulate results 

#-------------------------------------------------------
# Example 2 - categorical target with mixed inputs
#-------------------------------------------------------

# create artificial dataset
set.seed(1234)
gX <- factor(sample(c("a","b","c"), size = 100, replace = TRUE, prob = c(0.7,0.2,0.1)))
gY <- rnorm(100, mean = 1, sd = 0.5)
rX <- factor(sample(c("a","b","c"), size = 100, replace = TRUE, prob = c(0.1,0.2,0.7)))
rY <- rnorm(100, mean = 0, sd = 0.5)

# http://stackoverflow.com/questions/8229904/r-combining-two-factors
dataset2 <- data.frame(X = unlist(list(gX, rX)), Y = c(gY, rY), 
		Col = c(rep("red", 100), rep("green", 100)))

bp <- ggplot(dataset2, aes(X, Y, colour = Col)) + geom_point()
bp + scale_colour_manual(name = "Col", values = c("green", "red"), labels = c("GREEN", "RED"), breaks = c("green", "red"))

# create decision tree model
treeModel <- rpart(Col ~ ., data = dataset2)

# export as pmml
localFilenameAndPath = "/tmp/treeModel_mixedInput_categTarget.xml"
saveXML(pmml(treeModel, model.name="TreeModel", app.name="RR/PMML", dataset=dataset2),
		file = localFilenameAndPath)
# prediction
prediction2 <- predictPMMLModel(dataset = dataset2, transformTargetAttribute = factor, 
		modelURL = paste("file://",localFilenameAndPath,sep=""),
		applServerURL = "http://localhost:8080/OpenScoring/Scoring")

table(dataset2$Col, prediction2) # tabulate results 

#-----------------------------------------------
# Example 3 - numerical target with mixed input
#-----------------------------------------------

# create artificial dataset
set.seed(1234)
gX <- factor(sample(c("a","b","c"), size = 100, replace = TRUE, prob = c(0.7,0.2,0.1)))
gY <- rnorm(100, mean = 1, sd = 0.5)
rX <- factor(sample(c("a","b","c"), size = 100, replace = TRUE, prob = c(0.1,0.2,0.7)))
rY <- rnorm(100, mean = 0, sd = 0.5)

dataset <- data.frame(X = unlist(list(gX, rX)), Y = c(gY, rY), 
		Col = c(rnorm(100, mean = -5, sd = 1), rnorm(100, mean = 5, sd = 1)))

bp <- ggplot(dataset, aes(X, Y, colour = Col)) + geom_point()
bp

# create decision tree model
treeModel <- rpart(Col ~ ., data = dataset)

# export model as pmml
localFilenameAndPath = "/tmp/treeModel_mixedInput_numTarget.xml"
saveXML(pmml(treeModel, model.name="TreeModel", app.name="RR/PMML", dataset=dataset),
		file = localFilenameAndPath)
# prediction
prediction3 <- predictPMMLModel(dataset = dataset, transformTargetAttribute = as.numeric, 
		modelURL = paste("file://",localFilenameAndPath,sep=""),
		applServerURL = "http://localhost:8080/OpenScoring/Scoring")

modelResults <- data.frame(Y = dataset$Col, Y_hat = prediction3)
cor(modelResults$Y, modelResults$Y_hat)^2 # computing r squared

#-----------------------------------------------
# Example 4 - numerical target with numerical input
#-----------------------------------------------

# create first artificial dataset
set.seed(1234)
gX <- rnorm(100, mean = 0, sd = 0.5)
gY <- rnorm(100, mean = 1, sd = 0.5)
rX <- rnorm(100, mean = 1, sd = 0.5)
rY <- rnorm(100, mean = 0, sd = 0.5)
dataset <- data.frame(X = c(gX, rX), Y = c(gY, rY), Col = c(rnorm(100, mean = -5, sd = 1), rnorm(100, mean = 5, sd = 1)))

bp <- ggplot(dataset, aes(X, Y, colour = Col)) + geom_point()
bp

# create decision tree model
treeModel <- rpart(Col ~ ., data = dataset)

# export model as pmml
localFilenameAndPath = "/tmp/treeModel_numericalInput_numTarget.xml"
saveXML(pmml(treeModel, model.name="TreeModel", app.name="RR/PMML", dataset=dataset),
		file = localFilenameAndPath)
# prediction
prediction4 <- predictPMMLModel(dataset = dataset, transformTargetAttribute = as.numeric, 
		modelURL = paste("file://",localFilenameAndPath,sep=""),
		applServerURL = "http://localhost:8080/OpenScoring/Scoring")

modelResults <- data.frame(Y = dataset$Col, Y_hat = prediction4)
cor(modelResults$Y, modelResults$Y_hat)^2 # computing r squared
