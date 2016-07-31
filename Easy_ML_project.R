install.packages("caret")

library(caret)
 
data("iris")
dataset <- iris
#renamed iris to dataset to keep consistent between projects 
#split the loaded dataset 70% and 30% to train and validate our models
#correct typo of p=0.70m to p=0.70,
validation_index <- createDataPartition(dataset$Species, p=0.70, list=FALSE)
validation <- dataset[-validation_index]
dataset <- dataset[validation_index]
#currently the validation_index is 70% of the dataset, the variable validation is the remaining 30% of the dataset, and in the end the dataset variable was reassigned to represent the 70% and/or validation_index
dim(dataset)
NULL
#an error occurred when trying to get the dimensions of the dataset. We will go back and see why the dataset is appearing as a NULL
data("iris")
dataset <- iris
validation_index <- createDataPartition(dataset$Species, p=0.70, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]
dim(dataset)

#it turns out that the error was a simple one, but a crucial one. The comma was left out of the variable assignments involving valdiation_index above
sapply(dataset, class)
 
head(dataset)

#want to look at the dimensions of the dataset, the type of class, and what a small sample of the data looks like
levels(dataset$Species)
#let us look at the percentage of rows that belong to each class
percentage <- prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species), percentage=percentage)

summary(dataset)

#we should visualise the dataset to learn more about it
x <- dataset[,1:4]
y <- dataset[,5]
par(mfrow=c(1,4))
for(i in 1:4) {}
for(i in 1:4) {boxplot(x[,i], main=names(iris)[i])}
plot(y)
#see that instances(rows) are evenly distributed across the classes
#now for a scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
#looks like the ellipse package was not installed.
install.packages("ellipse")

featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

#let us do some density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#we should begin to evaluate some algorithms
#to estimate accuracy we can do a 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#let us try linear algorithms
#LDA - linear discriminant analysis
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
#try two nonlinear algorithms
#CART - classification and regression trees
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
#kNN - k-Nearest Neighbour
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
#try two advanced algorithms
#SVM - support vector machines
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
#RF - Random forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#Now we should try to select the best model.
#We can look at the accuracy of each model by creating a list and use the summary function
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))

#Let us create a plot of the model evaluation results to compare the spread and accuracy
#compare accuracy of models
dotplot(results)

#the LDA is the most accurate, we should summarize it
print(fit.lda)

#compare to validation set to check if accuracy is true and not overfit
#run the model on the validation set and summarize
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
