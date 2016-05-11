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