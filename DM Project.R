library(readr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)



#Reading the .csv file
system("ls F:/Study/RStudioWorkspace")
mushrooms= read.csv("F:/Study/RStudioWorkspace/mushrooms.csv")
#Summary of the data
summary(mushrooms)
#Getting the domain of the attributes belonging to mushroom dataset
attributes_classes= cbind.data.frame(Var=names(mushrooms), Total_Class=sapply(mushrooms, function(x){as.numeric(length(levels(x)))}))
#Printing
print(attributes_classes)

#Plotting and visualizing the mushrooom data
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(class))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(cap.shape))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(cap.color))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(bruises))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(odor))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(gill.attachment))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(gill.spacing))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(gill.size))
qplot(factor(class),data=mushrooms,geom = "bar",fill = factor(gill.color))



#Data Processing

#Showing there is no missing value in the dataset.
sum(complete.cases(mushrooms))

#Dropping the attribute veil.type as it has only one class and thus has no effect in 
#determining the class of the mushroom
mushrooms$veil.type=NULL

#Splitting the data in training and testing
split_ratio=0.7
training_examples <- floor(split_ratio * nrow(mushrooms))

# set the seed to make your partition reproductible
set.seed(1234)

training_indices <- sample(seq_len(nrow(mushrooms)), size = training_examples)

train <- mushrooms[training_indices, ]
test <- mushrooms[-training_indices, ]


#Training the Decision Tree algo
dtmodel=rpart(class~.,data = train, method = "class")
summary(dtmodel)

#Plotting the tree
win.graph(800,600,10)
plot(dtmodel,uniform = TRUE,compress = TRUE,margin = .2)
text(dtmodel,use.n = TRUE,all=TRUE,fancy = TRUE)
rpart.plot(dtmodel)

#training Random Forest
randomFor=randomForest(class~., data = train,ntree=50)
help("randomForest")
print(randomFor)

win.graph(800,600,10)
plot(randomFor,uniform = TRUE,compress = TRUE,margin = .2)

#Evaluation
#Desicion Tree
dt_predict=predict(dtmodel,newdata = test,type = "class")

print(dt_predict)

#Random Forest
rf_predict=predict(randomFor,newdata = test,type = "class")
print(rf_predict)
