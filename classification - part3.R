#Install and Load Packages
#install.packages("corrplot")
#install.packages("rospca")
#install.packages("readxl")
#install.packages("pcaPP")
#install.packages("onehot")
#install.packages("mltools")
#install.packages("calibrate")
#install.packages("klaR")
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("neuralnet")
#install.packages("nnet")
#install.packages("VGAM")
library(readxl)
library(rospca)
library(pcaPP)
library(corrplot)
library(RColorBrewer)
library(psych) 
library(data.table)
library(onehot)
library(mltools)
library(calibrate)
library(MASS)
library(caret)
library(klaR)
library(e1071)
library(randomForest)
library(neuralnet)
library(nnet)
library(VGAM)

#Read Data
data <- read_excel("Absenteeism_at_work.xls")
data

#Set categorical variables
data$ID <- as.factor(data$ID)
data$Seasons <- as.factor(data$Seasons)
data$`Reason for absence` <- as.factor(data$`Reason for absence`)
data$`Month of absence` <- as.factor(data$`Month of absence`)
data$`Day of the week` <- as.factor(data$`Day of the week`)
data$`Disciplinary failure` <- as.factor(data$`Disciplinary failure`)
data$Education <- as.factor(data$Education)
data$`Social drinker` <- as.factor(data$`Social drinker`)
data$`Social smoker` <- as.factor(data$`Social smoker`)

#Order columns by type
data=data[,c(1:5,12,13,15,16,6:9,11,14,17:21,10)]

#Rename columns
names(data)[11] <- "Work Distance"
names(data)[19] <- "BMI"
names(data)[20] <- "Absenteeism"
names(data)[3] <- "Month"
names(data)[4] <- "Week day"



#QUESTION 3 - CLASSIFICATION

#i didn't consider column 1 - ID


#using all variable for PCA
#one hot encoding (solving categorical variables' problem)
data.ponehot <- onehot(data[c(2:19,21)], max_levels = 60)
data.onehot <- predict(data.ponehot,data[c(2:19,21)])
#PCA for all variables (20) standardized
data.pca <- prcomp(data.onehot, scale. = T, retx=TRUE) 
summary(data.pca) 
#deciding how many PC we should use
screeplot(data.pca, npcs = 71, main="", type="lines") 
abline(h=mean(data.pca$sdev^2),col="green") #VEEEEER
data.loadings <- data.pca$sdev^2
sum(data.loadings[1:37])/sum(data.loadings) #37 PC to keep - 0.8084056
#dataset we are going to use for classification (scores)
scoresp <- data.pca$x
scoresp <- scoresp[,c(1:37)]
scoresp <- as.data.frame(scoresp)
scores <- as.data.frame(cbind(scoresp[,1:37],class=data[,20]))
names(scores)[38] <- "class"
#CLASSIFICATION
#divide dataset into train (70%) and test (30%)
set.seed(1)
train <- sample(1:740, round(740*.7)) 
test <- scores[c(-train),]
train <- scores[c(train),]
train.classes <- as.factor(train[,38])
scores.classes <- as.factor(scores[,38])
n.class=length(table(train[,38]))

### Linear Discriminant Analysis
z <- lda(train[,1:37], grouping=train.classes, prior=ones(n.class,1)/n.class, CV=FALSE)
zt<-predict(z,test[,1:37])
#confusion matrix LDA
zt.pred <- factor(zt$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zt.pred,test.class)
##leave-one-out
z2 <- lda(scores[,1:37], grouping=scores.classes, prior=ones(19,1)/19, CV=TRUE)
#confusion matrix LDA loo
z2.pred <- factor(z2$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test2.class <- factor(scores[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(z2.pred,test2.class)

### Logistic Regression 
zlr <- nnet::multinom(class ~., data = train)
ztlr<-predict(zlr,test[,1:37])
#confusion matrix logistic regression
ztlr.pred <- factor(ztlr, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testlr.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(ztlr.pred,testlr.class)

#EEEERRRRRRRRRRRRRROOOOOOhelp
#Regularized Discriminant Analysis 
zr <- rda(train[,1:37], grouping=train.classes, prior=ones(n.class,1)/n.class, CV=FALSE)
ztr<-predict(zr,test[,1:37])
#confusion matrix RDA
ztr.pred <- factor(ztr$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testr.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(ztr.pred,testr.class)
##leave-one-out
z2r <- rda(scores[,1:37], grouping=scores.classes, prior=ones(19,1)/19, CV=TRUE)
#confusion matrix RDA loo
z2r.pred <- factor(z2r$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test2r.class <- factor(scores[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(z2r.pred,test2r.class)

###Support Vector Machines
zsvm <- svm(train[,1:37],train.classes,type = "C-classification")
ztsvm <- predict(zsvm,test[,1:37])
#confusion matrix svm
ztsvm.pred <- factor(ztsvm, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testsvm.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(ztsvm.pred,testsvm.class)

###Naive Bayes Classifier
znb <- svm(train[,1:37],train.classes,type = "C-classification")
ztnb <- predict(znb,test[,1:37])
#confusion matrix naive bayes
ztnb.pred <- factor(ztnb, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testnb.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(ztnb.pred,testnb.class)

##EEEEERRRRRROOOOOOO
#Neural Networks
znn <- nnet(class ~ ., data=train, size=15)
ztnn <- predict(znn,test[,1:37])
#confusion matrix svm
ztnn.pred <- factor(ztnn, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testnn.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(ztnn.pred,testnn.class)

###Random Forest
zrf <- randomForest(train[,1:37],train.classes)
ztrf <- predict(zrf,test[,1:37])
#confusion matrix random forest
ztrf.pred <- factor(ztrf, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testrf.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(ztrf.pred,testrf.class)





#using only quantitative varibles for PCA
#PCA for quantitative variables (9) standardized
data.pca2 <- prcomp(data[,c(10:19,21)], scale. = T, retx=TRUE)
summary(data.pca2) 
screeplot(data.pca2, main="", type="lines") 
abline(h=mean(data.pca2$sdev^2),col="green") 
data.loadings2 <- data.pca2$sdev^2
sum(data.loadings2[1:6])/sum(data.loadings2) #6 PC to keep - 0.8300326
#dataset we are going to use for classification (scores and categorical variables from original dataset)
scoresp2 <- data.pca2$x
scoresp2 <- scoresp2[,c(1:6)]
data.ponehot2 <- onehot(data[c(2:9)], max_levels = 60)
data.onehot2 <- predict(data.ponehot2,data[c(2:9)])
scores2 <- cbind(scoresp2,data.onehot2)
scores2 <- as.data.frame(scores2)
scores2 <- as.data.frame(cbind(scores2,class=data[,20]))
names(scores2)[67] <- "class"

#CLASSIFICATION
#divide dataset into train (70%) and test (30%)
set.seed(1)
train2 <- sample(1:740, round(740*.7)) 
test2 <- scores2[c(-train2),]
train2 <- scores2[c(train2),]
train2.classes <- as.factor(train2[,67])
scores2.classes <- as.factor(scores2[,67])
n.class2=length(table(train2[,67]))
#removing constant columns
train2 <- train2[,!apply(train2, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
n.columns <- length(train2)-1
#removing the same columns from testing data
test2 <- test2[colnames(train2)]

### Linear Discriminant Analysis
zz <- lda(train2[,1:n.columns], grouping=train2.classes, prior=ones(n.class2,1)/n.class2, CV=FALSE)
zzt<-predict(zz,test2[,1:n.columns])
#confusion matrix LDA
zzt.pred <- factor(zzt$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test2.class <- factor(test2[,n.columns+1], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zzt.pred,test2.class)
##leave-one-out
zz2 <- lda(scores2[,1:66], grouping=scores2.classes, prior=ones(19,1)/19, CV=TRUE)
#confusion matrix LDA loo
zz2.pred <- factor(zz2$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test22.class <- factor(scores2[,67], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zz2.pred,test22.class)

### Logistic Regression 
zzlr <- nnet::multinom(class ~., data = train2)
zztlr<-predict(zzlr,test2[,1:14])
#confusion matrix logistic regression
zztlr.pred <- factor(zztlr, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testlr2.class <- factor(test2[,15], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zztlr.pred,testlr2.class)

#EEEERRRRRRRRRRRRRROOOOOOhelp
#Regularized Discriminant Analysis 
zzr <- rda(train2[,1:n.columns], grouping=train2.classes, prior=ones(n.class2,1)/n.class2, CV=FALSE)
zztr<-predict(zzr,test2[,1:n.columns])
#confusion matrix RDA
zztr.pred <- factor(zztr$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testr2.class <- factor(test2[,n.columns+1], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zztr.pred,testr2.class)
##leave-one-out
zz2r <- rda(scores2[,1:66], grouping=scores2.classes, prior=ones(19,1)/19, CV=TRUE)
#confusion matrix RDA loo
zz2r.pred <- factor(zz2r$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test2r2.class <- factor(scores2[,67], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zz2r.pred,test2r2.class)

###Support Vector Machines
zzsvm <- svm(train2[,1:n.columns],train2.classes,type = "C-classification")
zztsvm <- predict(zzsvm,test2[,1:n.columns])
#confusion matrix svm
zztsvm.pred <- factor(zztsvm, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testsvm2.class <- factor(test2[,n.columns+1], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zztsvm.pred,testsvm2.class)

###Naive Bayes Classifier
zznb <- svm(train2[,1:n.columns],train2.classes,type = "C-classification")
zztnb <- predict(zznb,test2[,1:n.columns])
#confusion matrix naive bayes
zztnb.pred <- factor(zztnb, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testnb2.class <- factor(test2[,n.columns+1], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zztnb.pred,testnb2.class)

##EEEEERRRRRROOOOOOO
#Neural Networks
zznn <- nnet(class ~ ., data=train2, size=15)
zztnn <- predict(zznn,test2[,1:n.columns])
#confusion matrix svm
zztnn.pred <- factor(zztnn, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testnn2.class <- factor(test2[,n.columns+1], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zztnn.pred,testnn2.class)

###Random Forest
zzrf <- randomForest(train2[,1:n.columns],train2.classes)
zztrf <- predict(zzrf,test2[,1:n.columns])
#confusion matrix random forest
zztrf.pred <- factor(zztrf, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
testrf2.class <- factor(test2[,n.columns+1], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zztrf.pred,testrf2.class)
