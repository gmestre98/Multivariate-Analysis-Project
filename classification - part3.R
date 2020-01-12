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
#install.packages("ROCit")
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
library(ROCit)

######PARTE DA XANA
#Read Data
data <- read_excel("Absenteeism_at_work.xls")
data

#Set categorical variables
data$ID <- as.factor(data$ID)
data$Seasons <- as.factor(data$Seasons)
data$`Month of absence` <- as.factor(data$`Month of absence`)
data$`Day of the week` <- as.factor(data$`Day of the week`)
data$`Disciplinary failure` <- as.factor(data$`Disciplinary failure`)
data$Education <- as.factor(data$Education)
data$`Social drinker` <- as.factor(data$`Social drinker`)
data$`Social smoker` <- as.factor(data$`Social smoker`)

#Order columns by type
data=data[,c(1:5,12,13,15,16,6:9,11,14,17:21,10)]

#Rename columns
names(data)[3] <- "Month"
names(data)[4] <- "Week day"
names(data)[11] <- "Work Distance"
names(data)[19] <- "BMI"
names(data)[20] <- "Absenteeism"

#remove ID, Weight, Height and BMI
data=data[,c(2:16,20:21)]

#remove rows with missing date for variable Absenteeism
data <- data[-which(data$Absenteeism==0,),]

#changing work load average/day column
i=1;
while(i<=dim(data[,1])[1]){data[i,17]=round(data[i,17]/1000);i=i+1} 

#changing absenteeism column
i=1;
while(i<=dim(data[,1])[1]){if(1<=data[i,16] && data[i,16]<8){data[i,16]=1} 
  else if(data[i,16]==8){data[i,16]=2}
  else if(data[i,16]>8 && data[i,16]<=40){data[i,16]=3}
  else if(data[i,16]>40){data[i,16]=4}
  ;i=i+1}
data$Absenteeism <- as.factor(data$Absenteeism)

#changing reason for absence column
i=1;
while(i<=dim(data[,1])[1]){if(data[i,1]>=1 && data[i,1]<=21){data[i,1]=1}
  else if(data[i,1]==22){data[i,1]=2}   
  else if(data[i,1]==23){data[i,1]=3} 
  else if(data[i,1]==24){data[i,1]=4} 
  else if(data[i,1]==25){data[i,1]=5}   
  else if(data[i,1]==26){data[i,1]=6} 
  else if(data[i,1]==27){data[i,1]=7} 
  else if(data[i,1]==28){data[i,1]=8}
  ;i=i+1}
data$`Reason for absence` <- as.factor(data$`Reason for absence`)

#removing outliers
data <- data[-c(207,209),]
data <- data[, c(1:15, 17, 16)]
#removing work distance, transportation expense and service time
data <- data[-c(9:11)]
#remove the variable Disciplinary failure
data <- data[-c(5)]





####### QUESTION 3 - CLASSIFICATION
####PRE-ANALYSIS
#using multidimensional scaling (MDS) for categorical variables
#one hot encoding for Hamming distance
data.ponehot <- onehot(data[c(1:7)], max_levels = 60)
data.onehot <- predict(data.ponehot,data[c(1:7)])
#dist <- dist(data.onehot, method="hamming")
dist <- hamming.distance(data.onehot)
mds <- cmdscale(dist, k=12,eig=TRUE) 
plot(mds$eig, 
     type="h", lwd=5, las=1, 
     xlab="Number of dimensions", 
     ylab="Eigenvalues")
abline(v =12, untf = FALSE, col="blue")
scores.mds <- mds$points


#using quantitative variables for PCA - standardized
data.pca <- prcomp(data[,c(8:12)], scale. = T, retx=TRUE) 
summary(data.pca) 
#deciding how many PC we should use
screeplot(data.pca, npcs = 5, main="", type="lines") 
data.loadings <- data.pca$sdev^2
sum(data.loadings[1:4])/sum(data.loadings) #4 PC to keep - 0.8637267
data.pca[["rotation"]]

#dataset we are going to use for classification (scores)
scores.pca <- data.pca$x #from PCA
scores.pca <- scores.pca[,c(1:4)]
scores <- as.data.frame(cbind(scores.mds,scores.pca,data[,13]))
names(scores)[17] <- "class"

####CLASSIFICATION
#divide dataset into train (70%) and test (30%)
train <- sample(1:694, round(694*.7)) 
test <- scores[c(-train),]
train <- scores[c(train),]
train.classes <- as.factor(train[,17])
scores.classes <- as.factor(scores[,17])
n.class=length(table(train[,17]))
table(scores$class)
table(train$class)


###Linear Discriminant Analysis
z <- lda(train[,1:16], grouping=train.classes, prior=ones(n.class,1)/n.class, CV=FALSE)
zt<-predict(z,test[,1:16], type = "class")
#confusion matrix LDA
zt.pred <- factor(zt$class, levels=c(1:4))
test.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(zt.pred,test.class)
##leave-one-out
z2 <- lda(scores[,1:16], grouping=scores.classes, prior=ones(4,1)/4, CV=TRUE)
#confusion matrix LDA loo
z2.pred <- factor(z2$class, levels=c(1:4))
test2.class <- factor(scores[,17], levels=c(1:4))
confusionMatrix(z2.pred,test2.class)

###Logistic Regression 
zlr <- nnet::multinom(class ~., data = train)
ztlr<-predict(zlr,test[,1:16], type = "class")
#confusion matrix logistic regression
ztlr.pred <- factor(ztlr, levels=c(1:4))
testlr.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(ztlr.pred,testlr.class)

###Regularized Discriminant Analysis 
zr <- rda(train[,1:16], grouping=train.classes, prior=ones(n.class,1)/n.class, CV=FALSE)
ztr<-predict(zr,test[,1:16], type = "class")
#confusion matrix RDA
ztr.pred <- factor(ztr$class, levels=c(1:4))
testr.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(ztr.pred,testr.class)

###Support Vector Machines
zsvm <- svm(train[,1:16],train.classes,type = "C-classification")
ztsvm <- predict(zsvm,test[,1:16], type = "class")
#confusion matrix svm
ztsvm.pred <- factor(ztsvm, levels=c(1:4))
testsvm.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(ztsvm.pred,testsvm.class)

###Naive Bayes Classifier
znb <- naiveBayes(train[,1:16],train.classes)
ztnb <- predict(znb,test[,1:16], type = "class")
#confusion matrix naive bayes
ztnb.pred <- factor(ztnb, levels=c(1:4))
testnb.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(ztnb.pred,testnb.class)

###Neural Networks
znn <- nnet(class ~ ., data=train, size=5)
ztnn <- predict(znn,test[,1:16], type = "class")
#confusion matrix svm
ztnn.pred <- factor(ztnn, levels=c(1:4))
testnn.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(ztnn.pred,testnn.class)

###Random Forest
zrf <- randomForest(train[,1:16],train.classes)
ztrf <- predict(zrf,test[,1:16], type = "class")
#confusion matrix random forest
ztrf.pred <- factor(ztrf, levels=c(1:4))
testrf.class <- factor(test[,17], levels=c(1:4))
confusionMatrix(ztrf.pred,testrf.class)
