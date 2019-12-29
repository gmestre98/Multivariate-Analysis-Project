#Install and Load Packages
#install.packages("corrplot")
#install.packages("rospca")
#install.packages("readxl")
#install.packages("pcaPP")
#install.packages("onehot")
#install.packages("mltools")
#install.packages("calibrate")
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
train <- sample(1:740, round(740*.8)) 
test <- scores[c(-train),]
train <- scores[c(train),]
class <- as.factor(train[,38])
n.class=length(table(train[,38]))

### Linear Discriminant Analysis
z <- lda(train[,1:37], grouping=train[,38], prior=ones(n.class,1)/n.class, CV=FALSE)
zt<-predict(z,test[,1:37],prior=as.vector(ones(n.class,1)/n.class))
#confusion matrix
zt.pred <- factor(zt$class, levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
test.class <- factor(test[,38], levels=c(0:5,7:8,16,24,32,40,48,56,64,80,104,112,120))
confusionMatrix(zt.pred,test.class)





#using only quantitative varibles for PCA
#PCA for quantitative variables (9) standardized
data.pca2 <- prcomp(data[,c(10:19,21)], scale. = T, retx=TRUE)
summary(data.pca2) 
screeplot(data.pca2, main="", type="lines") 
abline(h=mean(data.pca2$sdev^2),col="green") 
data.loadings2 <- data.pca2$sdev^2
sum(data.loadings2[1:6])/sum(data.loadings2) #6 PC to keep - 0.8300326
#dataset we are going to use for classification (scores and categorical variables from original dataset)
scoresp <- data.pca2$x
scoresp <- scoresp[,c(1:6)]
datamatrixp <- data[,c(2:9)]
scores2 <- cbind(scoresp,datamatrixp)


