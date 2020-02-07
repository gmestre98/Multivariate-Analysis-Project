####Install and Load Packages
install.packages("corrplot")
install.packages("aricode")
install.packages("caret")
install.packages("aricode")
install.packages("prodlim")
devtools::install_github("pohlio/tidyinftheo")
install.packages("ggsci")
install.packages("rospca")
install.packages("readxl")
install.packages("pcaPP")
install.packages("onehot")
install.packages("mltools")
install.packages("calibrate")
install.packages("klaR")
install.packages("e1071")
install.packages("neuralnet")
install.packages("nnet")
install.packages("VGAM")
install.packages("ROCit")
install.packages("MLmetrics")
install.packages("mclust")
install.packages("MLeval")
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
library(MLmetrics)
library(readxl)
library(corrplot)
library(RColorBrewer)
library(psych) 
library(ggplot2)
library(aricode)
library("tidyinftheo")
library(aricode)
library(caret)
library(plyr)
library(prodlim)
library("ggsci")
library(cluster)
library('NbClust')
library(fpc)
library(mclust)
library(MLeval)

#Read Data
data <- read_excel("Absenteeism_at_work.xls")
data

####### QUESTION 2 -  PRELIMINARY ANALYSIS
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

#Boxplots for the quantitative variables
boxplot(data[,10:15], col=rgb(0.3,0.5,0.4,0.6), las=2,
        names=c("Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet"),
        cex.axis=0.70)
plotti <- data[c(9,17)]
boxplot(plotti, col=rgb(0.3,0.5,0.4,0.6),las=1, ylim = c(0, 400),cex.axis=0.70)


#Divide the dataset considering the classes of absenteeism
absenteeism1<-data[which(data$Absenteeism==1),]
absenteeism2<-data[which(data$Absenteeism==2),]
absenteeism3<-data[which(data$Absenteeism==3),]
absenteeism4<-data[which(data$Absenteeism==4),]

#Outliers in each class of absenteeism

which(absenteeism1$Age %in% boxplot(absenteeism1$Age, plot=FALSE)$out)
which(absenteeism1$`Hit target` %in% boxplot(absenteeism1$`Hit target`, plot=FALSE)$out)
which(absenteeism1$Pet %in% boxplot(absenteeism1$Pet, plot=FALSE)$out)
which(absenteeism1$`Work load Average/day` %in% boxplot(absenteeism1$`Work load Average/day`, plot=FALSE)$out)
which(absenteeism1$`Transportation expense` %in% boxplot(absenteeism1$`Transportation expense`, plot=FALSE)$out)
which(absenteeism2$Age %in% boxplot(absenteeism2$Age, plot=FALSE)$out)
which(absenteeism2$`Service time` %in% boxplot(absenteeism2$`Service time`, plot=FALSE)$out)
which(absenteeism2$`Hit target` %in% boxplot(absenteeism2$`Hit target`, plot=FALSE)$out)
which(absenteeism2$Pet %in% boxplot(absenteeism2$Pet, plot=FALSE)$out)
which(absenteeism2$`Work load Average/day` %in% boxplot(absenteeism2$`Work load Average/day`, plot=FALSE)$out)
which(absenteeism3$`Service time` %in% boxplot(absenteeism3$`Service time`, plot=FALSE)$out)
which(absenteeism3$`Hit target` %in% boxplot(absenteeism3$`Hit target`, plot=FALSE)$out)
which(absenteeism3$Pet %in% boxplot(absenteeism3$Pet, plot=FALSE)$out)
which(absenteeism4$`Work load Average/day` %in% boxplot(absenteeism4$`Work load Average/day`, plot=FALSE)$out)

outliers <- absenteeism2[c(62,64),]
data1 <- data[order(data$Absenteeism),]
data2 <- data[c(207,209),] #corresponds the the two rows present in outliers

data <- data[-c(207,209),]
data <- data[, c(1:15, 17, 16)]

#Correlogram
corvar <- cor(data[9:16])
colnames(corvar) <- c("Transportation Expense", "Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
                      "Work load Average/day")
rownames(corvar) <- c("Transportation Expense", "Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
                      "Work load Average/day")
corrplot(corvar, method="color", col = brewer.pal(n = 8, name = "RdBu"), type="lower", tl.col = "black",
         addCoef.col = "black", tl.srt = 15, tl.cex = 0.6, number.cex = 0.7)


#Mutual information between the variables
mutinf <- mutual_info_matrix(data, 1:17, normalized=TRUE)
mutual_info_heatmap(mutinf, font_sizes = c(9,9))

#removing work distance, transportation expense and service time
data <- data[-c(9:11)]

#Test normality of variables
shapiro.test(data$Age)
shapiro.test(data$`Hit target`)
shapiro.test(data$Son)
shapiro.test(data$Pet)
shapiro.test(data$`Work load Average/day`)

#Categorical variables 
table(data$Absenteeism)
table(data$`Reason for absence`)
table(data$Month)
table(data$`Week day`)
table(data$Seasons)
table(data$`Disciplinary failure`)
table(data$Education)
table(data$`Social drinker`)
table(data$`Social smoker`)

reasons_counts <- table(data$`Reason for absence`)
reasons_counts / sum(reasons_counts)
education_counts <- table(data$Education)
education_counts / sum(education_counts)
smoker_counts <- table(data$`Social smoker`)
smoker_counts / sum(smoker_counts)
absenteeism_counts <- table(data$Absenteeism)
absenteeism_counts / sum(absenteeism_counts)

#remove the variable Disciplinary failure
data <- data[-c(5)]


#Barcharts
customcol <- c("#618685", "#b1cbbb", "#80ced6",  
               "#667292", "#339966", "#00cc99", "#b586ae", "#81377b", "#F1948A")

#Age
ages <- data.frame(table(data$Age, data$Absenteeism))
names(ages) <-c("Age", "Absenteeism", "Count")
ggplot(ages, aes(x=Age, y=Count, fill=Absenteeism))+geom_bar(stat = "identity")+ scale_fill_manual(values = customcol)

#Education
educate <- data.frame(table(data$Absenteeism, data$Education))
names(educate) <-c("Absenteeism", "Education", "Count")
ggplot(educate, aes(x=Absenteeism, y=Count, fill=Education))+geom_bar(stat = "identity")+ scale_fill_manual(values = customcol)

#Social drinker and social smoker
addictions <- data[c(6,7)]

addictions$"Drinker_and_Smoker"= c(0)

addictions[,3]=c(0)
for(i in 1:nrow(addictions)){
  if(addictions[i,1]==1 && addictions[i,2]==1){addictions[i,3]="Social Drinker and Smoker"}
  else if(addictions[i,1]==1 && addictions[i,2]==0 ){addictions[i,3]="Social Drinker"}   
  else if(addictions[i,2]==1 && addictions[i,1]==0){addictions[i,3]="Social Smoker"}   
  else if(addictions[i,1]==0 && addictions[i,2]==0){addictions[i,3]="No drinker/smoker"} 
}
addic <- data.frame(table(data$Absenteeism, addictions$Drinker_and_Smoker))
names(addic) <-c("Absenteeism", "Addictions", "Count")
ggplot(addic, aes(x=Absenteeism, y=Count, fill=Addictions))+geom_bar(stat = "identity")+scale_fill_manual(values = customcol)

table(addictions$Drinker_and_Smoker)/sum(table(addictions$Drinker_and_Smoker))





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
set.seed(123)
#divide dataset into train (70%) and test (30%)
train <- sample(1:694, round(694*.7)) 
test <- scores[c(-train),]
train <- scores[c(train),]
train.classes <- as.factor(train[,17])
test.class <- factor(test[,17], levels=c(1:4))
scores.classes <- as.factor(scores[,17])
n.class=length(table(train[,17]))
table(scores$class)
table(train$class)

#Training control
train.control <- trainControl(method = "cv", number=10, savePredictions = "final")


###Linear Discriminant Analysis
model_lda <- train(class ~., data=train, method = "lda", trControl = train.control)
lda_pred <- predict(model_lda, test[,1:16])
#confusion matrix LDA
lda_pred  <- factor(lda_pred, levels=c(1:4))
confusionMatrix(lda_pred, test.class)
F1_Score(lda_pred, test.class, positive = NULL)


###Regularized Discriminant Analysis 
model_rda <- train(class ~., data=train, method = "rda", trControl = train.control)
rda_pred <- predict(model_rda, test[,1:16])
#confusion matrix LDA
rda_pred  <- factor(rda_pred, levels=c(1:4))
confusionMatrix(rda_pred, test.class)
F1_Score(rda_pred, test.class, positive = NULL)


###Support Vector Machines
model_svm <- train(class ~., data=train, method = "svmRadial", trControl = train.control)
svm_pred <- predict(model_svm, test[,1:16])
#confusion matrix LDA
svm_pred  <- factor(svm_pred, levels=c(1:4))
confusionMatrix(svm_pred, test.class)
F1_Score(svm_pred, test.class, positive = NULL)


###Naive Bayes
model_nb <- train(class ~., data=train, method = "nb", trControl = train.control)
nb_pred <- predict(model_nb, test[,1:16])
#confusion matrix LDA
nb_pred  <- factor(nb_pred, levels=c(1:4))
confusionMatrix(nb_pred, test.class)
F1_Score(nb_pred, test.class, positive = NULL)


###Neural Networks
model_nn <- train(class ~., data=train, method = "nnet", tuneGrid=expand.grid(.size=c(10), .decay=c(0,0)), trControl = train.control)
nn_pred <- predict(model_nn, test[,1:16])
#confusion matrix LDA
nn_pred  <- factor(nn_pred, levels=c(1:4))
confusionMatrix(nn_pred, test.class)
F1_Score(nn_pred, test.class, positive = NULL)


###Random Forest
model_rf <- train(class ~., data=train, method = "rf", trControl = train.control)
rf_pred <- predict(model_rf, test[,1:16])
#confusion matrix LDA
rf_pred  <- factor(rf_pred, levels=c(1:4))
confusionMatrix(rf_pred, test.class)
F1_Score(rf_pred, test.class, positive = NULL)


###K-Nearest Neighbour 
knnFit <- train(class ~ ., data = train, method = "knn", trControl = train.control, preProcess = c("center","scale"),tuneLength = 20)
knnFit
plot(knnFit)
model_rf <- train(class ~., data=train, method = "knn",tuneGrid=expand.grid(.k=11), trControl = train.control)
rf_pred <- predict(model_rf, test[,1:16])
#confusion matrix LDA
rf_pred  <- factor(rf_pred, levels=c(1:4))
confusionMatrix(rf_pred, test.class)
F1_Score(rf_pred, test.class, positive = NULL)



####### QUESTION 4 -  CLUSTER ANALYSIS

datamat <- matrix(unlist(data), ncol = 13, byrow = FALSE)

################################################# GOWER ##############################################
########USING GOWER METRIC 
dist_g <- daisy(data[,1:12], metric="gower")

NbClust(diss=dist_g, distance=NULL, min.nc=2, max.nc=10, method="single", index="silhouette") #2-0.1915
#Cluster using Single Linkage algorithm
single_g <- agnes(dist_g, diss=TRUE, method="single", keep.diss=FALSE)
pltree(single_g, main="Single Linkage", cex=0.83, xlab="Data", ylab="Height")
c_single_g<-cutree(single_g, 2)
table(c_single_g, datamat[,13])


NbClust(diss=dist_g, distance=NULL, min.nc=2, max.nc=10, method="complete", index="silhouette") #2-0.2313
#Cluster using Complete Linkage algorithm
complete_g <- agnes(dist_g, diss=TRUE, method="complete", keep.diss=FALSE)
pltree(complete_g, main="Complete Linkage - Gower", cex=0.83, xlab="Data", ylab="Height")
c_complete_g <- cutree(complete_g, 2)
table(c_complete_g, datamat[,13])


NbClust(diss=dist_g, distance=NULL, min.nc=2, max.nc=10, method="average", index="silhouette") #2-0.2341
#Cluster using Average Linkage algorithm
average_g <- agnes(dist_g, diss=TRUE, method="average", keep.diss=FALSE)
pltree(average_g, main="Average Linkage - Gower", cex=0.83, xlab="Data", ylab="Height")
c_average_g <- cutree(average_g, 2)
table(c_average_g, datamat[,13])


NbClust(diss=dist_g, distance=NULL, min.nc=2, max.nc=10, method="ward.D", index="silhouette") #10-0.177
#Cluster using Ward's Method algorithm
ward_g <- agnes(dist_g, diss=TRUE, method="ward", keep.diss=FALSE)
pltree(ward_g, main="Ward Method - Gower", cex=0.83, xlab="Data", ylab="Height")
c_ward_g <- cutree(ward_g, 6)
table(c_ward_g, datamat[,13])

#Using Partitioning Around Medoids
pam_g<-pamk(dist_g, k=2:15, diss=TRUE, usepam=TRUE)
table(pam_g$pamobject$cluster,datamat[,13])

dbcan <- dbscan(dist_g, eps = 0.15, MinPts = 5, method="dist")
table(dbcan$cluster, datamat[,13])


##########################################################################################################
##########################################################################################################
#################################################MDS and PCA##############################################
##########################################################################################################
##########################################################################################################
##########USING MDS FOR CATEGORICAL VARIABLES
data.ponehot = onehot(data[c(1:7)], max_levels=60)
data.onehot = predict(data.ponehot, data[c(1:7)])
dist<-hamming.distance(data.onehot)
mds <- cmdscale(dist, k=12,eig=TRUE) 
plot(mds$eig, 
     type="h", lwd=5, las=1, 
     xlab="Number of dimensions", 
     ylab="Eigenvalues")
abline(v =11, untf = FALSE, col="blue")
scores.mds <- mds$points

##########USING PCA FOR QUANTITATIVE VARIABLES
data.pca <- prcomp(data[,c(8:12)], scale. = T, retx=TRUE) 
summary(data.pca) 
#deciding how many PC we should use
screeplot(data.pca, npcs = 5, main="", type="lines") 
data.loadings <- data.pca$sdev^2
sum(data.loadings[1:4])/sum(data.loadings) #4 PC to keep - 0.8595111
data.pca[["rotation"]]

#dataset we are going to use for classification (scores)
scores.pca <- data.pca$x #from PCA
scores.pca <- scores.pca[,c(1:4)]
scores <- as.data.frame(cbind(scores.mds,scores.pca,data[,13]))
names(scores)[17] <- "class"



##########################################################################################################
##########################################################################################################
#####################################CLUSTER ANALYSIS -  EUCLIDEAN########################################
##########################################################################################################
##########################################################################################################

########USING EUCLIDEAN METRIC 
dist_eu <- daisy(scores[,1:16], metric="euclidean")

NbClust(diss=dist_eu, distance=NULL, min.nc=2, max.nc=10, method="single", index="silhouette") #2-0.2811
#Cluster using Single Linkage algorithm
single_eu <- agnes(dist_eu, diss=TRUE, method="single", keep.diss=FALSE)
pltree(single_eu, main="Single Linkage - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_single_eu<-cutree(single_eu, 2)
table(c_single_eu, scores[,17])


NbClust(diss=dist_eu, distance=NULL, min.nc=2, max.nc=10, method="complete", index="silhouette") #2-0.1504
#Cluster using Complete Linkage algorithm
complete_eu <- agnes(dist_eu, diss=TRUE, method="complete", keep.diss=FALSE)
pltree(complete_eu, main="Complete Linkage - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_complete_eu <- cutree(complete_eu, 2)
table(c_complete_eu, scores[,17])


NbClust(diss=dist_eu, distance=NULL, min.nc=2, max.nc=10, method="average", index="silhouette") #2-0.2574
#Cluster using Average Linkage algorithm
average_eu <- agnes(dist_eu, diss=TRUE, method="average", keep.diss=FALSE)
pltree(average_eu, main="Average Linkage - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_average_eu <- cutree(average_eu, 2)
table(c_average_eu, scores[,17])


NbClust(diss=dist_eu, distance=NULL, min.nc=2, max.nc=10, method="ward.D", index="silhouette") #9-0.2004
#Cluster using Ward's Method algorithm
ward_eu <- agnes(dist_eu, diss=TRUE, method="ward", keep.diss=FALSE)
pltree(ward_eu, main="Ward Method - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_ward_eu <- cutree(ward_eu, 9)
table(c_ward_eu, scores[,17])

#Using Partitioning Around Medoids
pam_eu<-pamk(dist_eu, k=2:15, diss=TRUE, usepam=TRUE)
table(pam_eu$pamobject$cluster,scores[,17])

dbcan_eu <- dbscan(dist_eu, eps = 0.15, MinPts = 2, method="dist")
table(dbcan_eu$cluster, scores[,17])

normal.mod <- Mclust(scores[,1:16])
table(normal.mod$classification, scores[,17])


#summary(silhouette(c_single_g, dist_g))
summary(silhouette(c_complete_g, dist_g))
summary(silhouette(c_average_g, dist_g))
summary(silhouette(c_ward_g, dist_g))
summary(silhouette(pam_g$pamobject$cluster, dist_g))
summary(silhouette(dbcan$cluster, dist_g))
#summary(silhouette(c_single_eu, dist_eu))
summary(silhouette(c_complete_eu, dist_eu))
summary(silhouette(c_average_eu, dist_eu))
summary(silhouette(c_ward_eu, dist_eu))
summary(silhouette(pam_eu$pamobject$cluster, dist_eu))
#summary(silhouette(dbcan_eu$cluster, dist_eu))
summary(silhouette(normal.mod$classification, dist_eu))


result = c_average_eu

c1 = data[which(result == 1),]
c2 = data[which(result == 2),]
summary(c1)
summary(c2)

##Obtained clusters
result

###### QUESTION 5 -- CLASSIFICATION INTO OBTAINED CLUSTERS
cols.dont.want <- "Absenteeism"
data <- data[, ! names(data) %in% cols.dont.want, drop = F]
data["cluster"]<- NA
data$cluster<-as.factor(result)
str(data)

#the column regarding ID wasn´t considered
#Abseentism column is no longer used, in this study they are no longer the classes. 
#The values of 'cluster' are the new classes.

#PRE-ANALYSIS
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
scores
scores <- as.data.frame(cbind(scores.mds,scores.pca,data[,13]))
names(scores)[17] <- "class"

####CLASSIFICATION
set.seed(123)
#divide dataset into train (70%) and test (30%)
train <- sample(1:694, round(694*.7)) 
test <- scores[c(-train),]
train <- scores[c(train),]
train.classes <- as.factor(train[,17])
test.class <- factor(test[,17], levels=c(1:2))
scores.classes <- as.factor(scores[,17])
n.class=length(table(train[,17]))
table(scores$class)
table(train$class)
table(test$class)
#Training control
train.control <- trainControl(method = "cv", number=10, savePredictions = "final")


###Linear Discriminant Analysis
model_lda <- train(class ~., data=train, method = "lda", trControl = train.control)
lda_pred <- predict(model_lda, test[,1:16])
#confusion matrix LDA
lda_pred  <- factor(lda_pred, levels=c(1,2))
confusionMatrix(lda_pred, test.class)
F1_Score(lda_pred, test.class, positive = NULL)


###Regularized Discriminant Analysis 
model_rda <- train(class ~., data=train, method = "rda", trControl = train.control)
rda_pred <- predict(model_rda, test[,1:16])
#confusion matrix LDA
rda_pred  <- factor(rda_pred, levels=c(1,2))
confusionMatrix(rda_pred, test.class)
F1_Score(rda_pred, test.class, positive = NULL)


###Support Vector Machines
model_svm <- train(class ~., data=train, method = "svmRadial", trControl = train.control)
svm_pred <- predict(model_svm, test[,1:16])
#confusion matrix LDA
svm_pred  <- factor(svm_pred, levels=c(1,2))
confusionMatrix(svm_pred, test.class)
F1_Score(svm_pred, test.class, positive = NULL)


###Naive Bayes
model_nb <- train(class ~., data=train, method = "nb", trControl = train.control)
nb_pred <- predict(model_nb, test[,1:16])
#confusion matrix LDA
nb_pred  <- factor(nb_pred, levels=c(1,2))
confusionMatrix(nb_pred, test.class)
F1_Score(nb_pred, test.class, positive = NULL)


###Neural Networks
model_nn <- train(class ~., data=train, method = "nnet", tuneGrid=expand.grid(.size=c(10), .decay=c(0,0)), trControl = train.control)
nn_pred <- predict(model_nn, test[,1:16])
#confusion matrix LDA
nn_pred  <- factor(nn_pred, levels=c(1,2))
confusionMatrix(nn_pred, test.class)
F1_Score(nn_pred, test.class, positive = NULL)


###Random Forest
model_rf <- train(class ~., data=train, method = "rf", trControl = train.control)
rf_pred <- predict(model_rf, test[,1:16])
#confusion matrix LDA
rf_pred  <- factor(rf_pred, levels=c(1,2))
confusionMatrix(rf_pred, test.class)
F1_Score(rf_pred, test.class, positive = NULL)


###K-Nearest Neighbour 
knnFit <- train(class ~ ., data = train, method = "knn", trControl = train.control, preProcess = c("center","scale"),tuneLength = 20)
knnFit
plot(knnFit)
model_rf <- train(class ~., data=train, method = "knn",tuneGrid=expand.grid(.k=11), trControl = train.control)
rf_pred <- predict(model_rf, test[,1:16])
#confusion matrix LDA
rf_pred  <- factor(rf_pred, levels=c(1,2))
confusionMatrix(rf_pred, test.class)
F1_Score(rf_pred, test.class, positive = NULL)

