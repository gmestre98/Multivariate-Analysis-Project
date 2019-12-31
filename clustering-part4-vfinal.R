#install.packages("readxl")
#install.packages("onehot")
#install.packages("cluster")
#install.packages("NbClust")
#install.packages("fpc")
library(readxl)
library(onehot)
library(cluster)
library('NbClust')
library(fpc)
library(clusterCrit)
library(mclust)


##########################################################################################################
##########################################################################################################
###########################################DATA PRE PROCESSING############################################
##########################################################################################################
##########################################################################################################
#Read Data
data <- read_excel("Absenteeism_at_work.xls")

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
data=data[,c(1:5,12:13,15:16,6:11,14,17:21)]

#Rename columns
names(data)[3] <- "Month"
names(data)[4] <- "Week day"
names(data)[11] <- "Work Distance"
names(data)[20] <- "BMI"
names(data)[21] <- "Absenteeism"

#Get data as number matrix instead of a list
datamat <- matrix(unlist(data), ncol = 21, byrow = FALSE)

##########################################################################################################
##########################################################################################################
###############################CLUSTERING WITH GOWER DISTANCE AS METRIC###################################
##########################################################################################################
##########################################################################################################
#Get dissimilarity matrix
dissdata_g <- daisy(data[,2:20], metric="gower")

NbClust(diss=dissdata_g, distance=NULL, min.nc=2, max.nc=15, method="single", index="silhouette") #2-0.2158
#Cluster using Single Linkage algorithm
single_g <- agnes(dissdata_g, diss=TRUE, method="single", keep.diss=FALSE)
pltree(single_g, main="Single Linkage", cex=0.83, xlab="Data", ylab="Height")
c_single_g<-cutree(single_g, 2)
table(c_single_g, datamat[,21])


NbClust(diss=dissdata_g, distance=NULL, min.nc=2, max.nc=15, method="complete", index="silhouette") #14-0.1866
#Cluster using Complete Linkage algorithm
complete_g <- agnes(dissdata_g, diss=TRUE, method="complete", keep.diss=FALSE)
pltree(complete_g, main="Complete Linkage - Gower", cex=0.83, xlab="Data", ylab="Height")
c_complete_g <- cutree(complete_g, 14)
table(c_complete_g, datamat[,21])


NbClust(diss=dissdata_g, distance=NULL, min.nc=2, max.nc=15, method="average", index="silhouette") #15-0.2026
#Cluster using Average Linkage algorithm
average_g <- agnes(dissdata_g, diss=TRUE, method="average", keep.diss=FALSE)
pltree(average_g, main="Average Linkage - Gower", cex=0.83, xlab="Data", ylab="Height")
c_average_g <- cutree(average_g, 15)
table(c_average_g, datamat[,21])


NbClust(diss=dissdata_g, distance=NULL, min.nc=2, max.nc=15, method="ward.D", index="silhouette") #12-0.2319
#Cluster using Ward's Method algorithm
ward_g <- agnes(dissdata_g, diss=TRUE, method="ward", keep.diss=FALSE)
pltree(ward_g, main="Ward Method - Gower", cex=0.83, xlab="Data", ylab="Height")
c_ward_g <- cutree(ward_g, 12)
table(c_ward_g, datamat[,21])

#Clustering using divisive algoritms
diana_g <- diana(dissdata_g, diss=TRUE, keep.data=FALSE)
pltree(diana_g, main="Divisive Method - Gower", cex=0.83, xlab="Data", ylab="Height")
c_diana_g <- cutree(diana_g, 5)
table(c_diana_g, datamat[,21])

#Using Partitioning Around Medoids
pam_g<-pamk(dissdata_g, k=2:15, diss=TRUE, usepam=TRUE)
table(pam_g$pamobject$cluster,datamat[,21])



##########################################################################################################
##########################################################################################################
#######################################Principal Component Analysis#######################################
##########################################################################################################
##########################################################################################################
# 
# #one hot encoding (solving categorical variables' problem)
data.ponehot <- onehot(data[c(2:20)], max_levels = 60)
data.onehot <- predict(data.ponehot,data[c(2:20)])
# 
# #pca
data.pca <- prcomp(data.onehot, scale. = T, retx=TRUE)
summary(data.pca)
#36 makes it to aproximately 80%, so let's keep the first 36 principal components
scoresp <- data.pca$x
scoresp <- scoresp[,c(1:36)]
scores <- as.data.frame(scoresp[,1:36])


##########################################################################################################
##########################################################################################################
###############################CLUSTERING WITH EUCLIDIAN DISTANCE AS METRIC###############################
##########################################################################################################
##########################################################################################################


NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="single", index="silhouette")#2-0.6215
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="single", index="mcclain")#2-0.001
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="single", index="cindex")#2-0.3267
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="single", index="dunn")#4-0.9019
#Clustering with the Single Linkage method
single_eu <- agnes(scores, metric="euclidian", method="single", keep.diss=FALSE)
pltree(single_eu, main="Single Linkage - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_single_eu<-cutree(single_eu, 2)
table(c_single_eu, datamat[,21])

NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="complete", index="silhouette")#2-0.6215
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="complete", index="mcclain")#2-0.001
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="complete", index="cindex")#2-0.3267
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="complete", index="dunn")#4-0.9019
#Clustering with Complete Linkage method
complete_eu <- agnes(scores, metric="euclidian", method="complete", keep.diss=FALSE)
pltree(complete_eu, main="Complete Linkage - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_complete_eu <- cutree(complete_eu, 2)
table(c_complete_eu, datamat[,21])

NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="average", index="silhouette")#2-0.6215
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="average", index="mcclain")#2-0.001
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="average", index="cindex")#2-0.3267
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="average", index="dunn")#4-0.9019
#Clustering with Average Linkage method
average_eu <- agnes(scores, metric="euclidian", method="average", keep.diss=FALSE)
pltree(average_eu, main="Average Linkage - Euclidian", cex=0.83, xlab="Data", ylab="Height")
c_average_eu <- cutree(average_eu, 2)
table(c_average_eu, datamat[,21])


NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="ward.D", index="silhouette")#2-0.175
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="ward.D", index="mcclain")#2-0.0957
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="ward.D", index="cindex")#15-0.2097
NbClust(scores, distance="euclidean", min.nc=2, max.nc=15, method="ward.D", index="dunn")#2-0.2132
#Clustering with Ward Method
ward_eu <- agnes(scores, metric="euclidian", method="ward", keep.diss=FALSE)
pltree(ward_eu, main="Ward Method - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_ward_eu <- cutree(ward_eu, 2)
table(c_ward_eu, datamat[,21])


#Clustering with Divisive method
diana_eu = diana(scores, metric="euclidean", keep.diss=FALSE)
pltree(diana_eu, main="Diana - Euclidean", cex=0.83, xlab="Data", ylab="Height")
c_diana_eu<-cutree(diana_eu, 4)
table(c_diana_eu, datamat[,21])

#Clustering with Partition Around Medoids method
pam_eu<-pamk(scores, k=2:15, metric="euclidean", diss=FALSE, usepam=TRUE)
table(pam_eu$pamobject$cluster,datamat[,21])







##########################################################################################################
##########################################################################################################
###############################CLUSTERING WITH MANHATTAN DISTANCE AS METRIC###############################
##########################################################################################################
##########################################################################################################


NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="single", index="silhouette")#2-0.63
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="single", index="mcclain")#2-0.001
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="single", index="cindex")#2-0.2726
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="single", index="dunn")#4-0.7764
#Clustering with the Single Linkage method
single_man <- agnes(scores, metric="manhattan", method="single", keep.diss=FALSE)
pltree(single_man, main="Single Linkage - Manhattan", cex=0.83, xlab="Data", ylab="Height")
c_single_man<-cutree(single_man, 4)
table(c_single_man, datamat[,21])

NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="complete", index="silhouette")#2-0.6264
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="complete", index="mcclain")#2-0.001
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="complete", index="cindex")#2-0.3107
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="complete", index="dunn")#4-0.7764
#Clustering with Complete Linkage method
complete_man <- agnes(scores, metric="manhattan", method="complete", keep.diss=FALSE)
pltree(complete_man, main="Complete Linkage - Manhattan", cex=0.83, xlab="Data", ylab="Height")
c_complete_man <- cutree(complete_man, 2)
table(c_complete_man, datamat[,21])

NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="average", index="silhouette")#2-0.63
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="average", index="mcclain")#2-0.001
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="average", index="cindex")#2-0.2726
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="average", index="dunn")#4-0.7764
#Clustering with Average Linkage method
average_man <- agnes(scores, metric="manhattan", method="average", keep.diss=FALSE)
pltree(average_man, main="Average Linkage - Manhattan", cex=0.83, xlab="Data", ylab="Height")
c_average_man <- cutree(average_man, 2)
table(c_average_man, datamat[,21])


NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="ward.D", index="silhouette")#2-0.2005
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="ward.D", index="mcclain")#2-0.3693
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="ward.D", index="cindex")#15-0.1937
NbClust(scores, distance="manhattan", min.nc=2, max.nc=15, method="ward.D", index="dunn")#2-0.1179
#Clustering with Ward Method
ward_man <- agnes(scores, metric="manhattan", method="ward", keep.diss=FALSE)
pltree(ward_man, main="Ward Method - Manhattan", cex=0.83, xlab="Data", ylab="Height")
c_ward_man <- cutree(ward_man, 2)
table(c_ward_man, datamat[,21])


#Clustering with Divisive method
diana_man = diana(scores, metric="manhattan", keep.diss=FALSE)
pltree(diana_man, main="Diana - Manhattan", cex=0.83, xlab="Data", ylab="Height")
c_diana_man<-cutree(diana_man, 2)
table(c_diana_man, datamat[,21])

#Clustering with Partition Around Medoids method
pam_man<-pamk(scores, k=2:15, metric="manhattan", diss=FALSE, usepam=TRUE)
table(pam_man$pamobject$cluster,datamat[,21])


###################################################
#Using Kmeans for clustering now
####################################################
NbClust(scores, min.nc=2, max.nc=15, method="kmeans", index="all")#2-#11times
k1 <- kmeans(scores, centers=2)
table(k1$cluster, datamat[,21])
k2 <- kmeans(scores, centers=7)
table(k2$cluster, datamat[,21])
k3 <- kmeans(scores, centers=15)
table(k3$cluster, datamat[,21])


normal1 <- Mclust(scores, G=1:15)
table(normal1$classification, datamat[,21])
normal2 <- Mclust(scores, G=4:15)
table(normal2$classification, datamat[,21])
normal3 <- Mclust(scores, G=6:15)
table(normal3$classification, datamat[,21])


##########################################################################################################
##########################################################################################################
#######################################EVALUATE EACH CLUSTERING METHOD####################################
##########################################################################################################
##########################################################################################################
#Methods left out because of outliers greatly affecting the dendogram and the obtained partitions
#1-Single Linkage - Gower
#2-Single Linkage - Euclidean
#3-Complete Linkage - Euclidean
#4-Average Linkage - Euclidean
#5-Diana - Euclidean
#6-Single Linkage - Manhattan
#7-Complete Linkage - Manhattan
#8-Average Linkage - Manhattan
#9-Diana - Manhattan
###FROM HERE WORK IN PROGRESS, STILL NEED TO CHECK THE BEST SET OF PARTITIONS OBTAINED FROM THE 15 THAT ARE LEFT
#1-Complete Linkage - Gower
#2-Average Linkage - Gower
#3-Ward's Method - Gower
#4-Diana - Gower
#5-Pam - Gower
#6-Ward's Method - Euclidean
#7-Pam - Euclidian
#8-Ward's Method - Manhattan
#9-Pam - Manhattan
#10-Kmeans - 2 Centers
#11-Kmeans - 7 Centers
#12-Kmeans - 15 Centers
#13-Gaussian Mixture Model - Best One - IMPROVE THIS COMMENT
#14-Gaussian Mixture Model - Second inline for best one - IMPROVE THIS COMMENT
#15-Gaussian Mixture Model - Third inline for best one - IMPROVE THIS COMMENT






#intCriteria(datamat[,2:20], c_single_g, c("C_index", "Dunn", "Silhouette"))
#intCriteria(datamat[,2:20], c_complete_g, c("C_index", "Dunn", "Silhouette"))
#intCriteria(datamat[,2:20], c_average_g, c("C_index", "Dunn", "Silhouette"))
#intCriteria(datamat[,2:20], c_ward_g, c("C_index", "Dunn", "Silhouette"))
#intCriteria(datamat[,2:20], c_diana_g, c("C_index", "Dunn", "Silhouette"))
#intCriteria(datamat[,2:20], pam_g$pamobject$cluster, c("C_index", "Dunn", "Silhouette"))



####################################
####################################
########RESULTADO PROVISÓRIO########
####################################
####################################
result <- k2$cluster