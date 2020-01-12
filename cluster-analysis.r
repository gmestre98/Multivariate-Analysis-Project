library(readxl)
library(cluster)
library('NbClust')
library(fpc)
library(onehot)
library(e1071)

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


data <- data[-c(207,209),]
data <- data[, c(1:15, 17, 16)]
data <- data[-c(5,9:11)]

datamat <- matrix(unlist(data), ncol = 13, byrow = FALSE)

##########################################################################################################
##########################################################################################################
#######################################CLUSTER ANALYSIS -  GOWER##########################################
##########################################################################################################
##########################################################################################################

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
