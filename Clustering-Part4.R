library(cluster)

nclusters = 15
data = read.csv2("Absenteeism_at_work.csv")
#Uncomment next line to remove nonsense values
#data = subset(subset(data, Month.of.absence != 0), Reason.for.absence != 0)

###############################################################
### Agglomerative clustering methods, with Euclidian Distance
###############################################################

eu_single = agnes(data, diss=FALSE, metric="euclidian",
                  stand=TRUE, method="single", keep.data=FALSE)
eu_complete = agnes(data, diss=FALSE, metric="euclidian",
                    stand=TRUE, method="complete", keep.data=FALSE)
eu_average = agnes(data, diss=FALSE, metric="euclidian",
                   stand=TRUE, method="average", keep.data=FALSE)
eu_ward = agnes(data, diss=FALSE, metric="euclidian",
                stand=TRUE, method="ward", keep.data=FALSE)



pltree(eu_single,main="Single linkage", cex=0.83, xlab="Data", ylab="Height")
c_eu_single<-cutree(eu_single,nclusters)
table(c_eu_single, data[,21])
rect.hclust(eu_complete, k=nclusters, border="red")

pltree(eu_complete,main="Complete linkage", cex=0.83, xlab="Data", ylab="Height")
c_eu_complete<-cutree(eu_complete,nclusters)
table(c_eu_complete, data[,21])
rect.hclust(eu_complete, k=nclusters, border="red")

pltree(eu_average,main="Average linkage", cex=0.83, xlab="Data", ylab="Height")
c_eu_average<-cutree(eu_average, nclusters)
table(c_eu_average, data[,21])
rect.hclust(eu_average, k=nclusters, border="red")

pltree(eu_ward,main="Ward Method", cex=0.83, xlab="Data", ylab="Height")
c_eu_ward<-cutree(eu_ward,nclusters)
table(c_eu_ward, data[,21])
rect.hclust(eu_ward, k=nclusters, border="red")


###############################################################
### Agglomerative clustering methods, with Manhattan Distance
###############################################################

man_single = agnes(data, diss=FALSE, metric="manhattan",
                   stand=TRUE, method="single", keep.data=FALSE)
man_complete = agnes(data, diss=FALSE, metric="manhattan",
                     stand=TRUE, method="complete", keep.data=FALSE)
man_average = agnes(data, diss=FALSE, metric="manhattan",
                    stand=TRUE, method="average", keep.data=FALSE)
man_ward = agnes(data, diss=FALSE, metric="manhattan",
                 stand=TRUE, method="ward", keep.data=FALSE)

pltree(man_single,main="Single linkage", cex=0.83, xlab="Data", ylab="Height")
c_man_single<-cutree(man_single,nclusters)
table(c_man_single, data[,21])
rect.hclust(man_complete, k=nclusters, border="red")

pltree(man_complete,main="Complete linkage", cex=0.83, xlab="Data", ylab="Height")
c_man_complete<-cutree(man_complete,nclusters)
table(c_man_complete, data[,21])
rect.hclust(man_complete, k=nclusters, border="red")

pltree(man_average,main="Average linkage", cex=0.83, xlab="Data", ylab="Height")
c_man_average<-cutree(man_average, nclusters)
table(c_man_average, data[,21])
rect.hclust(man_average, k=nclusters, border="red")

pltree(man_ward,main="Ward Method", cex=0.83, xlab="Data", ylab="Height")
c_man_ward<-cutree(man_ward,nclusters)
table(c_man_ward, data[,21])
rect.hclust(man_ward, k=nclusters, border="red")


###############################################################
### Divisive clustering methods
###############################################################
eu_diana = diana(data, diss=FALSE, metric="euclidian",
                  stand=TRUE, keep.data=FALSE)
pltree(eu_diana, main="Diana", cex=0.83, xlab="Data", ylab="Height")
c_eu_diana<-cutree(eu_diana,nclusters)
table(c_eu_diana, data[,21])
rect.hclust(eu_diana, k=nclusters, border="purple")

man_diana = diana(data, diss=FALSE, metric="manhattan",
                  stand=TRUE, keep.data=FALSE)
pltree(man_diana, main="Diana", cex=0.83, xlab="Data", ylab="Height")
c_man_diana<-cutree(man_diana,nclusters)
table(c_man_diana, data[,21])
rect.hclust(man_diana, k=nclusters, border="red")




### Find the best number of clusters for each method
#Check favourite bar - Datanovia article






###############################################################
### K-Means
###############################################################
#Use methods to obtain the recommended number of clusters to use
#Check link on favourite bar on chrome


###############################################################
### Partition Around Medoids
###############################################################
#Use methods to obtain the recommended number of clusters to use
#Check link on favourite bar on chrome






###############################################################
### Cluster analysis based on mixtures of normal distributions
###############################################################
#Use code from the lab



###############################################################
### Evaluate each clustering method
###############################################################
#Search web for the metrics to use





#Choose the best one of all the methods and stay with those clusters




###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
### NOW TRY ALL OF THESE USING PCA, MAY GET BETTER RESULTS
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################


