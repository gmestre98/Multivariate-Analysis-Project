#Install and Load Packages
install.packages("corrplot")
install.packages("aricode")
install.packages("caret")
install.packages("aricode")
devtools::install_github("pohlio/tidyinftheo")
library(readxl)
library(corrplot)
library(RColorBrewer)
library(psych) 
library(ggplot2)
library(aricode)
library("tidyinftheo")
library(aricode)
library(caret)

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


#changing work load average/day column
i=1;
while(i<dim(data[,1])[1]){data[i,17]=data[i,17]/1000; i=i+1}

#changing absenteeism column
i=1;
while(i<dim(data[,1])[1]){if(data[i,16]==0){data[i,16]=0}
  else if(1<=data[i,16] && data[i,16]<8){data[i,16]=1} 
  else if(data[i,16]==8){data[i,16]=2}
  else if(data[i,16]>8 && data[i,16]<=40){data[i,16]=3}
  else if(data[i,16]>40){data[i,16]=4}
  ;i=i+1}
data$Absenteeism <- as.factor(data$Absenteeism)

#changing reason for absence column
i=1;
while(i<dim(data[,1])[1]){if(data[i,1]>=1 && data[i,1]<=21){data[i,1]=1}
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
boxplot(data[17], col=rgb(0.3,0.5,0.4,0.6),las=2, ylim = c(0, 400),cex.axis=0.70)
mtext("Work Load Average/Day", side = 1, line = 1, cex = 0.7, font = 0.5)
boxplot(data[9], col=rgb(0.3,0.5,0.4,0.6),las=2,cex.axis=0.70)
mtext("Transportation Expense", side = 1, line = 1, cex = 0.7, font = 0.5)

#Divide the dataset considering the classes of absenteeism
absenteeism0<-data[which(data$Absenteeism==0),]
absenteeism1<-data[which(data$Absenteeism==1),]
absenteeism2<-data[which(data$Absenteeism==2),]
absenteeism3<-data[which(data$Absenteeism==3),]
absenteeism4<-data[which(data$Absenteeism==4),]

#Outliers in each class of absenteeism

which(absenteeism0$`Service time` %in% boxplot(absenteeism0$`Service time`, plot=FALSE)$out)
which(absenteeism0$Pet %in% boxplot(absenteeism0$Pet, plot=FALSE)$out)
which(absenteeism0$`Work load Average/day` %in% boxplot(absenteeism0$`Work load Average/day`, plot=FALSE)$out)
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

data <- data[-c(62,64),]
data <- data[, c(1:15, 17, 16)]

#Correlogram
corvar <- cor(data[9:16])
colnames(corvar) <- c("Transportation Expense", "Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
"Work load Average/day")
rownames(corvar) <- c("Transportation Expense", "Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
"Work load Average/day")
corrplot(corvar, method="color", col = brewer.pal(n = 8, name = "RdBu"), type="lower", tl.col = "black",
addCoef.col = "black", tl.srt = 15, tl.cex = 0.6, number.cex = 0.7, diag = FALSE)

#Mutual information between the quantitative variables
mutinf <- mutual_info_matrix(data, 9:15, normalized=TRUE)
mutinfnoage <- mutual_info_matrix(data, 9:11,13:15, normalized = TRUE)

#Remove the variables Age and Work Distance
data <- data[-c(10,12)]

#Test normality of variables
shapiro.test(data$`Transportation expense`)
shapiro.test(data$`Service time`)
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
disciplinary_counts <- table(data$`Disciplinary failure`)
disciplinary_counts / sum(disciplinary_counts)
education_counts <- table(data$Education)
education_counts / sum(education_counts)
smoker_counts <- table(data$`Social smoker`)
smoker_counts / sum(smoker_counts)
absenteeism_counts <- table(data$Absenteeism)
absenteeism_counts / sum(absenteeism_counts)


