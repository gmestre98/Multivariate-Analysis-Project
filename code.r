#Install and Load Packages
install.packages("corrplot")
library(readxl)
library(corrplot)
library(RColorBrewer)
library(psych) 

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

summary(data)
apply(data, 2, sd)

#Boxplots for the quantitative variables
boxplot(data[,11:20], col=rgb(0.3,0.5,0.4,0.6), las=2,
names=c("Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
"Weight", "Height", "BMI", "Absenteeism"),
cex.axis=0.70)
boxplot(data[21], col=rgb(0.3,0.5,0.4,0.6),las=2,cex.axis=0.70)
mtext("Work Load Average/Day", side = 1, line = 1, cex = 0.7, font = 0.5)
boxplot(data[10], col=rgb(0.3,0.5,0.4,0.6),las=2,cex.axis=0.70)
mtext("Transportation Expense", side = 1, line = 1, cex = 0.7, font = 0.5)

#Correlogram
corvar <- cor(data[10:21])
colnames(corvar) <- c("Transportation Expense", "Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
"Weight", "Height", "BMI", "Absenteeism", "Work load Average/day")
rownames(corvar) <- c("Transportation Expense", "Work Distance", "Service Time", "Age", "Hit Target", "Son", "Pet", 
"Weight", "Height", "BMI", "Absenteeism", "Work load Average/day")
corrplot(corvar, method="color", col = brewer.pal(n = 11, name = "RdBu"), type="lower", tl.col = "black",
addCoef.col = "black", tl.srt = 30, tl.cex = 0.6, number.cex = 0.7)

#Remove variables with low correlation and scatterplot
datacorr <- data[c(10:13,15:19)]
names(datacorr) <- c("Tranportation Expense", "Work Distance", "Service Time", "Age", "Son", "Pet", 
"Weight", "Height", "BMI")
pairs.panels(datacorr, hist.col = "#00AFBB", smooth =FALSE,ellipses=FALSE,lm=TRUE) 
 
