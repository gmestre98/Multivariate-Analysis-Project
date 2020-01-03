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
      
#Test normality of variables
shapiro.test(data$`Transportation expense`)
shapiro.test(data$`Work Distance`)
shapiro.test(data$`Service time`)
shapiro.test(data$Age)
shapiro.test(data$`Hit target`)
shapiro.test(data$Son)
shapiro.test(data$Pet)
shapiro.test(data$Weight)
shapiro.test(data$Height)
shapiro.test(data$BMI)
shapiro.test(data$Absenteeism)
shapiro.test(data$`Work load Average/day`)

 #Categorical variables 
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

pairs(data[,10:21],col=ifelse(data$`Disciplinary failure`==0, "black", "red"), upper.panel = NULL) 
pairs(data[,10:21],col=ifelse(data$`Social smoker`==0, "black", "red"), upper.panel = NULL) 
pairs(data[,10:21],col=ifelse(data$`Social drinker`==0, "black", "red"), upper.panel = NULL) 

#Barchart absenteeism vs age
absent <- data[c(13,20)]
ggplot(absent) + geom_bar(aes(x = Age, y=Absenteeism), stat = "identity")
tab <-aggregate(absent$Absenteeism, by=list(Age=absent$Age), FUN=sum)
tab1 <- table(absent$Age)
Absenteeism <- as.vector(tab$x/tab1)
ggplot(tab) + geom_bar(aes(x = Age, y=Absenteeism), stat = "identity", fill=rgb(0.3,0.5,0.4,0.6)) +
ggtitle("Average Absenteeism time in hours by age") + theme(plot.title = element_text(hjust = 0.5))

