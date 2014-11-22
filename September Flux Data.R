#SPRUCE Data Analysis 
#September 2014
setwd("~/Desktop/BU/Research/minnesota/Picarro Data 6.25.14/R analysis/09/data files")
filenames <- list.files("test data", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
dim(ldf)

load_data <- function(path) { 
  files <- dir("test data", pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

data <- load_data("test data")
dim(data)
data1<-data[,c(1:9,22:46)] 
data1<-data1[complete.cases(data1[,"MEASUREMENT_ID"]),]

#AllData<-AllData[complete.cases(AllData[,"Measurement_ID"]),]
data1$HR_CH4_sum<-rowSums(data1[,c('HR_12CH4', 'HR_13CH4')])
data1$HP_CH4_sum<-rowSums(data1[,c('HP_12CH4', 'HP_13CH4')])
data1$CO2_sum<-rowSums(data1[,c('X12CO2', 'X13CO2')])
data1$HR_CH4_Inverse<-1/data1$HR_CH4_sum
data1$HP_CH4_Inverse<-1/data1$HP_CH4_sum
data1$CO2_Inverse<-1/data1$CO2_sum
data1$TIME_EDIT<-as.POSIXct(data1$TIME_EDIT, format='%H:%M:%S')
data1$TIME_OFFSET<-as.POSIXct(data1$TIME_OFFSET, format='%H:%M:%S')
data1$TIME_EDIT[1:20]
colnames(data1)
unique(data1$MEASUREMENT_ID)

###############################################################################################
##Flux measurements
#meas1
meas1<-subset(data1, data1[, "MEASUREMENT_ID"]=="1") #create baby matrix
plot(meas1$JULIAN_DAYS, meas1$HR_CH4_sum) #check tail on graph
dim(meas1)
meas1_edit<-meas1[30:3050,] #creates smaller matrix
plot(meas1_edit$JULIAN_DAYS, meas1_edit$HP_CH4_sum)
fit1<-lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS)
CH4.R2<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$coefficients[2]
plot(meas1_edit$JULIAN_DAYS, meas1_edit$CO2_sum)
abline(fit1)
fit1<-lm(meas1_edit$CO2_sum~meas1_edit$JULIAN_DAYS)
CO2.R2<-summary(lm(meas1_edit$CO2_sum~meas1_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas1_edit$CO2_sum~meas1_edit$JULIAN_DAYS))$coefficients[2]

meas1data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas2
meas2<-subset(data1, data1[, "MEASUREMENT_ID"]=="2") #create baby matrix
plot(meas2$JULIAN_DAYS, meas2$HR_CH4_sum) #check tail on graph
dim(meas2)
meas2_edit<-meas2[130:1445,] #creates smaller matrix
plot(meas2_edit$JULIAN_DAYS, meas2_edit$HR_CH4_sum)
fit1<-lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS))$coefficients[2]
plot(meas2_edit$JULIAN_DAYS, meas2_edit$CO2_sum)
abline(fit1)
fit1<-lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS)
CO2.R2<-summary(lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS))$coefficients[2]

meas2data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas2<-subset(data1, data1[, "MEASUREMENT_ID"]=="2") #create baby matrix
plot(meas2$JULIAN_DAYS, meas2$HR_CH4_sum) #check tail on graph
dim(meas2)
meas2_edit<-meas2[130:500,] #creates smaller matrix
plot(meas2_edit$JULIAN_DAYS, meas2_edit$HR_CH4_sum)
fit1<-lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS))$coefficients[2]
plot(meas2_edit$JULIAN_DAYS, meas2_edit$CO2_sum)
fit1<-lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS)
abline(fit1)
CO2.R2<-summary(lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS))$coefficients[2]

meas2data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas2<-subset(data1, data1[, "MEASUREMENT_ID"]=="2") #create baby matrix
plot(meas2$JULIAN_DAYS, meas2$HR_CH4_sum) #check tail on graph
dim(meas2)
meas2_edit<-meas2[1100:1445,] #creates smaller matrix
plot(meas2_edit$JULIAN_DAYS, meas2_edit$HR_CH4_sum)
fit1<-lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas2_edit$HR_CH4_sum~meas2_edit$JULIAN_DAYS))$coefficients[2]
plot(meas2_edit$JULIAN_DAYS, meas2_edit$CO2_sum)
fit1<-lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS)
abline(fit1)
CO2.R2<-summary(lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas2_edit$CO2_sum~meas2_edit$JULIAN_DAYS))$coefficients[2]

meas2data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas3
meas3<-subset(data1, data1[, "MEASUREMENT_ID"]=="3") #create baby matrix
plot(meas3$JULIAN_DAYS, meas3$HR_CH4_sum) #check tail on graph
dim(meas3)
meas3_edit<-meas3[180:2065,] #creates smaller matrix
plot(meas3_edit$JULIAN_DAYS, meas3_edit$HP_CH4_sum)
fit1<-lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS))$coefficients[2]
plot(meas3_edit$JULIAN_DAYS, meas3_edit$CO2_sum)
fit1<-lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS))$coefficients[2]

meas3data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas3<-subset(data1, data1[, "MEASUREMENT_ID"]=="3") #create baby matrix
plot(meas3$JULIAN_DAYS, meas3$HR_CH4_sum) #check tail on graph
dim(meas3)
meas3_edit<-meas3[180:600,] #creates smaller matrix
plot(meas3_edit$JULIAN_DAYS, meas3_edit$HP_CH4_sum)
fit1<-lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS))$coefficients[2]
plot(meas3_edit$JULIAN_DAYS, meas3_edit$CO2_sum)
fit1<-lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS))$coefficients[2]

meas3data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas3<-subset(data1, data1[, "MEASUREMENT_ID"]=="3") #create baby matrix
plot(meas3$JULIAN_DAYS, meas3$HR_CH4_sum) #check tail on graph
dim(meas3)
meas3_edit<-meas3[1200:2050,] #creates smaller matrix
plot(meas3_edit$JULIAN_DAYS, meas3_edit$HP_CH4_sum)
fit1<-lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas3_edit$HP_CH4_sum~meas3_edit$JULIAN_DAYS))$coefficients[2]
plot(meas3_edit$JULIAN_DAYS, meas3_edit$CO2_sum)
fit1<-lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas3_edit$CO2_sum~meas3_edit$JULIAN_DAYS))$coefficients[2]

meas3data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas4
meas4<-subset(data1, data1[, "MEASUREMENT_ID"]=="4") #create baby matrix
plot(meas4$JULIAN_DAYS, meas4$HR_CH4_sum) #check tail on graph
dim(meas4)
meas4_edit<-meas4[150:3000,] #creates smaller matrix
plot(meas4_edit$JULIAN_DAYS, meas4_edit$HP_CH4_sum)
fit1<-lm(meas4_edit$HP_CH4_sum~meas4_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas4_edit$HP_CH4_sum~meas4_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas4_edit$HP_CH4_sum~meas4_edit$JULIAN_DAYS))$coefficients[2]
plot(meas4_edit$JULIAN_DAYS, meas4_edit$CO2_sum)
fit1<-lm(meas4_edit$CO2_sum~meas4_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas4_edit$CO2_sum~meas4_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas4_edit$CO2_sum~meas4_edit$JULIAN_DAYS))$coefficients[2]

meas4data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas5
meas5<-subset(data1, data1[, "MEASUREMENT_ID"]=="5") #create baby matrix
plot(meas5$JULIAN_DAYS, meas5$HR_CH4_sum) #check tail on graph
dim(meas5)
meas5_edit<-meas5[200:1540,] #creates smaller matrix
plot(meas5_edit$JULIAN_DAYS, meas5_edit$HP_CH4_sum)
fit1<-lm(meas5_edit$HP_CH4_sum~meas5_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas5_edit$HP_CH4_sum~meas5_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas5_edit$HP_CH4_sum~meas5_edit$JULIAN_DAYS))$coefficients[2]
plot(meas5_edit$JULIAN_DAYS, meas5_edit$CO2_sum)
fit1<-lm(meas5_edit$CO2_sum~meas5_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas5_edit$CO2_sum~meas5_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas5_edit$CO2_sum~meas5_edit$JULIAN_DAYS))$coefficients[2]

meas5data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas6
meas6<-subset(data1, data1[, "MEASUREMENT_ID"]=="6") #create baby matrix
plot(meas6$JULIAN_DAYS, meas6$HR_CH4_sum) #check tail on graph
plot(meas6$JULIAN_DAYS, meas6$CO2_sum)
dim(meas6)
meas6_edit<-meas6[100:3038,] #creates smaller matrix
plot(meas6_edit$JULIAN_DAYS, meas6_edit$HP_CH4_sum)
fit1<-lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS))$coefficients[2]
plot(meas6_edit$JULIAN_DAYS, meas6_edit$CO2_sum)
fit1<-lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS))$coefficients[2]

meas6data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas6<-subset(data1, data1[, "MEASUREMENT_ID"]=="6") #create baby matrix
plot(meas6$JULIAN_DAYS, meas6$HR_CH4_sum) #check tail on graph
plot(meas6$JULIAN_DAYS, meas6$CO2_sum)
dim(meas6)
meas6_edit<-meas6[100:1500,] #creates smaller matrix
plot(meas6_edit$JULIAN_DAYS, meas6_edit$HP_CH4_sum)
fit1<-lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS))$coefficients[2]
plot(meas6_edit$JULIAN_DAYS, meas6_edit$CO2_sum)
fit1<-lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS))$coefficients[2]

meas6data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas6<-subset(data1, data1[, "MEASUREMENT_ID"]=="6") #create baby matrix
plot(meas6$JULIAN_DAYS, meas6$HR_CH4_sum) #check tail on graph
plot(meas6$JULIAN_DAYS, meas6$CO2_sum)
dim(meas6)
meas6_edit<-meas6[1570:3038,] #creates smaller matrix
plot(meas6_edit$JULIAN_DAYS, meas6_edit$HP_CH4_sum)
fit1<-lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas6_edit$HP_CH4_sum~meas6_edit$JULIAN_DAYS))$coefficients[2]
plot(meas6_edit$JULIAN_DAYS, meas6_edit$CO2_sum)
fit1<-lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas6_edit$CO2_sum~meas6_edit$JULIAN_DAYS))$coefficients[2]

meas6data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas7
meas7<-subset(data1, data1[, "MEASUREMENT_ID"]=="7") #create baby matrix
plot(meas7$JULIAN_DAYS, meas7$HR_CH4_sum) #check tail on graph
dim(meas7)
meas7_edit<-meas7[100:789,] #creates smaller matrix
plot(meas7_edit$JULIAN_DAYS, meas7_edit$HR_CH4_sum)
fit1<-lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS))$coefficients[2]
plot(meas7_edit$JULIAN_DAYS, meas7_edit$CO2_sum)
fit1<-lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS))$coefficients[2]

meas7data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas7<-subset(data1, data1[, "MEASUREMENT_ID"]=="7") #create baby matrix
plot(meas7$JULIAN_DAYS, meas7$HR_CH4_sum) #check tail on graph
dim(meas7)
meas7_edit<-meas7[100:350,] #creates smaller matrix
plot(meas7_edit$JULIAN_DAYS, meas7_edit$HR_CH4_sum)
fit1<-lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS))$coefficients[2]
plot(meas7_edit$JULIAN_DAYS, meas7_edit$CO2_sum)
fit1<-lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS))$coefficients[2]

meas7data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas7<-subset(data1, data1[, "MEASUREMENT_ID"]=="7") #create baby matrix
plot(meas7$JULIAN_DAYS, meas7$HR_CH4_sum) #check tail on graph
dim(meas7)
meas7_edit<-meas7[440:789,] #creates smaller matrix
plot(meas7_edit$JULIAN_DAYS, meas7_edit$HR_CH4_sum)
fit1<-lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas7_edit$HR_CH4_sum~meas7_edit$JULIAN_DAYS))$coefficients[2]
plot(meas7_edit$JULIAN_DAYS, meas7_edit$CO2_sum)
fit1<-lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas7_edit$CO2_sum~meas7_edit$JULIAN_DAYS))$coefficients[2]

meas7data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas8
meas8<-subset(data1, data1[, "MEASUREMENT_ID"]=="8") #create baby matrix
plot(meas8$JULIAN_DAYS, meas8$HR_CH4_sum) #check tail on graph
dim(meas8)
meas8_edit<-meas8[50:1150,] #creates smaller matrix
plot(meas8_edit$JULIAN_DAYS, meas8_edit$HR_CH4_sum)
fit1<-lm(meas8_edit$HR_CH4_sum~meas8_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas8_edit$HR_CH4_sum~meas8_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas8_edit$HR_CH4_sum~meas8_edit$JULIAN_DAYS))$coefficients[2]
plot(meas8_edit$JULIAN_DAYS, meas8_edit$CO2_sum)
fit1<-lm(meas8_edit$CO2_sum~meas8_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas8_edit$CO2_sum~meas8_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas8_edit$CO2_sum~meas8_edit$JULIAN_DAYS))$coefficients[2]

meas8data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas9
meas9<-subset(data1, data1[, "MEASUREMENT_ID"]=="9") #create baby matrix
plot(meas9$JULIAN_DAYS, meas9$HP_CH4_sum) #check tail on graph
dim(meas9)
meas9_edit<-meas9[100:1150,] #creates smaller matrix
plot(meas9_edit$JULIAN_DAYS, meas9_edit$HP_CH4_sum)
fit1<-lm(meas9_edit$HP_CH4_sum~meas9_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas9_edit$HP_CH4_sum~meas9_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas9_edit$HP_CH4_sum~meas9_edit$JULIAN_DAYS))$coefficients[2]
plot(meas9_edit$JULIAN_DAYS, meas9_edit$CO2_sum)
fit1<-lm(meas9_edit$CO2_sum~meas9_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas9_edit$CO2_sum~meas9_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas9_edit$CO2_sum~meas9_edit$JULIAN_DAYS))$coefficients[2]

meas9data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas10
meas10<-subset(data1, data1[, "MEASUREMENT_ID"]=="10") #create baby matrix
plot(meas10$JULIAN_DAYS, meas10$HP_CH4_sum) #check tail on graph
dim(meas10)
meas10_edit<-meas10[10:1959,] #creates smaller matrix
plot(meas10_edit$JULIAN_DAYS, meas10_edit$HP_CH4_sum)
fit1<-lm(meas10_edit$HP_CH4_sum~meas10_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas10_edit$HP_CH4_sum~meas10_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas10_edit$HP_CH4_sum~meas10_edit$JULIAN_DAYS))$coefficients[2]
plot(meas10_edit$JULIAN_DAYS, meas10_edit$CO2_sum)
fit1<-lm(meas10_edit$CO2_sum~meas10_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas10_edit$CO2_sum~meas10_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas10_edit$CO2_sum~meas10_edit$JULIAN_DAYS))$coefficients[2]

meas10data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas11
meas11<-subset(data1, data1[, "MEASUREMENT_ID"]=="11") #create baby matrix
plot(meas11$JULIAN_DAYS, meas11$HR_CH4_sum) #check tail on graph
dim(meas11)
meas11_edit<-meas11[300:2545,] #creates smaller matrix
plot(meas11_edit$JULIAN_DAYS, meas11_edit$HR_CH4_sum)
fit1<-lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS))$coefficients[2]
plot(meas11_edit$JULIAN_DAYS, meas11_edit$CO2_sum)
fit1<-lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS))$coefficients[2]

meas11data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas11<-subset(data1, data1[, "MEASUREMENT_ID"]=="11") #create baby matrix
plot(meas11$JULIAN_DAYS, meas11$HR_CH4_sum) #check tail on graph
dim(meas11)
meas11_edit<-meas11[300:1400,] #creates smaller matrix
plot(meas11_edit$JULIAN_DAYS, meas11_edit$HR_CH4_sum)
fit1<-lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS))$coefficients[2]
plot(meas11_edit$JULIAN_DAYS, meas11_edit$CO2_sum)
fit1<-lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS))$coefficients[2]

meas11data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas11<-subset(data1, data1[, "MEASUREMENT_ID"]=="11") #create baby matrix
plot(meas11$JULIAN_DAYS, meas11$HR_CH4_sum) #check tail on graph
dim(meas11)
meas11_edit<-meas11[1800:2540,] #creates smaller matrix
plot(meas11_edit$JULIAN_DAYS, meas11_edit$HR_CH4_sum)
fit1<-lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas11_edit$HR_CH4_sum~meas11_edit$JULIAN_DAYS))$coefficients[2]
plot(meas11_edit$JULIAN_DAYS, meas11_edit$CO2_sum)
fit1<-lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas11_edit$CO2_sum~meas11_edit$JULIAN_DAYS))$coefficients[2]

meas11data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas12
meas12<-subset(data1, data1[, "MEASUREMENT_ID"]=="12") #create baby matrix
plot(meas12$JULIAN_DAYS, meas12$HR_CH4_sum) #check tail on graph
dim(meas12)
meas12_edit<-meas12[100:2715,] #creates smaller matrix
plot(meas12_edit$JULIAN_DAYS, meas12_edit$HR_CH4_sum)
fit1<-lm(meas12_edit$HR_CH4_sum~meas12_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas12_edit$HR_CH4_sum~meas12_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas12_edit$HR_CH4_sum~meas12_edit$JULIAN_DAYS))$coefficients[2]
plot(meas12_edit$JULIAN_DAYS, meas12_edit$CO2_sum)
fit1<-lm(meas12_edit$CO2_sum~meas12_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas12_edit$CO2_sum~meas12_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas12_edit$CO2_sum~meas12_edit$JULIAN_DAYS))$coefficients[2]

meas12data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas13
meas13<-subset(data1, data1[, "MEASUREMENT_ID"]=="13") #create baby matrix
plot(meas13$JULIAN_DAYS, meas13$HR_CH4_sum) #check tail on graph
dim(meas13)
meas13_edit<-meas13[90:1550,] #creates smaller matrix
plot(meas13_edit$JULIAN_DAYS, meas13_edit$HR_CH4_sum)
fit1<-lm(meas13_edit$HR_CH4_sum~meas13_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas13_edit$HR_CH4_sum~meas13_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas13_edit$HR_CH4_sum~meas13_edit$JULIAN_DAYS))$coefficients[2]
plot(meas13_edit$JULIAN_DAYS, meas13_edit$CO2_sum)
fit1<-lm(meas13_edit$CO2_sum~meas13_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas13_edit$CO2_sum~meas13_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas13_edit$CO2_sum~meas13_edit$JULIAN_DAYS))$coefficients[2]

meas13data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas13
meas13<-subset(data1, data1[, "MEASUREMENT_ID"]=="13") #create baby matrix
plot(meas13$JULIAN_DAYS, meas13$HR_CH4_sum) #check tail on graph
dim(meas13)
meas13_edit<-meas13[90:1150,] #creates smaller matrix
plot(meas13_edit$JULIAN_DAYS, meas13_edit$HR_CH4_sum)
fit1<-lm(meas13_edit$HR_CH4_sum~meas13_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas13_edit$HR_CH4_sum~meas13_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas13_edit$HR_CH4_sum~meas13_edit$JULIAN_DAYS))$coefficients[2]
plot(meas13_edit$JULIAN_DAYS, meas13_edit$CO2_sum)
fit1<-lm(meas13_edit$CO2_sum~meas13_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas13_edit$CO2_sum~meas13_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas13_edit$CO2_sum~meas13_edit$JULIAN_DAYS))$coefficients[2]

meas13data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas14
meas14<-subset(data1, data1[, "MEASUREMENT_ID"]=="14") #create baby matrix
plot(meas14$JULIAN_DAYS, meas14$HP_CH4_sum) #check tail on graph
dim(meas14)
meas14_edit<-meas14[150:1151,] #creates smaller matrix
plot(meas14_edit$JULIAN_DAYS, meas14_edit$HP_CH4_sum)
fit1<-lm(meas14_edit$HP_CH4_sum~meas14_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas14_edit$HP_CH4_sum~meas14_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas14_edit$HP_CH4_sum~meas14_edit$JULIAN_DAYS))$coefficients[2]
plot(meas14_edit$JULIAN_DAYS, meas14_edit$CO2_sum)
fit1<-lm(meas14_edit$CO2_sum~meas14_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas14_edit$CO2_sum~meas14_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas14_edit$CO2_sum~meas14_edit$JULIAN_DAYS))$coefficients[2]

meas14data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas15
meas15<-subset(data1, data1[, "MEASUREMENT_ID"]=="15") #create baby matrix
plot(meas15$JULIAN_DAYS, meas15$HP_CH4_sum) #check tail on graph
dim(meas15)
meas15_edit<-meas15[5:2699,] #creates smaller matrix
plot(meas15_edit$JULIAN_DAYS, meas15_edit$HP_CH4_sum)
fit1<-lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS))$coefficients[2]
plot(meas15_edit$JULIAN_DAYS, meas15_edit$CO2_sum)
fit1<-lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS))$coefficients[2]

meas15data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas15<-subset(data1, data1[, "MEASUREMENT_ID"]=="15") #create baby matrix
plot(meas15$JULIAN_DAYS, meas15$HP_CH4_sum) #check tail on graph
dim(meas15)
meas15_edit<-meas15[5:750,] #creates smaller matrix
plot(meas15_edit$JULIAN_DAYS, meas15_edit$HP_CH4_sum)
fit1<-lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS))$coefficients[2]
plot(meas15_edit$JULIAN_DAYS, meas15_edit$CO2_sum)
fit1<-lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS))$coefficients[2]

meas15data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

meas15<-subset(data1, data1[, "MEASUREMENT_ID"]=="15") #create baby matrix
plot(meas15$JULIAN_DAYS, meas15$HP_CH4_sum) #check tail on graph
dim(meas15)
meas15_edit<-meas15[850:2699,] #creates smaller matrix
plot(meas15_edit$JULIAN_DAYS, meas15_edit$HP_CH4_sum)
fit1<-lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas15_edit$HP_CH4_sum~meas15_edit$JULIAN_DAYS))$coefficients[2]
plot(meas15_edit$JULIAN_DAYS, meas15_edit$CO2_sum)
fit1<-lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas15_edit$CO2_sum~meas15_edit$JULIAN_DAYS))$coefficients[2]

meas15data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas16
meas16<-subset(data1, data1[, "MEASUREMENT_ID"]=="16") #create baby matrix
plot(meas16$JULIAN_DAYS, meas16$HR_CH4_sum) #check tail on graph
dim(meas16)
meas16_edit<-meas16[50:1900,] #creates smaller matrix
plot(meas16_edit$JULIAN_DAYS, meas16_edit$HR_CH4_sum)
fit1<-lm(meas16_edit$HR_CH4_sum~meas16_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas16_edit$HR_CH4_sum~meas16_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas16_edit$HR_CH4_sum~meas16_edit$JULIAN_DAYS))$coefficients[2]
plot(meas16_edit$JULIAN_DAYS, meas16_edit$CO2_sum)
fit1<-lm(meas16_edit$CO2_sum~meas16_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas16_edit$CO2_sum~meas16_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas16_edit$CO2_sum~meas16_edit$JULIAN_DAYS))$coefficients[2]

meas16data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas17
meas17<-subset(data1, data1[, "MEASUREMENT_ID"]=="17") #create baby matrix
plot(meas17$JULIAN_DAYS, meas17$HR_CH4_sum) #check tail on graph
dim(meas17)
meas17_edit<-meas17[150:1650,] #creates smaller matrix
plot(meas17_edit$JULIAN_DAYS, meas17_edit$HR_CH4_sum)
fit1<-lm(meas17_edit$HR_CH4_sum~meas17_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas17_edit$HR_CH4_sum~meas17_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas17_edit$HR_CH4_sum~meas17_edit$JULIAN_DAYS))$coefficients[2]
plot(meas17_edit$JULIAN_DAYS, meas17_edit$CO2_sum)
fit1<-lm(meas17_edit$CO2_sum~meas17_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas17_edit$CO2_sum~meas17_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas17_edit$CO2_sum~meas17_edit$JULIAN_DAYS))$coefficients[2]

meas17data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas18
meas18<-subset(data1, data1[, "MEASUREMENT_ID"]=="18") #create baby matrix
plot(meas18$JULIAN_DAYS, meas18$HR_CH4_sum) #check tail on graph
dim(meas18)
meas18_edit<-meas18[150:2499,] #creates smaller matrix
plot(meas18_edit$JULIAN_DAYS, meas18_edit$HR_CH4_sum)
fit1<-lm(meas18_edit$HR_CH4_sum~meas18_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas18_edit$HR_CH4_sum~meas18_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas18_edit$HR_CH4_sum~meas18_edit$JULIAN_DAYS))$coefficients[2]
plot(meas18_edit$JULIAN_DAYS, meas18_edit$CO2_sum)
fit1<-lm(meas18_edit$CO2_sum~meas18_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas18_edit$CO2_sum~meas18_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas18_edit$CO2_sum~meas18_edit$JULIAN_DAYS))$coefficients[2]

meas18data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas19
meas19<-subset(data1, data1[, "MEASUREMENT_ID"]=="19") #create baby matrix
plot(meas19$JULIAN_DAYS, meas19$HR_CH4_sum) #check tail on graph
dim(meas19)
meas19_edit<-meas19[100:1096,] #creates smaller matrix
plot(meas19_edit$JULIAN_DAYS, meas19_edit$HR_CH4_sum)
fit1<-lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS))$coefficients[2]
plot(meas19_edit$JULIAN_DAYS, meas19_edit$CO2_sum)
fit1<-lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS))$coefficients[2]

meas19data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas19
meas19<-subset(data1, data1[, "MEASUREMENT_ID"]=="19") #create baby matrix
plot(meas19$JULIAN_DAYS, meas19$HR_CH4_sum) #check tail on graph
dim(meas19)
meas19_edit<-meas19[100:450,] #creates smaller matrix
plot(meas19_edit$JULIAN_DAYS, meas19_edit$HR_CH4_sum)
fit1<-lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS))$coefficients[2]
plot(meas19_edit$JULIAN_DAYS, meas19_edit$CO2_sum)
fit1<-lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS))$coefficients[2]

meas19data_seg1<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas19
meas19<-subset(data1, data1[, "MEASUREMENT_ID"]=="19") #create baby matrix
plot(meas19$JULIAN_DAYS, meas19$HR_CH4_sum) #check tail on graph
dim(meas19)
meas19_edit<-meas19[500:1096,] #creates smaller matrix
plot(meas19_edit$JULIAN_DAYS, meas19_edit$HR_CH4_sum)
fit1<-lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas19_edit$HR_CH4_sum~meas19_edit$JULIAN_DAYS))$coefficients[2]
plot(meas19_edit$JULIAN_DAYS, meas19_edit$CO2_sum)
fit1<-lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas19_edit$CO2_sum~meas19_edit$JULIAN_DAYS))$coefficients[2]

meas19data_seg2<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas20
meas20<-subset(data1, data1[, "MEASUREMENT_ID"]=="20") #create baby matrix
plot(meas20$JULIAN_DAYS, meas20$HR_CH4_sum) #check tail on graph
dim(meas20)
meas20_edit<-meas20[150:1541,] #creates smaller matrix
plot(meas20_edit$JULIAN_DAYS, meas20_edit$HP_CH4_sum)
fit1<-lm(meas20_edit$HP_CH4_sum~meas20_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas20_edit$HR_CH4_sum~meas20_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas20_edit$HR_CH4_sum~meas20_edit$JULIAN_DAYS))$coefficients[2]
plot(meas20_edit$JULIAN_DAYS, meas20_edit$CO2_sum)
fit1<-lm(meas20_edit$CO2_sum~meas20_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas20_edit$CO2_sum~meas20_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas20_edit$CO2_sum~meas20_edit$JULIAN_DAYS))$coefficients[2]

meas20data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas21
meas21<-subset(data1, data1[, "MEASUREMENT_ID"]=="21") #create baby matrix
plot(meas21$JULIAN_DAYS, meas21$HP_CH4_sum) #check tail on graph
dim(meas21)
meas21_edit<-meas21[50:1137,] #creates smaller matrix
plot(meas21_edit$JULIAN_DAYS, meas21_edit$HP_CH4_sum)
fit1<-lm(meas21_edit$HP_CH4_sum~meas21_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas21_edit$HP_CH4_sum~meas21_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas21_edit$HP_CH4_sum~meas21_edit$JULIAN_DAYS))$coefficients[2]
plot(meas21_edit$JULIAN_DAYS, meas21_edit$CO2_sum)
fit1<-lm(meas21_edit$CO2_sum~meas21_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas21_edit$CO2_sum~meas21_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas21_edit$CO2_sum~meas21_edit$JULIAN_DAYS))$coefficients[2]

meas21data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas22
meas22<-subset(data1, data1[, "MEASUREMENT_ID"]=="22") #create baby matrix
plot(meas22$JULIAN_DAYS, meas22$HP_CH4_sum) #check tail on graph
dim(meas22)
meas22_edit<-meas22[50:525,] #creates smaller matrix
plot(meas22_edit$JULIAN_DAYS, meas22_edit$HP_CH4_sum)
fit1<-lm(meas22_edit$HP_CH4_sum~meas22_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas22_edit$HP_CH4_sum~meas22_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas22_edit$HP_CH4_sum~meas22_edit$JULIAN_DAYS))$coefficients[2]
plot(meas22_edit$JULIAN_DAYS, meas22_edit$CO2_sum)
fit1<-lm(meas22_edit$CO2_sum~meas22_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas22_edit$CO2_sum~meas22_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas22_edit$CO2_sum~meas22_edit$JULIAN_DAYS))$coefficients[2]

meas22data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas23
meas23<-subset(data1, data1[, "MEASUREMENT_ID"]=="23") #create baby matrix
plot(meas23$JULIAN_DAYS, meas23$HP_CH4_sum) #check tail on graph
dim(meas23)
meas23_edit<-meas23[120:3105,] #creates smaller matrix
plot(meas23_edit$JULIAN_DAYS, meas23_edit$HP_CH4_sum)
fit1<-lm(meas23_edit$HP_CH4_sum~meas23_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas23_edit$HP_CH4_sum~meas23_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas23_edit$HP_CH4_sum~meas23_edit$JULIAN_DAYS))$coefficients[2]
plot(meas23_edit$JULIAN_DAYS, meas23_edit$CO2_sum)
fit1<-lm(meas23_edit$CO2_sum~meas23_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas23_edit$CO2_sum~meas23_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas23_edit$CO2_sum~meas23_edit$JULIAN_DAYS))$coefficients[2]

meas23data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas24
meas24<-subset(data1, data1[, "MEASUREMENT_ID"]=="24") #create baby matrix
plot(meas24$JULIAN_DAYS, meas24$HP_CH4_sum) #check tail on graph
dim(meas24)
meas24_edit<-meas24[500:3112,] #creates smaller matrix
plot(meas24_edit$JULIAN_DAYS, meas24_edit$HP_CH4_sum)
fit1<-lm(meas24_edit$HP_CH4_sum~meas24_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas24_edit$HP_CH4_sum~meas24_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas24_edit$HP_CH4_sum~meas24_edit$JULIAN_DAYS))$coefficients[2]
plot(meas24_edit$JULIAN_DAYS, meas24_edit$CO2_sum)
fit1<-lm(meas24_edit$CO2_sum~meas24_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas24_edit$CO2_sum~meas24_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas24_edit$CO2_sum~meas24_edit$JULIAN_DAYS))$coefficients[2]

meas24data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

#meas25
meas25<-subset(data1, data1[, "MEASUREMENT_ID"]=="25") #create baby matrix
plot(meas25$JULIAN_DAYS, meas25$HR_CH4_sum) #check tail on graph
plot(meas25$JULIAN_DAYS, meas25$CO2_sum) 
dim(meas25)
meas25_edit<-meas25[500:1000,] #creates smaller matrix
plot(meas25_edit$JULIAN_DAYS, meas25_edit$HR_CH4_sum)
fit1<-lm(meas25_edit$HR_CH4_sum~meas25_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CH4.R2<-summary(lm(meas25_edit$HR_CH4_sum~meas25_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas25_edit$HR_CH4_sum~meas25_edit$JULIAN_DAYS))$coefficients[2]
plot(meas25_edit$JULIAN_DAYS, meas25_edit$CO2_sum)
fit1<-lm(meas25_edit$CO2_sum~meas25_edit$JULIAN_DAYS)
abline(fit1)
summary(fit1)
CO2.R2<-summary(lm(meas25_edit$CO2_sum~meas25_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas25_edit$CO2_sum~meas25_edit$JULIAN_DAYS))$coefficients[2]

meas25data_all<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)












par(mfcol=c(3,2), mar=c(0,3,2,1), oma=c(0.5,0.5,0.5,0.5), mgp=c(2,0.5,0))
plot(meas25_edit[, "JULIAN_DAYS"],meas25_edit[,"HP_CH4_sum"], xaxt="n", xlab="", ylab="ppm CH4", main="Large Chamber 6 CH4", pch=20)
fit1<-lm(meas25_edit[,"HR_CH4_sum"]~meas25_edit[, "TIME_EDIT"])
abline(fit1)
par(mar=c(3,3,0,1))
plot(meas25_edit[, "JULIAN_DAYS"], meas25_edit[,"HP_Delta_iCH4_30s"], xlab="Julian Days", ylab="Delta iCH4", pch=20, col="red")
par(mar=c(3,3,0.5,1))
plot(meas25_edit[, "HP_CH4_Inverse"], meas25_edit[,"HP_Delta_iCH4_30s"], ylab="Delta 13C-CH4", xlab="1/[CH4]", pch=20, col="blue")
fit1<-lm(meas25_edit[,"HP_Delta_iCH4_30s"]~meas25_edit[, "HP_CH4_Inverse"])
summary(fit1)#slope parameters of methane keeling plot
abline(fit1)

#CO2 FLUXES
par(mar=c(0,3,2,1))
plot(meas25_edit[, "JULIAN_DAYS"], meas25_edit[,"CO2_sum"], xaxt="n", xlab="", ylab="ppm CO2", main="Large Chamber 6 CO2", pch=20)
fit1<-lm(meas25_edit[,"CO2_sum"]~meas25_edit[, "JULIAN_DAYS"])
abline(fit1)
par(mar=c(3,3,0,1))
plot(meas25_edit[, "JULIAN_DAYS"], meas25_edit[,"Delta_30s_iCO2"], xlab="Julian Days", ylab="Delta iCO2", pch=20, col="red")
par(mar=c(3,3,0.5,1))
plot(meas25_edit[, "CO2_Inverse"], meas25_edit[,"Delta_30s_iCO2"], ylab="Delta 13C-CO2", xlab="1/[CO2]", pch=20, col="blue")
fit1<-lm(meas25_edit[,"Delta_30s_iCO2"]~meas25_edit[, "CO2_Inverse"])
summary(fit1)
abline(fit1)