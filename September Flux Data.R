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
meas1<-subset(data1, data1[, "MEASUREMENT_ID"]=="1") #create baby matrix
plot(meas1$JULIAN_DAYS, meas1$HR_CH4_sum) #check tail on graph
dim(meas1)
meas1_edit<-meas1[30:3050,] #creates smaller matrix
plot(meas1_edit$JULIAN_DAYS, meas1_edit$HP_CH4_sum)
fit1<-lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS)
CH4.R2<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$coefficients[2]

plot(meas1_edit$JULIAN_DAYS, meas1_edit$CO2_sum)
fit1<-lm(meas1_edit$CO2_sum~meas1_edit$TIME_EDIT)
CO2.R2<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$coefficients[2]

meas1<-subset(data1, data1[, "MEASUREMENT_ID"]=="1") #create baby matrix
plot(meas1$JULIAN_DAYS, meas1$HR_CH4_sum) #check tail on graph
dim(meas1)
meas1_edit<-meas1[30:3050,] #creates smaller matrix
plot(meas1_edit$JULIAN_DAYS, meas1_edit$HP_CH4_sum)
fit1<-lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS)
CH4.R2<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$adj.r.squared 
CH4.slope<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$coefficients[2]

plot(meas1_edit$JULIAN_DAYS, meas1_edit$CO2_sum)
fit1<-lm(meas1_edit$CO2_sum~meas1_edit$TIME_EDIT)
CO2.R2<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$adj.r.squared 
CO2.slope<-summary(lm(meas1_edit$HP_CH4_sum~meas1_edit$JULIAN_DAYS))$coefficients[2]




meas1data<-c(CH4.R2, CH4.slope, CO2.R2, CO2.slope)

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