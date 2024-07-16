# Prepare Data for Online ReddyProc Partitioning tool

# load libraries
library(dplyr)
library(ggplot2)

reddy.in <- read.csv("xSR_needs_partitioning.csv", header=TRUE)

reddy.in <- rename(reddy.in, rH=RH)

# variables for ReddyProc input
# Year	DoY	Hour	NEE	LE	H	Rg	Tair	Tsoil	rH	VPD	Ustar

# site name: xSR
# LatDeg = 31.91068, LongDeg = -110.83549
# UTC offset: -7

# graph each variable
ggplot(reddy.in, aes(DoY, NEE))+geom_line()
ggplot(reddy.in, aes(DoY, LE))+geom_line()
ggplot(reddy.in, aes(DoY, H))+geom_line()
ggplot(reddy.in, aes(DoY, Tair))+geom_line()
ggplot(reddy.in, aes(DoY, rH))+geom_line()
ggplot(reddy.in, aes(DoY, VPD))+geom_line()
ggplot(reddy.in, aes(DoY, Ustar))+geom_line()

# function to save data in correct format for online Input

saveyear <- function(data,startyear,endyear) {
  
  for (i in startyear:endyear){
    # subset each year
    data1 <- subset(data, Year==i)
    
    data1 <- data1 %>% select(Year,DoY,Hour,NEE, LE, H, Rg, Tair, rH, VPD, Ustar)
    
    data.units <- c("-","-","-","umolm-2s-1", "Wm-2","Wm-2","Wm-2","degC","%","hPa","ms-1")
    
    data.final <- rbind(data.units,data1)
    
    # save with columns in prescribed order
    write.table (data.final,
                 file=paste("xSR_ReddyProc_Input_",i, ".txt",sep=""),
                 sep =' ', dec='.', na="-9999", row.names=FALSE)
  }} 

saveyear(reddy.in, 2023,2023)
