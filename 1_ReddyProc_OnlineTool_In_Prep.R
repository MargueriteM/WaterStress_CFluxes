# Prepare Data for Online ReddyProc Partitioning tool

# load libraries
library(dplyr)
library(ggplot2)

setwd("C:/Users/mebeckage/OneDrive - The University of Texas at El Paso/Mauritz Lab - ONAQ_Data")

reddy.in <- read.csv("ONAQ_needs_partitioning.csv", header=TRUE)

# rename columns to ReddyProc parameters
reddy.in <- rename(reddy.in, Year=year) 
reddy.in <- rename(reddy.in, DoY=doy)
reddy.in <- rename(reddy.in, NEE=FC)
reddy.in <- rename(reddy.in, Rg=SW_IN_1_1_1)
reddy.in <- rename(reddy.in, Tair=TA_1_1_2)
reddy.in <- rename(reddy.in, Tsoil=ts_mean)
reddy.in <- rename(reddy.in, rH=RH)
reddy.in <- rename(reddy.in, VPD=VPD_PI)
reddy.in <- rename(reddy.in, Ustar=USTAR)

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
ggplot(reddy.in, aes(DoY, Tsoil))+geom_line()
ggplot(reddy.in, aes(DoY, rH))+geom_line()
ggplot(reddy.in, aes(DoY, VPD))+geom_line()
ggplot(reddy.in, aes(DoY, Ustar))+geom_line()

# function to save data in correct format for online Input

saveyear <- function(data,startyear,endyear) {
  
  for (i in startyear:endyear){
    # subset each year
    data1 <- subset(data, Year==i)
    
    data1 <- data1 %>% select(Year,DoY,Hour,NEE, LE, H, Rg, Tair, Tsoil, rH, VPD, Ustar)
    
    data.units <- c("-","-","-","umolm-2s-1", "Wm-2","Wm-2","Wm-2","degC","degC","%","hPa","ms-1")
    t
    data.final <- rbind(data.units,data1)
    
    # save with columns in prescribed order
    write.table (data.final,
                 file=paste("ONAQ_ReddyProc_Input_",i, ".txt",sep=""),
                 sep =' ', dec='.', na="-9999", row.names=FALSE)
  }} 

saveyear(reddy.in, 2017,2024)
