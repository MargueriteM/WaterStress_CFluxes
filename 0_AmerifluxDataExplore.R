# Script to explore Ameriflux data prior to ReddProc preparation
 #test
# load libraries
library(tidyverse)
library(bit64)
library(data.table)
library(lubridate)
library(dplyr)
library(zoo)

setwd("C:/Users/mebeckage/OneDrive - The University of Texas at El Paso/Mauritz Lab - ONAQ_Data")


# Read in ONAQ data from Ameriflux download (2017-2024)
ec <- read_csv("AMF_US-xNQ_BASE_HH_10-5.csv",
               skip = 2,
               col_types = "cc",
               na = c("-9999", "NA")) 


# pick all the variables from TA_1_1_1 onward
biometvars <- list(colnames(ec[,76:186]))

# Convert timestamps to POSIXct format
# SHOULD WE USE TIMESTAMP START or END????
ec <- ec %>% 
  mutate(datetime = ymd_hm(TIMESTAMP_END),
         year=year(datetime),
         month=month(datetime),
         doy=yday(datetime))


# create long format cdata
ec_ts_long <- ec %>%
  select(datetime,contains("TS"))%>%
  pivot_longer( !datetime, 
    names_to = "variable",
    values_to = "value")%>%
  separate_wider_position(variable, c(1,1,1,probe_id = 1, 1, probe_pos = 1),too_many="drop",cols_remove = FALSE)

# graph timeseries of NEE (FC) 
ggplot(ec, aes(doy, FC))+
  geom_line()+
  facet_grid(.~year)

# no data in 2017 and 2018, graph only after
# graph timeseries of NEE (FC) 
ec %>%
  filter(year>2018 & year<2021) %>%
  ggplot(., aes(datetime, FC))+
  geom_line()+
  facet_grid(.~year, scales="free_x")

# graph jan of all years
ec %>% 
  filter(month==1)%>%
  ggplot(., aes(doy, FC, colour=factor(year)))+
  geom_point()

# graph just one month
ec %>% 
  filter(year==2019 & month==7)%>%
  ggplot(., aes(datetime, FC, colour=factor(year)))+
  geom_point(size=0.5)+
  geom_line()

# look at data for July 2019 -June 2020 which is when we need the data
ec %>%
  filter(datetime > as.Date("2019-07-01") & datetime <= as.Date("2020-06-30"))%>%
  ggplot(., aes(doy, FC))+
  geom_point()+
  facet_grid(.~year)

# graph FC and NEE_PI
# 1:1
ggplot(ec, aes(FC, NEE_PI))+
  geom_point()+
  geom_abline(intercept=0,slope=1)


# graph timeseries of NEE (FC and NEE_PI)
ggplot(ec, aes(x=doy))+
  geom_line(aes(y=FC, color="FC"))+
  geom_line(aes(y=NEE_PI, color="NEE_PI"))+
  facet_grid(.~year)

# graph timeseries of LE
# no data in 2017 and 2018, graph only after
ec %>%
  filter(year>2018) %>%
  ggplot(., aes(doy, LE))+
  geom_line()+
  facet_grid(.~year)

# graph timeseries of LE
# no data in 2017 and 2018, graph only 2019 and 2020
ec %>%
  filter(year>2018 & year<2021) %>%
  ggplot(., aes(doy, LE))+
  geom_line()+
  facet_grid(.~year)

# graph SW In 
ggplot(ec, aes(x=doy))+
  geom_line(aes(y=SW_IN_1_1_1, color="SW_IN_1_1_1"))+
  #geom_line(aes(y=SW_IN_1_1_2, color="SW_IN_1_1_2"))+
  geom_line(aes(y=SW_IN_1_1_3, color="SW_IN_1_1_3"))+
  facet_grid(.~year)

# graph TA 
ggplot(ec, aes(x=doy))+
  geom_line(aes(y=TA_1_1_1, color="TA_1_1_1"))+
  geom_line(aes(y=TA_1_1_2, color="TA_1_1_2"))+
  facet_grid(.~year)

ggplot(ec, aes(x=doy))+
  geom_line(aes(y=TA_1_1_2, color="TA_1_1_2"))+
 # geom_line(aes(y=TA_1_2_1, color="TA_1_2_1"))+
  #geom_line(aes(y=TA_1_3_1, color="TA_1_3_1"))+
  geom_line(aes(y=TA_1_4_1, color="TA_1_4_1"))+
  facet_grid(.~year)

# graph TS
ggplot(ec_ts_long, aes(x=datetime, y=value, colour = probe_pos)) + 
  geom_line() + 
  facet_wrap(probe_id ~.)

# calculate average for TS probes 1-4 across all depths
ec_ts_mean <- ec_ts_long %>%
  filter(probe_id < 5) %>%
  group_by(datetime) %>%
  summarise(ts_mean = mean(value, na.rm = TRUE))

# graph ts_mean to check data
ggplot(ec_ts_mean, aes(x=datetime, y = ts_mean)) + geom_line()

ec <- left_join(ec, ec_ts_mean, by="datetime")

ec <- as.data.table(ec)

# graph sw IN
ec %>% #
ggplot(.)+
  geom_line(aes(datetime,SW_IN_1_1_1))
facet_grid(.~year)

# create NIGHT variable
ec <- ec %>% 
  mutate(NIGHT = case_when(SW_IN_1_1_1 > 12 | PPFD_IN_1_1_1 > 100 ~ "daytime",
                     SW_IN_1_1_1 <= 12 | PPFD_IN_1_1_1 <= 100 ~ "nighttime"))
# check NIGHT variable
ec %>% #
 # filter((year == 2019 | year == 2020)) %>%
  filter(is.na(NIGHT)) %>%
  ggplot(.)+
  geom_point(aes(doy,FC, color = NIGHT)) + 
  facet_grid(.~ year)


# APPLY ROLLING MEANS APPROACH TO FILTER Fc, LE, H
# use rollmeanr from zoo
# 3 days = (24/0.5)*3 = 144
# exclude the level 1 filtered data that removes QC code==2, bad AGC, and values outside +/-30 range
ec[, ':=' (FC_rollmean3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              FC_rollsd3 = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              FC_rollmean5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              FC_rollsd5 = rollapply(FC, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
                              FC_rollmean7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              FC_rollsd7 = rollapply(FC, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]


ec[, ':=' (LE_rollmean3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           LE_rollsd3 = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
           LE_rollmean5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           LE_rollsd5 = rollapply(LE, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
           LE_rollmean7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           LE_rollsd7 = rollapply(LE, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]

ec[, ':=' (H_rollmean3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           H_rollsd3 = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right"),
           H_rollmean5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           H_rollsd5 = rollapply(H, width=(24/0.5)*5, fill=0, FUN=sd, na.rm=TRUE, align="right"),
           H_rollmean7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           H_rollsd7 = rollapply(H, width=(24/0.5)*7, fill=0, FUN=sd, na.rm=TRUE, align="right"))]


# also calculate a 3 day moving mean and SD for daytime and nighttime
ec[, ':=' (FC_rollmean3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
                              FC_rollsd3_daynight = rollapply(FC, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
         by=NIGHT]

ec[, ':=' (LE_rollmean3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           LE_rollsd3_daynight = rollapply(LE, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
   by=NIGHT]

ec[, ':=' (H_rollmean3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=mean, na.rm=TRUE, align="right"),
           H_rollsd3_daynight = rollapply(H, width=(24/0.5)*3, fill=0, FUN=sd, na.rm=TRUE, align="right")),
   by=NIGHT]

threshold <- 3

# graph the 3 and 5 day SD ribbons around measured flux_add
ggplot(ec[year > 2018, ])+
  geom_line(aes(yday(datetime), H))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=yday(datetime), ymin=H_rollmean3-threshold*H_rollsd3, ymax=H_rollmean3+threshold*H_rollsd3), alpha=0.5)+
 # geom_ribbon(aes(x=yday(datetime), ymin=FC_rollmean7-threshold*FC_rollsd7, ymax=FC_rollmean7+threshold*FC_rollsd7), colour="blue",alpha=0.3)+
  
  facet_grid(year(datetime)~.)

# graph the 3 day SD ribbons around measured flux_add by day/night
ggplot(ec[year > 2018,])+
  geom_line(aes(datetime, H))+
  #geom_line(aes(DOY_START, FC_rollmean), colour="green")+
  geom_ribbon(aes(x=datetime, ymin=H_rollmean3_daynight-threshold*H_rollsd3_daynight,
                  ymax=H_rollmean3_daynight+threshold*H_rollsd3_daynight, fill=factor(NIGHT)), alpha=0.5)+
  facet_grid(NIGHT~.)

# mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
# mark when NIGHT is NA for removal 
ec[,filter_fc_roll := 0L]
ec[,filter_le_roll := 0L]
ec[,filter_h_roll := 0L]

ec[is.na(NIGHT), filter_fc_roll := 1L]
ec[FC>FC_rollmean3+threshold*FC_rollsd3|FC<FC_rollmean3-threshold*FC_rollsd3, filter_fc_roll := 1L]

ec[is.na(NIGHT), filter_le_roll := 1L]
ec[LE>LE_rollmean3+threshold*LE_rollsd3|LE<LE_rollmean3-threshold*LE_rollsd3, filter_le_roll := 1L]

ec[is.na(NIGHT), filter_h_roll := 1L]
ec[H>H_rollmean3+threshold*H_rollsd3|H<H_rollmean3-threshold*H_rollsd3, filter_h_roll := 1L]


# by Day/Night mark any Fc>3*FCrollsd3 (3 day moving SD) for removal
ec[,filter_fc_roll_daynight := 0L]
ec[is.na(NIGHT), filter_fc_roll_daynight := 1L]
ec[FC>FC_rollmean3_daynight+threshold*FC_rollsd3_daynight|
           FC<FC_rollmean3_daynight-threshold*FC_rollsd3_daynight, filter_fc_roll_daynight := 1L]

# by Day/Night mark any Le>3*FCrollsd3 (3 day moving SD) for removal
ec[,filter_le_roll_daynight := 0L]
ec[is.na(NIGHT), filter_le_roll_daynight := 1L]
ec[LE>LE_rollmean3_daynight+threshold*LE_rollsd3_daynight|
     LE<LE_rollmean3_daynight-threshold*LE_rollsd3_daynight, filter_le_roll_daynight := 1L]

# by Day/Night mark any H>3*FCrollsd3 (3 day moving SD) for removal
ec[,filter_h_roll_daynight := 0L]
ec[is.na(NIGHT), filter_h_roll_daynight := 1L]
ec[H>H_rollmean3_daynight+threshold*H_rollsd3_daynight|
     H<H_rollmean3_daynight-threshold*H_rollsd3_daynight, filter_h_roll_daynight := 1L]

# view the marked fluxes in ~25 day chunks for day/night
ggplot(ec[year > 2018 & year < 2021 & filter_h_roll_daynight!=1&doy>=1&doy<=60,], aes(doy, H))+
  geom_line()+
  geom_point(aes(colour=factor(filter_h_roll_daynight)), size=0.5) + 
  facet_grid(.~ year)

# graph with the fluxes from the 3SD 3day day/night filter removed
ggplot(ec[filter_h_roll_daynight==0,], aes(datetime, H))+
  geom_line()

# creat filtered copy of ec
ec_filter_sd <- copy(ec[year > 2018,])

#drop the out of bounds FC data points                                    
ec_filter_sd[filter_fc_roll_daynight!=0 | FC < (-20), FC := NA]
ec_filter_sd[filter_le_roll_daynight!=0 , LE := NA]
ec_filter_sd[filter_h_roll_daynight!=0 , H := NA]

# filter datetime into half hour segments, and display by the half hour (0.0,0.5, 1.0,1.5, etc)
# create copy of ec and covert to data.table 
ec_time <- as.data.table(ec_filter_sd)

# Extract date part
ec_time[, date := as.Date(datetime)]

# this checks if there are any days that have < 48 values
ec_time[, .N, by = .(date = as.Date(datetime))]#[N != 48]


# filter dates so only includes days that have 48 values
complete_dates <- ec_time[, .N, by = date][N == 48, date]
ec_time <- ec_time[date %in% complete_dates]

# Assign Hour values: 0.5 to 24.0 (48 steps per day)
ec_time[, Hour := seq(0.5, 24, by = 0.5), by = date]


# add new Hour column to ec
ec_filter_sd <- merge(ec_filter_sd, ec_time[, .(datetime, Hour)], by = "datetime")

# create data with variables needed for ReddyProc partitioning
#ec2 = ec_filter_sd[c("year","doy", "Hour", "FC", "LE","H","SW_IN_1_1_1", "TA_1_1_2","ts_mean", "RH", "VPD_PI", "USTAR")]

ec2 <- as.data.frame(ec_filter_sd)[c("year","doy", "Hour", "FC", "LE","H","SW_IN_1_1_1", 
                                     "TA_1_1_2","ts_mean", "RH", "VPD_PI", "USTAR")]


###################################################################
# create a list of names to later subset biomet variables
# first, see all column names
colnames (ec)

# see which variables were selected
biometvars

# select datetime column and all columns selected in biomet vars
# grab doy, year, hour 
biomet <- ec_filter_sd %>% select(c(year, doy, Hour, biometvars[[1]]))

# save file in for biomet
write.csv(biomet, "ONAQ_biomet.csv", row.names = FALSE)
########################################################################

# save file in ReddyProc format for online tool in R
write.csv(ec2, "ONAQ_needs_partitioning.csv", row.names = FALSE)
