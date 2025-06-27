# Script to explore Ameriflux data prior to ReddProc preparation
 #test
# load libraries
library(tidyverse)
library(bit64)
library(data.table)
library(lubridate)

<<<<<<< HEAD
setwd("C:/Users/mebeckage/OneDrive - The University of Texas at El Paso/Mauritz Lab - ONAQ_Data")

=======
 
>>>>>>> master
# Read in ONAQ data from Ameriflux download (2017-2024)
ec <- read_csv("AMF_US-xNQ_BASE_HH_10-5.csv",
               skip = 2,
               col_types = "cc",
               na = c("-9999", "NA")) 

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

# filter datetime into half hour segments, and display by the half hour (0.0,0.5, 1.0,1.5, etc)
# create copy of ec and covert to data.table 
ec_time <- as.data.table(ec)

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
ec <- merge(ec, ec_time[, .(datetime, Hour)], by = "datetime")

# create data with variables needed for ReddyProc partitioning
ec2 = ec[c("year","doy", "Hour", "FC", "LE","H","SW_IN_1_1_1", "TA_1_1_2","ts_mean", "RH", "VPD_PI", "USTAR")]

# save file in ReddyProc format for online tool in R
write.csv(ec2, "ONAQ_needs_partitioning.csv", row.names = FALSE)
