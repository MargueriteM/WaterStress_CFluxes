# Script to explore Ameriflux data prior to ReddProc preparation

# load libraries
library(tidyverse)


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
