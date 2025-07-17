# Work with data from ReddyProc Online Tool Output

# load libraries
library(data.table)
library(ggplot2)
library(cowplot)
library(viridis)
library(dplyr)
library(bigleaf)
library(tidyr)
library(scales)
library(patchwork)

setwd("C:/Users/mebeckage/OneDrive - The University of Texas at El Paso/Mauritz Lab - ONAQ_Data")

# import
rp.units <- (fread("./REddyResults_ONAQ_2019_20250625_293430638/output.txt",
                   header=TRUE))[1,]

flux.rp2019 <- fread("./REddyResults_ONAQ_2019_2_20250703_776345013/output.txt",
                 header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                 col.names = colnames(rp.units))

flux.rp2020 <- fread("./REddyResults_ONAQ_2020_2_20250703_550277174/output.txt",
                     header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                     col.names = colnames(rp.units))

flux.rp2021 <- fread("./REddyResults_ONAQ_2021_20250625_196531586/output.txt",
                     header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                     col.names = colnames(rp.units))

flux.rp2023 <- fread("./REddyResults_ONAQ_2023_20250625_831786507/output.txt",
                     header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                     col.names = colnames(rp.units))

flux.rp2024 <- fread("./REddyResults_ONAQ_2024_20250625_942226779/output.txt",
                     header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                     col.names = colnames(rp.units))

flux.rp <- rbind(flux.rp2019,flux.rp2020) #, flux.rp2021, flux.rp2023, flux.rp2024)


# for some reason na.strings won't recognize the -9999
flux.rp[flux.rp == -9999] <- NA

# import biomet data for merging
biomet <- read.csv("ONAQ_biomet.csv", header=TRUE)

# format biomet columns to watch flux.rp column names
biomet <- biomet%>% 
  rename (Year = year,
   DoY = doy)

# merge flux data with biomet data
flux.biomet <- left_join(flux.rp, biomet, by=c("Year", "DoY", "Hour"))

###################################################################
# create a copy of the original GPP_DT_U50 column for filtered GPP
# and add to flux.rp
flux.rp[, GPP_DT_filtered := GPP_DT_U50]

# drop the uncertain daytime GPP values in filtered column
flux.rp[Year == 2019 & DoY >= 31 & DoY <= 56, GPP_DT_filtered := NA]
flux.rp[Year == 2019 & DoY >= 178 & DoY <= 216, GPP_DT_filtered := NA]
flux.rp[Year == 2019 & DoY >= 300, GPP_DT_filtered := NA]
flux.rp[Year == 2019 & DoY <= 50, GPP_DT_filtered := NA]
flux.rp[Year == 2019 & DoY >= 303 & DoY <= 333, GPP_DT_filtered := NA]
flux.rp[Year == 2020 & DoY >= 201 & DoY <= 260, GPP_DT_filtered := NA]
flux.rp[Year == 2020 & DoY >= 200 & DoY <= 300, GPP_DT_filtered := NA]

# plot original GPP
p1 <- ggplot(subset(flux.rp,FP_qc != 2), aes(DoY,GPP_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))
p1

# plot filtered GPP
p2 <- ggplot(subset(flux.rp,FP_qc != 2), aes(DoY,GPP_DT_filtered, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))
p2

# stack  graphs vertically to compare
p1 / p2
###################################################################


# plot with no U* filter or gapfill
ggplot(flux.rp, aes(DoY,NEE_orig))+
  geom_line()+
  facet_grid(Year~.)


###################################################################
# convert LE to ET using bigleaf
flux.rp <- flux.rp %>%
  mutate(ET = LE.to.ET(LE_f, Tair_f))

# convert ET to correct units, mm per half hour. 
# create new ET_mm column with these units and add to flux.rp
flux.rp <- flux.rp %>%
  mutate(ET_mm = ET * 1800) 
###################################################################


# plot NEE (no Ustar filter) with gapfill highlighted
fig_nee <- ggplot(subset(flux.rp), aes(DoY,NEE_orig))+
  geom_point(size=0.4)+
  geom_point(aes(y=NEE_U50_f),data=subset(flux.rp, is.na(NEE_orig)),colour="red",size=0.25)+
  facet_grid(.~Year)
fig_nee

# graph Ustar filtered NEE with 50th percentile and gap-filled
fig_nee_fill <- ggplot((flux.rp))+
  geom_line(aes(DoY,NEE_U50_f))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_grid(.~Year)
fig_nee_fill

# graph precipitation events by year
fig_precip <- ggplot((flux.biomet))+
  geom_line(aes(DoY,P_1_1_1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_grid(.~Year)
fig_precip

# graph mean Tair  by year
fig_tair <- ggplot((flux.biomet))+
  geom_line(aes(DoY,Tair))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_grid(.~Year)
fig_tair

# plot le
fig_le <- ggplot((flux.rp))+
  geom_line(aes(DoY,LE_f)) +
  facet_grid(.~Year)
fig_le


plot_grid(fig_nee_fill, fig_reco, fig_gpp, fig_le, nrow=4, align="v")
plot_grid(fig_nee_fill, fig_precip, nrow=2, align="v")
plot_grid(fig_nee_fill, fig_tair, nrow=2, align="v")
plot_grid(fig_nee_fill, fig_precip, fig_tair, nrow=3, align="v")

###################################################################
# look closer at NEE by month
# graph Ustar filtered NEE with 50th percentile and gap-filled
fig_2019data <- flux.rp %>% 
  filter(Year==2019) %>%
  filter (DoY > 212) %>%
  ggplot(.)+
  geom_line(aes(DoY + Hour/24,NEE_U50_f))+
  geom_point(aes(DoY + Hour/24,NEE_U50_f))
  facet_grid(.~Year)
  fig_2019data
  
 fig_2020data <- flux.biomet %>% 
    filter(Year==2020) %>%
    filter (DoY < 152) %>%
    ggplot(.)+
    geom_line(aes(DoY + Hour/24,NEE_U50_f))+
    geom_point(aes(DoY + Hour/24,NEE_U50_f))
  facet_grid(.~Year)
  fig_2020data
  
  
  # look closer at LE by month
  # graph Ustar filtered NEE with 50th percentile and gap-filled
  fig_2019data_LE <- flux.rp %>% 
    filter(Year==2019) %>%
    filter (DoY > 212) %>%
    ggplot(.)+
    geom_line(aes(DoY + Hour/24,LE_f))+
    geom_point(aes(DoY + Hour/24,LE_f))
  facet_grid(.~Year)
  fig_2019data_LE
  
  fig_2020data_LE <- flux.rp %>% 
    filter(Year==2020) %>%
    filter (DoY < 152) %>%
    ggplot(.)+
    geom_line(aes(DoY + Hour/24,LE_f))+
    geom_point(aes(DoY + Hour/24,LE_f))
  facet_grid(.~Year)
  fig_2020data_LE
  
  # look at radiation by year/month
  flux.biomet %>% 
   # filter(Year==2019) %>%
   # filter (DoY > 212) %>%
    ggplot(.)+
    geom_line(aes(DoY + Hour/24,SW_IN_1_1_1))+
    geom_point(aes(DoY + Hour/24,SW_IN_1_1_1))
  facet_grid(.~Year)
  
  plot_grid(fig_2019data, fig_precip2019, nrow=2, align="v")
 
  plot_grid(fig_2020data, fig_precip2020, nrow=2, align="v")
  
  plot_grid(fig_2019data, fig_2020data, nrow=2, align="v")
  
  
  # look at precipitation by year/month
  flux.biomet %>% 
   # filter(Year==2021) %>%
   # filter (DoY > 190 & DoY < 200) %>%
    ggplot(.)+
    geom_line(aes(DoY + Hour/24,P_1_1_1))+
    geom_point(aes(DoY + Hour/24,P_1_1_1))
    facet_grid(.~Year)
############################################################ 

# graph Ustar filtered Reco with 50th percentile and gap-filled
fig_reco <- ggplot(subset(flux.rp), aes(DoY,Reco_U50, colour = "nightime"))+geom_line()+
  geom_line(aes(y=Reco_DT_U50, colour = "daytime"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_grid(.~Year)
fig_reco


# plot daytime Reco with qc code (not sure what to do with this code: exlude 1?)
ggplot(subset(flux.rp), aes(DoY,Reco_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year)

# plot 2020 daytime Reco 
ggplot(subset(flux.rp, Year == 2020 & FP_qc == 0), aes(DoY, Reco_DT_U50, colour=factor(FP_qc))) +
  geom_point(size=1) +
  facet_grid(. ~ Year)


# plot daytime Rec, exclude the 2s
ggplot(subset(flux.rp, FP_qc != 2), aes(DoY, Reco_DT_U50, colour=factor(FP_qc))) +
  geom_point(size=1) +
  facet_grid(. ~ Year)

# plot daytime Rec, exclude the 1s and 2s
ggplot(subset(flux.rp, FP_qc == 0), aes(DoY, Reco_DT_U50, colour=factor(FP_qc))) +
  geom_point(size=1) +
  facet_grid(. ~ Year)





# graph Ustar filtered GPP Day time and night time with 50th percentile and gap-filled
fig_gpp <- ggplot(flux.rp, aes(DoY,GPP_U50_f, colour = "nightime"))+
  geom_line()+
  geom_line(aes(y=GPP_DT_U50, colour = "daytime"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) + 
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))
fig_gpp 


#  GPP Daytime 50th percentile 
ggplot(subset(flux.rp), aes(DoY,GPP_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))

# look at daytime GPP by month/year
flux.rp %>% 
  # filter(Year==2020) %>%
  #filter (DoY > 338 & DoY < 365) %>%
   filter (FP_qc !=2) %>%
  ggplot(., aes(DoY + Hour/24,GPP_DT_U50, colour=factor(FP_qc)))+
  geom_line(size=1)+
  facet_grid(.~Year) 
  #ylim(c(ymin = -2, ymax = 2))

# look at air temperature by month/year
flux.rp %>% 
  #filter(Year==2019) %>%
 # filter (DoY > 178 & DoY < 196) %>%
  #filter (FP_qc !=2) %>%
  ggplot(., aes(DoY + Hour/24,Tair_f))+
  geom_line(size=1)+
  facet_grid(.~Year) 
#ylim(c(ymin = -1, ymax = 8))


###########################
# Day time GPP with 1s and 2s removed
ggplot(subset(flux.rp,FP_qc == 0), aes(DoY,GPP_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))

# daytime GPP with 2s removed
ggplot(subset(flux.rp,FP_qc != 2), aes(DoY,GPP_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))

# night time GPP with 1s and 2s removed
ggplot(subset(flux.rp,FP_qc == 0), aes(DoY,GPP_U50_f, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))

# night time GPP with 2s removed
ggplot(subset(flux.rp,FP_qc != 2), aes(DoY,GPP_U50_f, colour=factor(FP_qc)))+
  geom_point(size=1)+
  facet_grid(.~Year) + 
  ylim(c(ymin = -100, ymax = 100))
####################################\


# look through GPP with year/month
flux.rp %>% 
  filter(Year==2019) %>%
  filter (DoY > 200 & DoY < 220) %>%
  ggplot(., aes(DoY + Hour/24,GPP_U50_f))+
  geom_line(size=1)+
  geom_point(size=1)+
  facet_grid(.~Year) 
#ylim(c(ymin = -2, ymax = 2))



# plot NEE with different U* quantiles
ggplot(subset(flux.rp), aes(x=DoY))+
  geom_line(aes(y=NEE_U05_f, colour="NEE_U05_f"))+
  geom_line(aes(y=NEE_U50_f, colour="NEE_U50_f"))+
  geom_line(aes(y=NEE_U95_f, colour="NEE_U95_f"))


# heat map of gap-filled NEE data
ggplot(flux.rp,
       aes(DoY,Hour,fill=NEE_U50_f))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name=expression(paste('C',O[2],' flux',sep='')))+
  facet_grid(Year~.)+
  scale_y_continuous(breaks=c(0,12,23),
                     labels=c("00:00","12:00","23:00"),
                     expand=c(0,0))+
  scale_x_continuous(breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,367),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand=c(0,0))+
  labs(title= expression("Half-hourly flux (μmol C" *O[2]* m^-2* "se" *c^-1*")"), x="Day", y="Half-hour") +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=8),
    axis.title=element_text(size=11),
    strip.background = element_blank(),
    strip.text=element_text(size=10),
    axis.ticks=element_blank(),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    panel.grid=element_blank(),
    panel.background=element_rect(fill="white"))

# heat map of gap-filled LE data
ggplot(flux.rp,
       aes(DoY,Hour,fill=LE_f))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="LE")+
  facet_grid(Year~.)+
  scale_y_continuous(breaks=c(0,12,23),
                     labels=c("00:00","12:00","23:00"),
                     expand=c(0,0))+
  scale_x_continuous(breaks =c(31,61,91,121,151,181,211,241,271,301,331,361),limits=c(1,367),
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand=c(0,0))+
  labs(title= expression("Half-hourly LE (W" *m^-2* ")"), x="Day", y="Half-hour") +
  theme(
    plot.title=element_text(size = 14),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=8),
    axis.title=element_text(size=11),
    strip.background = element_blank(),
    strip.text=element_text(size=10),
    axis.ticks=element_blank(),
    legend.title=element_text(size=10),
    legend.text=element_text(size=9),
    panel.grid=element_blank(),
    panel.background=element_rect(fill="white"))

#######################################################
#daily sum of GPP DT, GPP NT
gpp_daily <- flux.rp %>% 
  group_by(Year,DoY) %>% 
  summarise(GPP_DT = sum(GPP_DT_filtered),GPP_NT = sum(GPP_U50_f))

#plot daily sum of GPP daytime and GPP nighttime
ggplot(gpp_daily, aes(x = DoY)) +
  geom_point(aes(y = GPP_DT, color = "GPP_DT")) +
  geom_point(aes(y = GPP_NT, color = "GPP_NT")) +
  labs(title = "Daily sum of GPP DT and GPP NT",
       y = "Daily sum",
       color = "Variable") +
  theme_minimal()

#plot points
gpp_long <- gpp_daily %>%
  pivot_longer(cols = c(GPP_DT, GPP_NT), names_to = "Type", values_to = "DailySum")

ggplot(gpp_long, aes(x = DoY, y = DailySum, color = Type)) +
  geom_line() +
  labs(title = "Daily sum of GPP DT and GPP NT",
       y = "Daily sum") +
  theme_minimal()


# daily mean for all other biomet variables 
# (single variables, ie: 1_1_1, 1_2_1, etc)

daily_biomet_means <- flux.biomet %>%
  group_by(Year, DoY) %>%
  summarise(across(
    .cols = -c(`Date Time`, Hour),
    .fns = mean,
    na.rm = TRUE
  ))

# daily ET
et_daily <- flux.rp %>% 
  group_by(Year, DoY) %>% 
  summarise(et = sum(ET_mm, na.rm = TRUE))

# daily precipitation
precip_daily <- flux.biomet %>% 
  group_by(Year, DoY) %>% 
  summarise(precipitation = sum(P_1_1_1, na.rm = TRUE))

# combine et and precip for plotting
combined_daily <- left_join(et_daily, precip_daily, by = c("Year", "DoY"))

# convert to logng format
combined_long <- combined_daily %>%
  pivot_longer(cols = c(et, precipitation),
               names_to = "Variable",
               values_to = "Value")


# plot daily et
p_et <- ggplot(et_daily, aes(x = DoY, y = et)) +
  geom_line(color = "forestgreen", size = 0.8) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Daily ET", x = "Day of Year", y = "ET (mm)") +
  theme_minimal()
p_et

# plot daily precip
p_precip <- ggplot(precip_daily, aes(x = DoY, y = precipitation)) +
  geom_col(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Daily precipitation", x = "Day of Year", y = "Precipitation (mm)") +
  theme_minimal()

# plot daily filtered GPP
p_gpp <- ggplot(gpp_daily, aes(x = DoY, y = GPP_DT)) +
  geom_line(color = "darkorange", size = 0.8) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Daily GPP (Daytime)", x = "Day of Year", y = "GPP_DT") +
  theme_minimal()

# stack precip and et graphs to compare by day of year
p_precip / p_et / p_gpp


# calculate annual cumulative ET
annual_et_cum <- et_daily %>%
  group_by(Year) %>%
  summarise(cumulative_et = sum(et, na.rm = TRUE))
annual_et_cum

# calculate annual cumulative precipitation
annual_precip_cum <- precip_daily %>%
  group_by(Year) %>%
  summarise(cumulative_precip = sum(precipitation, na.rm = TRUE))
annual_precip_cum


# graph annual cumulative ET
et_daily %>%
  group_by(Year) %>%
  arrange(DoY) %>%
  mutate(cumulative_et = cumsum(et)) %>%
  ggplot(aes(x = DoY, y = cumulative_et)) +
  geom_line(color = "forestgreen") +
  facet_wrap(~ Year) +
  labs(title = "Cumulative annual ET",
       x = "Day of Year",
       y = "Cumulative ET (mm)") +
  theme_minimal()

# graph annual cumulative GPP filtered
gpp_daily %>%
  group_by(Year) %>%
  arrange(DoY) %>%
  mutate(cumulative_gpp = cumsum(GPP_DT)) %>%
  ggplot(aes(x = DoY, y = cumulative_gpp)) +
  geom_line(color = "forestgreen") +
  facet_wrap(~ Year) +
  labs(title = "Cumulative annual GPP DT Filtered",
       x = "Day of Year",
       y = "Cumulative DT GPP") +
  theme_minimal()

