# Read data from MOz and TON sites from Ameriflux downloads
# Exploratory data for Proposal
# Data variable descriptions for AMERIFLUX Subset porduct are at:
# https://fluxnet.org/data/fluxnet2015-dataset/subset-data-product/

# load libraries
library(data.table)
library(ggplot2)
library(cowplot)
library(lubridate)
library(bigleaf)
library(dplyr)
library(tidyr)

# load data
# Tonzi Ranch: https://fluxnet.org/doi/FLUXNET2015/US-Ton
ton <- fread("./AMF_US-Ton_FLUXNET_SUBSET_2001-2021_3-5/AMF_US-Ton_FLUXNET_SUBSET_HH_2001-2021_3-5.csv",
                 header=TRUE,,na.strings=c("-9999", "NA","-"))

# convert timestamp end (summary is for 30mins prior) to datetime
ton[,':='(datetime=ymd_hm(TIMESTAMP_END),
         ET = LE.to.ET(LE_F_MDS,TA_F))]

# view columns
colnames(ton)

# Moflux: https://ameriflux.lbl.gov/data/flux-data-products/fluxnet-publish/
moz <- fread("./AMF_US-MOz_FLUXNET_SUBSET_2004-2019_3-5/AMF_US-MOz_FLUXNET_SUBSET_HH_2004-2019_3-5.csv",
             header=TRUE,,na.strings=c("-9999", "NA","-"))

# convert timestamp end (summary is for 30mins prior) to datetime
# and LE to ET
moz[,':='(datetime=ymd_hm(TIMESTAMP_END),
          ET = LE.to.ET(LE_F_MDS,TA_F))]

# view columns
colnames(moz)

# TONZI
# graph NEE, Reco, GPP for night-time and daytime
# use _50 quantiles
# NEE
p.ton.nee <- ggplot(ton[NEE_VUT_50_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=NEE_VUT_50), linewidth=0.2)+
  labs(y="NEE umol/m2/sec")+
  theme_bw()

# GPP 
p.ton.gpp <- ggplot(ton[NEE_VUT_50_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=GPP_NT_VUT_50), linewidth=0.2, colour="darkgreen")+
  geom_line(aes(y=GPP_DT_VUT_50), linewidth=0.2, alpha=0.75, colour="lightgreen")+
  labs(y="GPP umol/m2/sec")+
  theme_bw()

# Reco
p.ton.reco <- ggplot(ton[NEE_VUT_50_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=RECO_NT_VUT_50), linewidth=0.2, colour="brown")+
  geom_line(aes(y=RECO_DT_VUT_50), linewidth=0.2, alpha=0.75, colour="grey")+
  labs(y="Reco umol/m2/sec")+
  theme_bw()

# graph tonzi data together
plot_grid(p.ton.nee+labs(title="Tonzi")+theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
          p.ton.gpp+theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
          p.ton.reco,
          nrow=3,
          align="hv")

# graph LE and TA
plot_grid(
  ggplot(ton, aes(datetime, LE_F_MDS))+
    geom_line(),
  ggplot(ton, aes(datetime, TA_F))+
    geom_line(),
  nrow=2)


# graph ET
p.ton.et <- ggplot(ton[LE_F_MDS_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=ET), linewidth=0.2, colour="lightblue")+
  labs(y="ET kg/m2/s")+
  theme_bw()

# graph ET and GPP
plot_grid(p.ton.et+labs(title="Tonzi")+theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
          p.ton.gpp,
          nrow=2,
          align="hv")

# graph soil water content by depth
ggplot()+
  geom_line(data=ton[SWC_F_MDS_1_QC<2,],aes(x=datetime,y=SWC_F_MDS_1),colour="lightblue")+
  geom_line(data=ton[SWC_F_MDS_2_QC<2,],aes(x=datetime,y=SWC_F_MDS_2),colour="blue")+
  geom_line(data=ton[SWC_F_MDS_3_QC<2,],aes(x=datetime,y=SWC_F_MDS_3),colour="black")+
  labs(title = "Tonzi", y="Soil Water Content %")+
  theme_bw()

# MOFLUX
# graph NEE, Reco, GPP for night-time and daytime
# use _50 quantiles
# NEE
p.moz.nee <- ggplot(moz[NEE_VUT_50_QC<2 & as.Date(datetime)>=as.Date("2004-07-01"),], aes(x=datetime))+
  geom_line(aes(y=NEE_VUT_50), linewidth=0.2)+
  labs(y="NEE umol/m2/sec")+
  theme_bw()

# GPP 
p.moz.gpp <- ggplot(moz[NEE_VUT_50_QC<2 & as.Date(datetime)>=as.Date("2004-07-01"),], aes(x=datetime))+
  geom_line(aes(y=GPP_NT_VUT_50), linewidth=0.2, colour="darkgreen")+
  geom_line(aes(y=GPP_DT_VUT_50), linewidth=0.2, alpha=0.75, colour="lightgreen")+
  labs(y="GPP umol/m2/sec")+
  theme_bw()

# Reco
p.moz.reco <- ggplot(moz[NEE_VUT_50_QC<2 & as.Date(datetime)>=as.Date("2004-07-01"),], aes(x=datetime))+
  geom_line(aes(y=RECO_NT_VUT_50), linewidth=0.2, colour="brown")+
  geom_line(aes(y=RECO_DT_VUT_50), linewidth=0.2, alpha=0.75, colour="grey")+
  labs(y="Reco umol/m2/sec")+
  theme_bw()

# graph Mo Flux data together
plot_grid(p.moz.nee+labs(title="MoFlux")+theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
          p.moz.gpp+theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
          p.moz.reco,
          nrow=3,
          align="hv")

# graph LE and TA
plot_grid(
  ggplot(moz, aes(datetime, LE_F_MDS))+
  geom_line(),
ggplot(moz, aes(datetime, TA_F))+
  geom_line(),
nrow=2)


# graph ET
p.moz.et <- ggplot(moz[LE_F_MDS_QC<2,] , aes(x=datetime))+
  geom_line(aes(y=ET), linewidth=0.2, colour="lightblue")+
  labs(y="ET kg/m2/s")+
  theme_bw()

# graph ET and GPP
plot_grid(p.moz.et+labs(title="MoFlux")+theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
          p.moz.gpp,
          nrow=2,
          align="hv")

# graph soil water content by depth
p.moz.swc <- ggplot(moz[SWC_F_MDS_1_QC<2,], aes(x=datetime))+
  geom_line(aes(y=SWC_F_MDS_1),colour="lightblue")+
  labs(title = "MoFlux", y="Soil Water Content %")+
theme_bw()

# graph precipitation
p.moz.p <- ggplot(moz[P_F_QC<2,], aes(x=datetime))+
  geom_col(aes(y=P_F),colour="lightblue")+
  labs(title = "MoFlux", y="Precipitation mm")+
  theme_bw()

# graph ET, SWC, precip
plot_grid(p.moz.et, p.moz.swc, p.moz.p, nrow=3, align="v")

# calculate DAILY for Tonzi and Mo Flux
# NEE, GPP, Reco sum to daily gC/m2/s
# ET sum to daily mm
# SWC, TA to daily mean
# P to sum

# tonzi daily
# make values long
ton.value.long <- ton %>%
  select(datetime, NEE_VUT_50, GPP_NT_VUT_50, GPP_DT_VUT_50, RECO_NT_VUT_50,RECO_DT_VUT_50,
         ET,SWC_F_MDS_1, SWC_F_MDS_2, SWC_F_MDS_3, TA_F, P_F) %>%
  pivot_longer(!datetime, names_to="variable", values_to="value")%>%
  mutate(qc_var = case_when (variable %in% c("NEE_VUT_50", "GPP_NT_VUT_50", "GPP_DT_VUT_50",
                                              "RECO_NT_VUT_50", "RECO_DT_VUT_50") ~ "NEE_VUT_50_QC",
                              variable == "ET" ~ "LE_F_MDS_QC",
                              variable == "SWC_F_MDS_1" ~ "SWC_F_MDS_1_QC",
                              variable == "SWC_F_MDS_2" ~ "SWC_F_MDS_2_QC",
                              variable == "SWC_F_MDS_3" ~ "SWC_F_MDS_3_QC",
                              variable == "TA_F" ~ "TA_F_QC",
                              variable == "P_F" ~ "P_F_QC"))
# make qc codes long
ton.qc.long <- ton %>%
  select(datetime, NEE_VUT_50_QC, LE_F_MDS_QC, ET,
         SWC_F_MDS_1_QC, SWC_F_MDS_2_QC, SWC_F_MDS_3_QC, TA_F_QC, P_F_QC) %>%
  pivot_longer(!datetime, names_to="qc_var", values_to="qc_code")

# join values and qc code
ton.long <- left_join(ton.value.long, ton.qc.long, by=c("datetime","qc_var"))

# filter out all qc codes > 2 and dates after "2001-07-01" because earlier data looks weird
ton.wide.filt <- ton.long %>%
  mutate(value = case_when(qc_code>2 ~ NA_real_,
                           TRUE ~ value))%>%
  filter(as.Date(datetime)>=as.Date("2004-07-01")) %>%
  select(datetime, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

# calculate daily sums of Co2 fluxes in umol/m2/sec converted to gC/m2/day,
# ET in kg/m2/day = mm
# drop NA in daily sums and means, include a count column of how many NA/day (complete data count = 48)
ton.daily <- ton.wide.filt %>%
  mutate(date = as.Date(datetime))%>%
  group_by(date) %>%
  summarise(NEE_daily = sum(NEE_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
            NEE_count = sum(!is.na(NEE_VUT_50)),
            GPP_NT_daily = sum(GPP_NT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
            GPP_NT_count = sum(!is.na(GPP_NT_VUT_50)),
            GPP_DT_daily = sum(GPP_DT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
            GPP_DT_count = sum(!is.na(GPP_DT_VUT_50)),
            RECO_NT_daily = sum(RECO_NT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
            RECO_NT_count = sum(!is.na(RECO_NT_VUT_50)),
            RECO_DT_daily = sum(RECO_DT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
            RECO_NT_count = sum(!is.na(RECO_NT_VUT_50)),
            ET_daily = sum(ET*1800, na.rm=TRUE),
            ET_count = sum(!is.na(ET)),
            SWC_mean_1 = mean(SWC_F_MDS_1, na.rm=TRUE),
            SWC_1_count = sum(!is.na(SWC_F_MDS_1)),
            SWC_mean_2 = mean(SWC_F_MDS_2, na.rm=TRUE),
            SWC_2_count = sum(!is.na(SWC_F_MDS_2)),
            SWC_mean_3 = mean(SWC_F_MDS_3, na.rm=TRUE),
            SWC_3_count = sum(!is.na(SWC_F_MDS_3)),
            TA_mean = mean(TA_F, na.rm=TRUE),
            TA_count = sum(!is.na(TA_F)),
            P_sum = sum(P_F, na.rm=TRUE),
            P_count = sum(!is.na(P_F)))

# graph daily fluxes
ggplot(ton.daily, aes(date, NEE_daily))+
  geom_line()

ggplot(ton.daily, aes(date, NEE_count))+
  geom_line()

ggplot(ton.daily, aes(date, GPP_DT_daily))+
  geom_line()

ggplot(ton.daily, aes(date, GPP_DT_count))+
  geom_line()

ggplot(ton.daily, aes(date, GPP_NT_daily))+
  geom_line()

ggplot(ton.daily, aes(date, RECO_DT_daily))+
  geom_line()

ggplot(ton.daily, aes(date, RECO_NT_daily))+
  geom_line()

ggplot(ton.daily, aes(date, ET_daily))+
  geom_line()

ggplot(ton.daily, aes(date, ET_count))+
  geom_line()

ggplot(ton.daily, aes(date, SWC_mean_1))+
  geom_line()

ggplot(ton.daily, aes(date, SWC_mean_2))+
  geom_line()

ggplot(ton.daily, aes(date, SWC_2_count))+
  geom_line()

ggplot(ton.daily, aes(date, SWC_mean_3))+
  geom_line()

ggplot(ton.daily, aes(date, SWC_3_count))+
  geom_line()

ggplot(ton.daily, aes(date, TA_mean))+
  geom_line()

ggplot(ton.daily, aes(date, P_sum))+
  geom_line()

ggplot(ton.daily, aes(date, P_count))+
  geom_line()

###############
# mo flux daily
# make values long
moz.value.long <- moz %>%
  select(datetime, NEE_VUT_50, GPP_NT_VUT_50, GPP_DT_VUT_50, RECO_NT_VUT_50,RECO_DT_VUT_50,
         ET,SWC_F_MDS_1, TA_F, P_F) %>%
  pivot_longer(!datetime, names_to="variable", values_to="value")%>%
  mutate(qc_var = case_when (variable %in% c("NEE_VUT_50", "GPP_NT_VUT_50", "GPP_DT_VUT_50",
                                             "RECO_NT_VUT_50", "RECO_DT_VUT_50") ~ "NEE_VUT_50_QC",
                             variable == "ET" ~ "LE_F_MDS_QC",
                             variable == "SWC_F_MDS_1" ~ "SWC_F_MDS_1_QC",
                             variable == "TA_F" ~ "TA_F_QC",
                             variable == "P_F" ~ "P_F_QC"))
# make qc codes long
moz.qc.long <- moz %>%
  select(datetime, NEE_VUT_50_QC, LE_F_MDS_QC, ET,
         SWC_F_MDS_1_QC, TA_F_QC, P_F_QC) %>%
  pivot_longer(!datetime, names_to="qc_var", values_to="qc_code")

# join values and qc code
moz.long <- left_join(moz.value.long, moz.qc.long, by=c("datetime","qc_var"))

# filter out all qc codes > 2 and dates after "2001-07-01" because earlier data looks weird
moz.wide.filt <- moz.long %>%
  mutate(value = case_when(qc_code>2 ~ NA_real_,
                           TRUE ~ value))%>%
  filter(as.Date(datetime)>=as.Date("2004-07-01")) %>%
  select(datetime, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

# calculate daily sums of Co2 fluxes in umol/m2/sec converted to gC/m2/day,
# ET in kg/m2/day = mm
# drop NA in daily sums and means, include a count column of how many NA/day (complete data count = 48)
moz.daily <- moz.wide.filt %>%
  mutate(date = as.Date(datetime))%>%
  group_by(date) %>%
  summarise(NEE_daily = sum(NEE_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
            NEE_count = sum(!is.na(NEE_VUT_50)),
            GPP_NT_daily = sum(GPP_NT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
           GPP_NT_count = sum(!is.na(GPP_NT_VUT_50)),
            GPP_DT_daily = sum(GPP_DT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
           GPP_DT_count = sum(!is.na(GPP_DT_VUT_50)),
            RECO_NT_daily = sum(RECO_NT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
           RECO_NT_count = sum(!is.na(RECO_NT_VUT_50)),
            RECO_DT_daily = sum(RECO_DT_VUT_50*1800*1*10^-6*12.01, na.rm=TRUE),
          RECO_NT_count = sum(!is.na(RECO_NT_VUT_50)),
            ET_daily = sum(ET*1800, na.rm=TRUE),
          ET_count = sum(!is.na(ET)),
            SWC_mean_1 = mean(SWC_F_MDS_1, na.rm=TRUE),
          SWC_1_count = sum(!is.na(SWC_F_MDS_1)),
            TA_mean = mean(TA_F, na.rm=TRUE),
          TA_count = sum(!is.na(TA_F)),
            P_sum = sum(P_F, na.rm=TRUE),
          P_count = sum(!is.na(P_F)))


# graph daily fluxes
ggplot(moz.daily, aes(date, NEE_daily))+
  geom_line()

ggplot(moz.daily, aes(date, NEE_count))+
  geom_line()

ggplot(moz.daily, aes(date, GPP_DT_daily))+
  geom_line()

ggplot(moz.daily, aes(date, GPP_NT_daily))+
  geom_line()

ggplot(moz.daily, aes(date, GPP_NT_count))+
  geom_line()

ggplot(moz.daily, aes(date, RECO_DT_daily))+
  geom_line()

ggplot(moz.daily, aes(date, RECO_NT_daily))+
  geom_line()

ggplot(moz.daily, aes(date, ET_daily))+
  geom_line()

ggplot(moz.daily, aes(date, ET_count))+
  geom_line()

ggplot(moz.daily, aes(date, SWC_mean_1))+
  geom_line()

ggplot(moz.daily, aes(date, SWC_1_count))+
  geom_line()

ggplot(moz.daily, aes(date, TA_mean))+
  geom_line()

ggplot(moz.daily, aes(date, P_sum))+
  geom_line()

# save daily data for tonzi and moflux
#  write.table(ton.daily, "AMF_US_TON_Fluxnet_daily_2001_2021.csv", row.names=FALSE, sep=",", dec=".")
# write.table(moz.daily, "AMF_US_MOz_Fluxnet_daily_2004_2019.csv", row.names=FALSE, sep=",", dec=".")


