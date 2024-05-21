# Read data from MOz and TON sites from Ameriflux downloads
# Exploratory data for Proposal

# load libraries
library(data.table)
library(ggplot2)
library(cowplot)
library(lubridate)
library(bigleaf)

# load data
# Tonzi Ranch
ton <- fread("./AMF_US-Ton_FLUXNET_SUBSET_2001-2021_3-5/AMF_US-Ton_FLUXNET_SUBSET_HH_2001-2021_3-5.csv",
                 header=TRUE,,na.strings=c("-9999", "NA","-"))

# convert timestamp end (summary is for 30mins prior) to datetime
ton[,':='(datetime=ymd_hm(TIMESTAMP_END))]

# view columns
colnames(ton)

# Moflux
moz <- fread("./AMF_US-MOz_FLUXNET_SUBSET_2004-2019_3-5/AMF_US-MOz_FLUXNET_SUBSET_HH_2004-2019_3-5.csv",
             header=TRUE,,na.strings=c("-9999", "NA","-"))

# convert timestamp end (summary is for 30mins prior) to datetime
moz[,':='(datetime=ymd_hm(TIMESTAMP_END))]

# view columns
colnames(moz)

# TONZI
# graph NEE, Reco, GPP for night-time and daytime
# use _50 quantiles
# NEE
p.ton.nee <- ggplot(ton[NEE_VUT_75_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=NEE_VUT_75), linewidth=0.2)+
  labs(y="NEE umol/m2/sec")+
  theme_bw()

# GPP 
p.ton.gpp <- ggplot(ton[NEE_VUT_75_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=GPP_NT_VUT_75), linewidth=0.2, colour="darkgreen")+
  geom_line(aes(y=GPP_DT_VUT_75), linewidth=0.2, alpha=0.75, colour="lightgreen")+
  labs(y="GPP umol/m2/sec")+
  theme_bw()

# Reco
p.ton.reco <- ggplot(ton[NEE_VUT_75_QC<2 & as.Date(datetime)>=as.Date("2001-07-01"),], aes(x=datetime))+
  geom_line(aes(y=RECO_NT_VUT_75), linewidth=0.2, colour="brown")+
  geom_line(aes(y=RECO_DT_VUT_75), linewidth=0.2, alpha=0.75, colour="grey")+
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

# convert LE to ET
ton[,':='(ET = LE.to.ET(LE_F_MDS,TA_F))]

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
p.moz.nee <- ggplot(moz[NEE_VUT_75_QC<2 & as.Date(datetime)>=as.Date("2004-07-01"),], aes(x=datetime))+
  geom_line(aes(y=NEE_VUT_75), linewidth=0.2)+
  labs(y="NEE umol/m2/sec")+
  theme_bw()

# GPP 
p.moz.gpp <- ggplot(moz[NEE_VUT_75_QC<2 & as.Date(datetime)>=as.Date("2004-07-01"),], aes(x=datetime))+
  geom_line(aes(y=GPP_NT_VUT_75), linewidth=0.2, colour="darkgreen")+
  geom_line(aes(y=GPP_DT_VUT_75), linewidth=0.2, alpha=0.75, colour="lightgreen")+
  labs(y="GPP umol/m2/sec")+
  theme_bw()

# Reco
p.moz.reco <- ggplot(moz[NEE_VUT_75_QC<2 & as.Date(datetime)>=as.Date("2004-07-01"),], aes(x=datetime))+
  geom_line(aes(y=RECO_NT_VUT_75), linewidth=0.2, colour="brown")+
  geom_line(aes(y=RECO_DT_VUT_75), linewidth=0.2, alpha=0.75, colour="grey")+
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

# convert LE to ET
moz[,':='(ET = LE.to.ET(LE_F_MDS,TA_F))]

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
ggplot(moz[SWC_F_MDS_1_QC<2,], aes(x=datetime))+
  geom_line(aes(y=SWC_F_MDS_1),colour="lightblue")+
  labs(title = "MoFlux", y="Soil Water Content %")+
theme_bw()

