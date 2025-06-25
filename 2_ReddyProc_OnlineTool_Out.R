# Work with data from ReddyProc Online Tool Output

# load libraries
library(data.table)
library(ggplot2)
library(cowplot)
library(viridis)

setwd("C:/Users/mebeckage/OneDrive - The University of Texas at El Paso/Mauritz Lab - ONAQ_Data")

# import
rp.units <- (fread("./REddyResults_ONAQ_2024_20250625_942226779/output.txt",
                   header=TRUE))[1,]

flux.rp <- fread("./REddyResults_ONAQ_2024_20250625_942226779/output.txt",
                 header=FALSE, skip=2,na.strings=c("-9999", "NA","-"),
                 col.names = colnames(rp.units))

# for some reason na.strings won't recognize the -9999
flux.rp[flux.rp == -9999] <- NA


# plot with no U* filter or gapfill
ggplot(flux.rp, aes(DoY,NEE_orig))+
  geom_line()+
  facet_grid(Year~.)

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
        axis.text.x = element_blank())
fig_nee_fill

# graph Ustar filtered Reco with 50th percentile and gap-filled
fig_reco <- ggplot(subset(flux.rp), aes(DoY,Reco_U50))+geom_line()+
  geom_line(aes(y=Reco_DT_U50),colour="blue")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
fig_reco

# plot daytime Reco with qc code (not sure what to do with this code: exlude 1?)
ggplot(subset(flux.rp), aes(DoY,Reco_DT_U50, colour=factor(FP_qc)))+
  geom_point(size=1)

# graph Ustar filtered GPP Day time and night time with 50th percentile and gap-filled
fig_gpp <- ggplot(flux.rp, aes(DoY,GPP_U50_f))+
  geom_line()+
  geom_line(aes(y=GPP_DT_U50),colour="blue")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
fig_gpp


fig_le <- ggplot((flux.rp))+
  geom_line(aes(DoY,LE_f))
fig_le

plot_grid(fig_nee_fill, fig_reco, fig_gpp, fig_le, nrow=4, align="v")



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
