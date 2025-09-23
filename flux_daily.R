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



# Read the CSV file into a dataframe called "flux"
flux <- read.csv("xNQ_biomet_daily.csv", stringsAsFactors = FALSE)

# Read the eddyproc data
eddy <- read.csv("xNQ_eddyproc.csv", stringsAsFactors = FALSE)

# Group by Year and DoY to calculate daily ET sum
et_daily <- eddy %>%
  group_by(Year, DoY) %>%
  summarise(ET = sum(ET_mm, na.rm = TRUE), .groups = "drop")

# Join et_daily to flux by Year and DoY
flux <- left_join(flux, et_daily, by = c("Year", "DoY"))


# Extract the desired columns into a new dataframe called "flux_daily"
flux_daily <- flux[, c("Year", "DoY", "ET", "NEE", "GPP_U50_f", "GPP_DT_U05", "Reco_U50", "Reco_DT_U05")]

flux$NEE <- flux$NEE * (1800 * 1 * 10^-6 * 12.01)
flux$GPP_U50_f <- flux$GPP_U50_f * (1800 * 1 * 10^-6 * 12.01)
flux$GPP_DT_U05 <- flux$GPP_DT_U05 * (1800 * 1 * 10^-6 * 12.01)
flux$Reco_U50 <- flux$Reco_U50 * (1800 * 1 * 10^-6 * 12.01)
flux$Reco_DT_U05 <- flux$Reco_DT_U05 * (1800 * 1 * 10^-6 * 12.01)





# Write the new dataframe to a CSV file
write.csv(flux_daily, "ONAQ_flux_daily.csv", row.names = FALSE)
