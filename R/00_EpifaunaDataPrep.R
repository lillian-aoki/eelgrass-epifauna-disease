# Data prep for grazing scar SEM - epifauna data
# Last updated: 2022-11-23
# Updated by: Lillian Aoki

# This script calculates transect-level data from Carmen's June epifauna data
# If newer epifauna data are available, we can substitute

library(tidyverse)
# use updated data file with missing OR-A-2021 macrophytes added
epi_june <- read_csv("data/EGWD_epifauna_data_v20220622_CJR_big_epi_update_macrophyte.csv")
epi_may <- read_csv("data/EGWD_transect_data_20220523.csv")
# pull out only summary taxa, macrophyte, and ID variables
epi_june <- select(epi_june, c(quadrat_unique_code, macrophyte_wet_mass_g, site_unique_code, year,
                               region, transect_unique_code, epifauna_abundance_total, 
                               ampithoid_abundance, lacuna_abundance, idoteid_abundance))
# add zeros for NA in abundance columns
epi_june <- epi_june %>% replace(is.na(.), 0)
# calculate the abundance per macrophyte mass
epi_june$epifauna_per_g <- epi_june$epifauna_abundance_total/epi_june$macrophyte_wet_mass_g
epi_june$ampithoid_per_g <- epi_june$ampithoid_abundance/epi_june$macrophyte_wet_mass_g
epi_june$lacuna_per_g <- epi_june$lacuna_abundance/epi_june$macrophyte_wet_mass_g
epi_june$idoteid_per_g <- epi_june$idoteid_abundance/epi_june$macrophyte_wet_mass_g
# at this point, drop the values from SD-C-2020 - site that had collapsed, no epifauna taken, no macrophytes
epi_june <- epi_june[is.finite(epi_june$epifauna_per_g),]
# calculate log values
# added 0.01 based on Emmett's prior data exploration analyses - same approach
epi_june$epifauna_log_per_g <- log10(epi_june$epifauna_per_g+0.01)
epi_june$ampithoid_log_per_g <- log10(epi_june$ampithoid_per_g+0.01)
epi_june$lacuna_log_per_g <- log10(epi_june$lacuna_per_g+0.01)
epi_june$idoteid_log_per_g <- log10(epi_june$idoteid_per_g+0.01)
# average to transect level
epi_june_transect <- epi_june %>%
  group_by(year, site_unique_code, region, transect_unique_code) %>%
  summarise(epifauna_per_g_transect=mean(epifauna_per_g, na.rm=T),
            ampithoid_per_g_transect=mean(ampithoid_per_g, na.rm=T),
            lacuna_per_g_transect=mean(lacuna_per_g, na.rm=T),
            idoteid_per_g_transect=mean(idoteid_per_g, na.rm=T),
            epifauna_log_per_g_transect=mean(epifauna_log_per_g, na.rm=T),
            ampithoid_log_per_g_transect=mean(ampithoid_log_per_g, na.rm=T),
            lacuna_log_per_g_transect=mean(lacuna_log_per_g, na.rm=T),
            idoteid_log_per_g_transect=mean(idoteid_log_per_g, na.rm=T))
ggplot(epi_june_transect, aes(x=epifauna_log_per_g_transect, fill=region))+geom_histogram()
ggplot(epi_may, aes(x=epifauna_per_g_log_transect, fill=Region))+geom_histogram()
# may and june epifauna data start to look different because we've filled in the missing OR-A-2021 data
# therefore, export this dataset for further use.
## note, BB-2021 data remain missing from the June dataset. Doesn't really matter because these sites don't have temps
## But might be important for future stuff...
## update as of 2022-12-20 - will ask Jay about the BB-2021 epifauna to see if there's any progress
write_csv(epi_june_transect, "data/EGWD_transect_data_v20220622_CJR_big_epi_update_macrophyte_with_calcs.csv")
# use this file in script 01_DataPrepForSEM to combine with temps, etc. 