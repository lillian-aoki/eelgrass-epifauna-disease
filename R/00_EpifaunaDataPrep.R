# Data prep for grazing scar SEM - epifauna data
# Last updated: 2023-03-03
# Updated by: Lillian Aoki

# This script calculates transect-level data from Carmen's epifauna data
# The most recent epifauna update is from January 2023, and includes Bodega 2021 data
# this should be the complete dataset, with no further epifauna modifications

library(tidyverse)
# use updated data file with missing OR-A-2021 macrophytes added
# note, as of 2023-03-07, I updated the macrophyte data within the SI DropBox folder, so no longer need to handle that manually
# also as of 2023-03-07, I now have calculated "large" and "all" abundance, etc. values for use in the modeling
# this is to figure out how much cutting out the small guys affects the models
epi_new <- read_csv("data/Carmen_DropBox/EGWD_epifauna_data_v20230307_LRA_big_epi_with_BB.csv") # should probably name this something else
# epi_may <- read_csv("data/EGWD_transect_data_20220523.csv") # this is the dataset from the May 2022 workshop - the "original"
# used for initial modeling, but no longer up to date
# pull out only summary taxa, macrophyte, and ID variables
# epi_new <- select(epi_new, c(quadrat_unique_code, macrophyte_wet_mass_g, site_unique_code, year,
#                                region, transect_unique_code, epifauna_abundance_total, 
#                                ampithoid_abundance, lacuna_abundance, idoteid_abundance,
#                                richness_site, richness_sample)) # old version
epi_new <- select(epi_new, c(quadrat_unique_code, macrophyte_wet_mass_g, site_unique_code, year,
                               region, transect_unique_code, epifauna_abundance_total, epifauna_abundance_large,
                               ampithoid_abundance_all, ampithoid_abundance_large, 
                               lacuna_abundance_all, lacuna_abundance_large,
                               idoteid_abundance_all, idoteid_abundance_large,
                               richness_site_all, richness_site_large, 
                              richness_sample_all, richness_sample_large))

# add zeros for NA in abundance columns
epi_new <- epi_new %>% replace(is.na(.), 0)
# calculate the abundance per macrophyte mass
epi_new$epifauna_per_g_all <- epi_new$epifauna_abundance_total/epi_new$macrophyte_wet_mass_g
epi_new$epifauna_per_g_large <- epi_new$epifauna_abundance_large/epi_new$macrophyte_wet_mass_g
epi_new$ampithoid_per_g_all <- epi_new$ampithoid_abundance_all/epi_new$macrophyte_wet_mass_g
epi_new$ampithoid_per_g_large <- epi_new$ampithoid_abundance_large/epi_new$macrophyte_wet_mass_g
epi_new$lacuna_per_g_all <- epi_new$lacuna_abundance_all/epi_new$macrophyte_wet_mass_g
epi_new$lacuna_per_g_large <- epi_new$lacuna_abundance_large/epi_new$macrophyte_wet_mass_g
epi_new$idoteid_per_g_all <- epi_new$idoteid_abundance_all/epi_new$macrophyte_wet_mass_g
epi_new$idoteid_per_g_large <- epi_new$idoteid_abundance_large/epi_new$macrophyte_wet_mass_g

# at this point, drop the values from SD-C-2020 - site that had collapsed, no epifauna taken, no macrophytes
epi_new <- epi_new[is.finite(epi_new$epifauna_per_g_all),]
# calculate log values
# added 0.01 based on Emmett's prior data exploration analyses - same approach
# old version
# epi_new$epifauna_log_per_g <- log10(epi_new$epifauna_per_g+0.01)
# epi_new$ampithoid_log_per_g <- log10(epi_new$ampithoid_per_g+0.01)
# epi_new$lacuna_log_per_g <- log10(epi_new$lacuna_per_g+0.01)
# epi_new$idoteid_log_per_g <- log10(epi_new$idoteid_per_g+0.01)
# new version
epi_new$epifauna_log_per_g_all <- log10(epi_new$epifauna_per_g_all+0.01)
epi_new$epifauna_log_per_g_large <- log10(epi_new$epifauna_per_g_large+0.01)

epi_new$ampithoid_log_per_g_all <- log10(epi_new$ampithoid_per_g_all+0.01)
epi_new$ampithoid_log_per_g_large <- log10(epi_new$ampithoid_per_g_large+0.01)

epi_new$lacuna_log_per_g_all <- log10(epi_new$lacuna_per_g_all+0.01)
epi_new$lacuna_log_per_g_large <- log10(epi_new$lacuna_per_g_large+0.01)

epi_new$idoteid_log_per_g_all <- log10(epi_new$idoteid_per_g_all+0.01)
epi_new$idoteid_log_per_g_large <- log10(epi_new$idoteid_per_g_large+0.01)

# average to transect level
# old version
# epi_new_transect <- epi_new %>%
#   group_by(year, site_unique_code, region, transect_unique_code) %>%
#   summarise(epifauna_per_g_transect=mean(epifauna_per_g, na.rm=T),
#             ampithoid_per_g_transect=mean(ampithoid_per_g, na.rm=T),
#             lacuna_per_g_transect=mean(lacuna_per_g, na.rm=T),
#             idoteid_per_g_transect=mean(idoteid_per_g, na.rm=T),
#             epifauna_log_per_g_transect=mean(epifauna_log_per_g, na.rm=T),
#             ampithoid_log_per_g_transect=mean(ampithoid_log_per_g, na.rm=T),
#             lacuna_log_per_g_transect=mean(lacuna_log_per_g, na.rm=T),
#             idoteid_log_per_g_transect=mean(idoteid_log_per_g, na.rm=T),
#             richness_site=richness_site)
# new version
epi_new_transect <- epi_new %>%
  group_by(year, region, site_unique_code, transect_unique_code) %>%
  summarise(epifauna_per_g_transect_all=mean(epifauna_per_g_all, na.rm=T),
            epifauna_per_g_transect_large=mean(epifauna_per_g_large, na.rm=T),
            ampithoid_per_g_transect_all=mean(ampithoid_per_g_all, na.rm=T),
            ampithoid_per_g_transect_large=mean(ampithoid_per_g_large, na.rm=T),
            lacuna_per_g_transect_all=mean(lacuna_per_g_all, na.rm=T),
            lacuna_per_g_transect_large=mean(lacuna_per_g_large, na.rm=T),
            idoteid_per_g_transect_all=mean(idoteid_per_g_all, na.rm=T),
            idoteid_per_g_transect_large=mean(idoteid_per_g_large, na.rm=T),
            epifauna_log_per_g_transect_all=mean(epifauna_log_per_g_all, na.rm=T),
            epifauna_log_per_g_transect_large=mean(epifauna_log_per_g_large, na.rm=T),
            ampithoid_log_per_g_transect_all=mean(ampithoid_log_per_g_all, na.rm=T),
            ampithoid_log_per_g_transect_large=mean(ampithoid_log_per_g_large, na.rm=T),
            lacuna_log_per_g_transect_all=mean(lacuna_log_per_g_all, na.rm=T),
            lacuna_log_per_g_transect_large=mean(lacuna_log_per_g_large, na.rm=T),
            idoteid_log_per_g_transect_all=mean(idoteid_log_per_g_all, na.rm=T),
            idoteid_log_per_g_transect_large=mean(idoteid_log_per_g_large, na.rm=T),
            richness_site_all= mean(richness_site_all),
            richness_site_large= mean(richness_site_large))
ggplot(epi_new_transect, aes(x=epifauna_log_per_g_transect_all, fill=region))+geom_histogram()+facet_wrap(~region)
ggplot(epi_new_transect, aes(x=epifauna_log_per_g_transect_large, fill=region))+geom_histogram()+facet_wrap(~region)
ggplot(epi_new_transect, aes(x=epifauna_log_per_g_transect_all, y=epifauna_log_per_g_transect_large, color=region))+geom_point()
ggplot(epi_new_transect, aes(x=ampithoid_log_per_g_transect_all, y=ampithoid_log_per_g_transect_large, color=region))+geom_point()
ggplot(epi_new_transect, aes(x=epifauna_per_g_transect_all, y=epifauna_per_g_transect_large, color=region))+geom_point()+facet_wrap(~region, scales="free")
ggplot(epi_may[-which(is.na(epi_may$epifauna_per_g_log_transect)),], aes(x=epifauna_per_g_log_transect, fill=Region))+geom_histogram()+facet_wrap(~Region)
# may and june epifauna data start to look different because we've filled in the missing OR-A-2021 data
# therefore, export this dataset for further use.
## note, BB-2021 data remain missing from the June dataset. Doesn't really matter because these sites don't have temps
## But might be important for future stuff...
## update as of 2022-12-20 - will ask Jay about the BB-2021 epifauna to see if there's any progress
## update as of 2023-03-03, BB-2021 data have been added. They are problematic in that they are not sieved (includes 0.5-1.0 mm)
## update as of 2023-03-06, recalculated taxon abundances in Carmen's script (NSF DropBox) - no longer have 0s for Bodega
write_csv(epi_new_transect, "data/EGWD_transect_data_v20230307_big_epi_with_BB_transect.csv")
# use this file in script 01_DataPrepForSEM to combine with temps, etc. 


epi_new_site <- epi_new %>%
  group_by(year, region, site_unique_code) %>%
  summarise(epifauna_per_g_site_all=mean(epifauna_per_g_all, na.rm=T),
            epifauna_per_g_site_large=mean(epifauna_per_g_large, na.rm=T),
            ampithoid_per_g_site_all=mean(ampithoid_per_g_all, na.rm=T),
            ampithoid_per_g_site_large=mean(ampithoid_per_g_large, na.rm=T),
            lacuna_per_g_site_all=mean(lacuna_per_g_all, na.rm=T),
            lacuna_per_g_site_large=mean(lacuna_per_g_large, na.rm=T),
            idoteid_per_g_site_all=mean(idoteid_per_g_all, na.rm=T),
            idoteid_per_g_site_large=mean(idoteid_per_g_large, na.rm=T),
            epifauna_log_per_g_site_all=mean(epifauna_log_per_g_all, na.rm=T),
            epifauna_log_per_g_site_large=mean(epifauna_log_per_g_large, na.rm=T),
            ampithoid_log_per_g_site_all=mean(ampithoid_log_per_g_all, na.rm=T),
            ampithoid_log_per_g_site_large=mean(ampithoid_log_per_g_large, na.rm=T),
            lacuna_log_per_g_site_all=mean(lacuna_log_per_g_all, na.rm=T),
            lacuna_log_per_g_site_large=mean(lacuna_log_per_g_large, na.rm=T),
            idoteid_log_per_g_site_all=mean(idoteid_log_per_g_all, na.rm=T),
            idoteid_log_per_g_site_large=mean(idoteid_log_per_g_large, na.rm=T),
            richness_site_all= mean(richness_site_all),
            richness_site_large= mean(richness_site_large))
write_csv(epi_new_site, "data/EGWD_site_data_20230308.csv")
