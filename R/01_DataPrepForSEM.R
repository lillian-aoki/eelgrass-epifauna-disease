# Data prep for grazing scar SEM
# Last updated: 2022-12-20
# Updated by: Lillian Aoki

# This script compiles transect-level survey data, epifauna data, temp anomaly data, site metadata for modeling

library(tidyverse)

# Input Data files:
# - combined_transect_survey_metrics.csv - this datafile aggregates eelgrass field data from surveys (e.g. morphology, density). 
#   This file is produced in the "eelgrass-surveys-2019" project 
# - monthly_temp_anomaly_mur_9y_sites.csv - this datafile contains monthly temperate anmoalies (warm, cold, total) for all 21 sites with MUR data.
#   This file is set up in a wide format, so each row is a site-year combination, and each month's anomalies are a column. This allows the monthly 
#   anomalies to be predictors in models. This file is produced in the "mur-sst-2019" project. 
# - monthly_temp_anomaly_g1sst_9y_sites.csv - analagous file to above, for sites with G1SST data from 2019. Only 6 sites.
# - EGWD_transect_data_v20220622_CJR_big_epi_update_macrophyte_with_calcs.csv - This datafile is produced in 
#   script 00_EpifaunaDataPrep.R, using the most recent update from Carmen for the underlying epifauna data.
#   The script makes additional changes by re-calculating standardized abundances with 0s instead of NAs.
# - grazing_scars_transect_compiles.csv - this datafile contains all the grazing scar count data, from surveys, from Coco's reanalysis of WA and SD scars
#   Note that for 2019, the surveys recorded grazing only on epiphyte blades, but Coco looked at all 20 blade for WA and SD. Therefore, as part of 
#   data prep in this script, scars are limited to only 5 per transect for WA and SD (blades 16-20, epiphyte blades). For 2021 and onward, 
#   all blades were assessed at all sites for grazing. 
# - combined_site_metadata.csv - this datafile has sampling metadata, e.g. transect locations and sampling dates. Latitude is a possible predictor in the
#   models, therefore need the metadata as well. 

# Data processing
# The script pulls in the data and binds it all together. Everything is at the transect level (except temp is at the site level). 
# Processing includes adjusting grazing scars (see notes above)
# Output data excludes sites with no temperature data, with separate data files per year (?)

# read in data
sg <- read_csv("data/combined_transect_survey_metrics.csv")
mur <- read_csv("data/monthly_temp_anomaly_mur_9y_sites.csv")
ghr <- read_csv("data/monthly_temp_anomaly_g1sst_9y_sites.csv")
epi <- read_csv("data/EGWD_transect_data_v20220622_CJR_big_epi_update_macrophyte_with_calcs.csv")
gz <- read_csv("data/grazing_scars_compiled.csv")
meta <- read_csv("data/combined_site_metadata.csv")

# combine mur and ghr
mur <- select(mur, -c("TempAnomTot_October", "TempAnomTot_November", "TempAnomTot_December",
                      "TempAnomWarm_October", "TempAnomWarm_November", "TempAnomWarm_December",
                      "TempAnomCold_October", "TempAnomCold_November", "TempAnomCold_December"))
temps <- rbind(mur, ghr)
# combine with a right-join to limit to sites with a temperature record
sg_temps <- right_join(sg, temps, by = c("Year", "Region", "SiteCode"="Site"))

# with udpated epifauna data, no longer have duplicate entries, disregard this section
# # epi dataset has transect data that Emmett added - this might be old? avoid any replicates by removing
# # also note that epifauna has some calculated log values - at least some are log10(x+1) - check with Emmett before using
# epi <- select(epi, -c("LongestBladeLengthMean", "LongestBladeLengthSe", "LongestBladeWidthMean",
#                       "SheathLengthMean", "SheathLengthSe", "EpiphyteBladeAreaMean", "EpiphyteDryMassMean", 
#                       "EpiphytePerAreaMean", "DensityShootsMean", "DensityShootsSd",
#                       "DensityShootsSe", "PercentSeagrassMean", "PercentBareMean", "PercentOtherMean", 
#                       "PrevalenceMean", "SeverityMean", "LesionAreaMean", "CountBlades", "Percent",
#                       "CountBrokenTip", "BladeAreaMean", "scars_mean_transect", "scars_mean_transect_log"))

# add epifauna - again limit to prevent any repeating rows. note, the epifauna are limited to temperature sites only
sg_temps$transect_unique_code <- paste(sg_temps$Region, ".", sg_temps$SiteCode, ".", 
                                       sg_temps$TidalHeight, sg_temps$Transect, ".", sg_temps$Year, sep="")
sg_temps_epi <- inner_join(sg_temps, epi)
# adjust grazing scars - only 5 blades in 2019
# gz <- subset(gz, Blade>15 & Blade<21)
# this limits SD to one blade - but that's off. Instead, pick 5 blades from SD and blades >15 for WA

gz <- gz[-c(which(gz$Region=="SD" & gz$Year=="2019" & gz$Blade %in% c(2,3,5,6,7,9,10,11,13,14,15))),]
gz <- gz[-c(which(gz$Region=="WA" & gz$Year=="2019" & (gz$Blade<16 | gz$Blade>21))),]
# gz <- na.omit(gz)
gz <- subset(gz, !is.na(gz$GrazingScars))
# this keeps all the grazing for 2021 
gz_summ <- gz %>%
  group_by(Region, SiteCode, Year, Transect, TidalHeight) %>%
  summarise(GrazingScarsMeanTransect=mean(GrazingScars, na.rm=TRUE), CountScars=length(!is.na(GrazingScars)))
sg_temps_epi_gz <- left_join(sg_temps_epi, gz_summ)

# extract transect locations from the meta data
meta19 <- subset(meta, Year=="2019")
loc19 <- select(meta19, c("Region","SiteCode", "TidalHeight", "Transect", "TransectBeginDecimalLatitude",
                          "TransectBeginDecimalLongitude", "TransectEndDecimalLatitude", "TransectEndDecimalLongitude"))
# fill in missing transect location data (based on 2019 locations unless updated)
full_meta <- meta %>%
  rows_update(loc19, by = c("Region", "SiteCode", "TidalHeight", "Transect"))
# add meta data
sg_temps_epi_gz_meta <- left_join(sg_temps_epi_gz, full_meta)

# final step is to add a few calculated seagrass metrics that might be useful for the SEM - Canopy height and Structure
sg_temps_epi_gz_meta$CanopyHeight <- sg_temps_epi_gz_meta$LongestBladeLengthMean + sg_temps_epi_gz_meta$SheathLengthMean
# note that Canopy Height and Sheath Length are both in mm, so this value is pretty large (divided by 100 to convert mm to m)
# calculate Structure as Canopy Height times Density
sg_temps_epi_gz_meta$Structure <- sg_temps_epi_gz_meta$CanopyHeight/100 * sg_temps_epi_gz_meta$DensityShootsMean
# take log base 10 to improve inference of results
sg_temps_epi_gz_meta$StructureLog <- log10(sg_temps_epi_gz_meta$Structure)
# convert EpiphytePerAreaMean units from g per cm2 to mg per cm2
sg_temps_epi_gz_meta$EpiphytePerAreaMeanMg <- sg_temps_epi_gz_meta$EpiphytePerAreaMean*1000
# calculate log of shoot density
sg_temps_epi_gz_meta$DensityLog <- log10(sg_temps_epi_gz_meta$DensityShootsMean)
# compiled dataset is now ready for modeling etc. 
write_csv(sg_temps_epi_gz_meta, "data/full_seagrass_epifauna_for_SEM.csv")


# Blade level
# Same idea, but now pulling blade-level processing - to combine with some transect level markers
blade <- read_csv("data/meter_level_shoot_metrics_with_disease.csv")
blade$GrazingScars <- ifelse(blade$GrazingScars==0, 0, 1)
write_csv(blade, "data/blade_data_for_SEM.csv")
# the above is for the 5 epiphyte blades only. We've decided to include additional blades from 2021 (but not 2020?)
dis <- read_csv("data/meter_level_disease_metrics.csv")
all_dis <- left_join(dis, gz)
write_csv(all_dis, "data/all_dis_gz_blade_for_SEM.csv")
