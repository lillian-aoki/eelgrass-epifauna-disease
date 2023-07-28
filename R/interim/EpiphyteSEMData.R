## Data prep for SEMS using epiphyte
## This script brings in transect-level data (from 01_DataPrepForSEM.R). Consider combining these scripts?
## Other input is the blade/meter level shoot metrics - which is where the grazing scar data are placed. However, these need to be updated. 

## two kinds of SEMs - epiphyte as a meadow scale predictor and as a blade scale predictor
## no blade-level epiphytes for San Diego in 2019 (key year)

library(tidyverse)
library(lubridate)
library(GGally)
# collate variables

tran <- read_csv("data/full_seagrass_epifauna_for_SEM.csv")
# old version
# tran_vars <- select(tran, "PercentOtherMean", "PercentBareMean", "Structure", "StructureLog", "epifauna_log_per_g_transect",
#                     "Meadow", "Year", "TidalHeight", "Region", "SiteCode", "Transect", "TransectBeginDecimalLatitude",
#                     "TempAnomWarm_March", "TempAnomWarm_April", "TempAnomWarm_May", "TempAnomWarm_June",
#                     "PrevalenceMean", "SeverityMean", "LesionAreaMean", "Latitude"="TransectBeginDecimalLatitude",
#                     "CanopyHeight", "DensityShootsMean", "SheathLengthMean", "DensityLog", "EpiphytePerAreaMeanMg",
#                     "Ampithoid" = "ampithoid_log_per_g_transect", "Lacuna" = "lacuna_log_per_g_transect",
#                     "Idoteid"="idoteid_log_per_g_transect", "Richness"="richness_site")
# new version with large and all epifauna separated
tran_vars <- select(tran, "PercentOtherMean", "PercentBareMean", "Structure", "StructureLog", "epifauna_log_per_g_transect_all",
                    "epifauna_log_per_g_transect_large",
                    "Meadow", "Year", "TidalHeight", "Region", "SiteCode", "Transect", "TransectBeginDecimalLatitude",
                    "TempAnomWarm_March", "TempAnomWarm_April", "TempAnomWarm_May", "TempAnomWarm_June",
                    "PrevalenceMean", "SeverityMean", "LesionAreaMean", "Latitude"="TransectBeginDecimalLatitude",
                    "CanopyHeight", "DensityShootsMean", "SheathLengthMean", "DensityLog", "EpiphytePerAreaMeanMg",
                    "Ampithoid" = "ampithoid_log_per_g_transect_all", "Lacuna" = "lacuna_log_per_g_transect_all",
                    "Idoteid"="idoteid_log_per_g_transect_all", "Richness"="richness_site_all",
                    "Ampithoid_large" = "ampithoid_log_per_g_transect_large", "Lacuna_large" = "lacuna_log_per_g_transect_large",
                    "Idoteid_large"="idoteid_log_per_g_transect_large", "Richness_large"="richness_site_large")

# tran_vars <- subset(tran_vars, DensityShootsMean>0) # don't do this
# site <- tran_vars %>%
#   group_by(Year, Region, SiteCode, Meadow) %>%
#   summarise(PercentOther=mean(PercentOtherMean), PercentBare=mean(PercentBareMean), 
#             Structure=mean(Structure), StructureLog=log10(Structure), Latitude=mean(Latitude),
#             Density=mean(DensityShootsMean), SheathLength=mean(SheathLengthMean), CanopyHeight=mean(CanopyHeight),
#             Epifauna=mean(epifauna_log_per_g_transect), DensityLog=log10(Density),
#             EpiphytePerAreaMeadowMg=mean(EpiphytePerAreaMeanMg),
#             TempAnomWarm_March=mean(TempAnomWarm_March), TempAnomWarm_April=mean(TempAnomWarm_April), 
#             TempAnomWarm_May=mean(TempAnomWarm_May), TempAnomWarm_June=mean(TempAnomWarm_June),
#             Prevalence=mean(PrevalenceMean), LesionArea=mean(LesionAreaMean),
#             Ampithoid=mean(Ampithoid, na.rm=TRUE), Lacuna=mean(Lacuna, na.rm=TRUE),
#             Idoteid=mean(Idoteid, na.rm=TRUE), Richness=max(Richness))
site <- tran_vars %>%
  group_by(Year, Region, SiteCode, Meadow) %>%
  summarise(PercentOther=mean(PercentOtherMean), PercentBare=mean(PercentBareMean), 
            Structure=mean(Structure), StructureLog=log10(Structure), Latitude=mean(Latitude),
            Density=mean(DensityShootsMean), SheathLength=mean(SheathLengthMean), CanopyHeight=mean(CanopyHeight),
            Epifauna_all=mean(epifauna_log_per_g_transect_all), 
            Epifauna_large = mean(epifauna_log_per_g_transect_large),
            DensityLog=log10(Density), Latitude=mean(Latitude),
            EpiphytePerAreaMeadowMg=mean(EpiphytePerAreaMeanMg),
            TempAnomWarm_March=mean(TempAnomWarm_March), TempAnomWarm_April=mean(TempAnomWarm_April), 
            TempAnomWarm_May=mean(TempAnomWarm_May), TempAnomWarm_June=mean(TempAnomWarm_June),
            Prevalence=mean(PrevalenceMean), LesionArea=mean(LesionAreaMean),
            Ampithoid_all=mean(Ampithoid, na.rm=TRUE), Lacuna_all=mean(Lacuna, na.rm=TRUE),
            Idoteid_all=mean(Idoteid, na.rm=TRUE), Richness_all=max(Richness),
            Ampithoid_large=mean(Ampithoid_large, na.rm=TRUE), Lacuna_large=mean(Lacuna_large, na.rm=TRUE),
            Idoteid_large=mean(Idoteid_large, na.rm=TRUE), Richness_large=max(Richness_large))
temps <- read_csv("data/monthly_mean_temps_mur_g1sst_9y_sites.csv")
temps$MonthW <- format(temps$Month, "%B")
june <- subset(temps, MonthW=="June")
june$Year <- as.integer(format(june$Month, "%Y"))
june <- select(june, c(Year, Region, SiteCode=Site, Meadow, MonthlyMeanTemp_June=MonthlyMeanTemp))
site <- left_join(site, june)

# collate variables again

# site_sem <- select(site, c(CanopyHeight, Epifauna, TempAnomWarm_April, TempAnomWarm_May, EpiphytePerAreaMeadowMg,
#                            TempAnomWarm_June, MonthlyMeanTemp_June, DensityLog, Ampithoid, Lacuna, Idoteid,
#                            Richness))
site_sem <- select(site, c(CanopyHeight, Epifauna_all, Epifauna_large, TempAnomWarm_April, TempAnomWarm_May, EpiphytePerAreaMeadowMg,
                           TempAnomWarm_June, MonthlyMeanTemp_June, DensityLog, Ampithoid_all, Ampithoid_large, Lacuna_all, Lacuna_large,
                          Idoteid_all, Idoteid_large,Latitude,
                           Richness_all, Richness_large))
# divide canopy height by 1000 to convert from mm to m
site_sem$CanopyHeight <- site_sem$CanopyHeight/1000

## blade-level epiphytes ####
# read in blade level data
dis <- read_csv("data/meter_level_shoot_metrics_with_disease.csv")
dis$EpiphytePerAreagcm2 <- dis$EpiphyteDryMass/dis$BladeArea # blade area is already in cm2 for this
dis$EpiphytePerAreamgcm2 <- dis$EpiphyteDryMass/dis$BladeArea*1000 # convert to mg to get better numbers
dis <- dis[-which(dis$EpiphyteDryMass<0),] # removes erroneous mass values
dis_site <- left_join(dis, site_sem)
# add binary variables for year and tidal height
dis_site$TidalHeightBinary <- ifelse(dis_site$TidalHeight=="L", 0, 1)
# restirct to 2019 and 2021
dis_site <- subset(dis_site, Year!=2020)
dis_site$YearBinary <- ifelse(dis_site$Year==2019, 0, 1)
# this is the data set for models that will build SEM
# note, lots of missing epifauna data (e.g. no BB for 2021?)
# also missing temp data (lack of SST) this is the main issue - only about 48 sites with MUR data in 2019 + 2021
# exclude
dis_site <- dis_site[-which(is.na(dis_site$Epifauna_all)),] # reduces dataset 
dis_site$Meadow <- paste(dis_site$Region, dis_site$SiteCode, sep="_")
write_csv(dis_site, "data/epiphyte_SEM_data_all_large.csv")

## meadow-scale epiphytes ####
## other dataset uses all blades and does not use the epiphyte as a blade-level predictor 
# read in blade-level disease data (all blades)
dis2 <- read_csv("data/meter_level_disease_metrics.csv")
dis_site2 <- left_join(dis2, site_sem)
# add binary variables for year and tidal height
dis_site2$TidalHeightBinary <- ifelse(dis_site2$TidalHeight=="L", 0, 1)
# restirct to 2019 and 2021
dis_site2 <- subset(dis_site2, Year!=2020)
dis_site2$YearBinary <- ifelse(dis_site2$Year==2019, 0, 1)
# this is the data set for models that will build SEM
# note, lots of missing epifauna data (e.g. no BB for 2021?)
# also missing temp data (lack of SST)
# exclude
dis_site2 <- dis_site2[-which(is.na(dis_site2$Epifauna)),] # reduces dataset from 7257 observations to 5543 
dis_site2$Meadow <- paste(dis_site2$Region, dis_site2$SiteCode, sep="_")
write_csv(dis_site2, "data/epiphyte_SEM_data_2.csv")


## alternative - site level predictors with blade level responses, NO TEMPERATURE so all years inclulded
site_all <- tran_vars %>%
  group_by(Year, Region, SiteCode, Meadow) %>%
  summarise(PercentOther=mean(PercentOtherMean), PercentBare=mean(PercentBareMean), 
            Structure=mean(Structure), StructureLog=log10(Structure), Latitude=mean(Latitude),
            Density=mean(DensityShootsMean), SheathLength=mean(SheathLengthMean), CanopyHeight=mean(CanopyHeight),
            Epifauna_all=mean(epifauna_log_per_g_transect_all), 
            Epifauna_large = mean(epifauna_log_per_g_transect_large),
            DensityLog=log10(Density),
            EpiphytePerAreaMeadowMg=mean(EpiphytePerAreaMeanMg),
            TempAnomWarm_March=mean(TempAnomWarm_March), TempAnomWarm_April=mean(TempAnomWarm_April), 
            TempAnomWarm_May=mean(TempAnomWarm_May), TempAnomWarm_June=mean(TempAnomWarm_June),
            Prevalence=mean(PrevalenceMean), LesionArea=mean(LesionAreaMean),
            Ampithoid_all=mean(Ampithoid, na.rm=TRUE), Lacuna_all=mean(Lacuna, na.rm=TRUE),
            Idoteid_all=mean(Idoteid, na.rm=TRUE), Richness_all=max(Richness),
            Ampithoid_large=mean(Ampithoid_large, na.rm=TRUE), Lacuna_large=mean(Lacuna_large, na.rm=TRUE),
            Idoteid_large=mean(Idoteid_large, na.rm=TRUE), Richness_large=max(Richness_large))
