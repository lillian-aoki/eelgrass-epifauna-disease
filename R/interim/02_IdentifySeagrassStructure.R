# Identify Seagrass Predictors for SEM analysis
# Last updated: 2022-07-18
# Updated by: Lillian Aoki

# This script explores transect-level data to identify most relevant seagrass predictor
# for grazing scar SEM models

# Input data is the file "full_seagrass_epifauna_for_SEM.csv", produced from script 01_DataPrepForSEM.R

library(tidyverse)
library(GGally)
library(car)
library(readr)
library(dplyr)

# Some seagrass variable likely predicts epifauna abundance and/or Grazing Scars
# Possible predictors are Canopy Height, Shoot Density, Structure, Blade Area

# read in data
egwd <- read_csv("data/full_seagrass_epifauna_for_SEM.csv")
# limit to 2019 for the first round of analysis (using more sites with temperature)
egwd19 <- subset(egwd, Year=="2019")
# use pair plots to systemically compare correlations
egwd19_lim1 <- select(egwd19, c("CanopyHeight", "StructureLog", "DensityShootsMean", "Structure", "BladeAreaMean",
                                 "GrazingScarsMeanTransect", "PrevalenceMean"))
ggpairs(egwd19_lim1)
# Scars and Prevalence are correlated
# Scars is correlated with Structure and Density, Prevalence isn't really correlated with any seagrass metric
# shoot density was the better seagrass predictor of disease at transect level in 2019 (see L&O paper) but slightly different dataset here
# Structure is ecologically more meaningful and has the strongest correlation with Grazing (StructureLog)

# look at additional pair plots with epifauna abundance, and other disease metrics
egwd19_lim2 <- select(egwd19, c("CanopyHeight", "StructureLog", "DensityShootsMean", "Structure", "BladeAreaMean", "epiphytes_per_area_transect_log",
                                 "GrazingScarsMeanTransect", "epifauna_per_g_log_transect", "PrevalenceMean", "SeverityMean", "LesionAreaMean"))
ggpairs(egwd19_lim2)
# epifauna abundance and grazing scars are correlated, and epifauna abundance is correlated with Prev and Sev, weaker with Lesion Area
# Severity is more strongly correlated with the plant metrics (esp Canopy Height, Blade Area) whereas lesion area correlates with grazing and nothing else

# For the simplest SEM, we will include Structure as the predictor of Grazing in the Prevalence model
# Possibly use Canopy Height or Blade Area in the Severity Model (Blade Area is not correlated with Structure) - to predict Severity
# Lesion area doesn't need a seagrass predictor

sg <- select(egwd19, c("TempAnomWarm_March", "TempAnomWarm_April", "TempAnomWarm_May",
                       "TempAnomWarm_June", "StructureLog", "SeverityMean", "LesionAreaMean", "GrazingScarsMeanTransect"))
ggpairs(sg)
