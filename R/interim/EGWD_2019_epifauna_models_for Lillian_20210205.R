###################################################################################
#                                                                                ##
# NSF East Pacific Eelgrass Wasting Disease (EGWD) ecology project               ##
# SCRIPT: EPIFAUNA MODELING                                                      ## 
# Data are current as of 2021-02-02                                              ##
# Emmett Duffy (duffye@si.edu)                                                   ##  
# Last updated 2021-02-05 by Emmett Duffy                                        ##
#                                                                                ##
###################################################################################

###################################################################################
# METADATA                                                                        #
###################################################################################

# This script fits alternative path models (SEMs) to test the relative and interactive influence 
# of local warming (CDiffMeanHeat = local cumulative temperature anomaly) and grazer 
# abundance on eelgrass wasting disease.

# Note: Random component nests within region. Too few replicates per site to nest within
# sites. Also random grouping by site probably eliminates much of the variation in 
# temperature animaly that is key to explaining the disrease patterns. 


###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

# Load packages:
library(tidyverse)
library(nlme)
library(piecewiseSEM)
library(psych)


###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

EWD_2019_transect_data <- read.csv("EGWD_2019_data_transect_level_20210203.csv", header = TRUE)

# Data frame has NA's for the temp anomaly (CDiffMeanHeat) for several sites. Delete these so all models have same data: 
EWD_2019_transect_data_nona <- EWD_2019_transect_data %>% drop_na(CDiffMeanHeat) # requires tidyr/tidyverse


###################################################################################
# SEM COMPARISON: DISEASE PREVALENCE AND EPIFAUNA (TRANSECT LEVEL)                #
###################################################################################

# Temperature > disease
sem_prev_epi_1 <- psem(
  lme(disease_prevalence_transect ~ CDiffMeanHeat, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(epifauna_per_g_plant_log_transect ~ 1,
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_1) 
# BAD MODEL: 
# disease_prevalence_transect ~ epifauna_per_g_plant_log_transect P = 1e-04 ***
# Chi-Squared = 9.873 with P-value = 0.002 and on 1 degrees of freedom
# Fisher's C = 18.341 with P-value = 0 and on 2 degrees of freedom


# Epifauna > disease
sem_prev_epi_2 <- psem(
  lme(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(CDiffMeanHeat ~ 1,
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_2)
# Warning message:
# Check model convergence: log-likelihood estimates lead to negative Chi-squared! 
# OK model
# disease_prevalence_transect ~ CDiffMeanHeat P = 0.2184 
# Chi-Squared = NA with P-value = NA and on 1 degrees of freedom
# Fisher's C = 3.043 with P-value = 0.218 and on 2 degrees of freedom


# Temperature > (disease, Epifauna)
sem_prev_epi_3 <- psem(
  lme(disease_prevalence_transect ~ CDiffMeanHeat,
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat,
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  (disease_prevalence_transect %~~% epifauna_per_g_plant_log_transect)
)
summary(sem_prev_epi_3)
# No independence claims present. Tests of directed separation not possible.


# Temperature > (disease, Epifauna); disease > Epifauna
sem_prev_epi_4 <- psem(
  lme(disease_prevalence_transect ~ CDiffMeanHeat, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + disease_prevalence_transect, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_4) 
# No independence claims present. Tests of directed separation not possible.


# Temperature > (disease, Epifauna); Epifauna > disease
sem_prev_epi_5 <- psem(
  lme(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect + CDiffMeanHeat, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_5)
# No independence claims present. Tests of directed separation not possible.


# Temperature > disease > Epifauna
sem_prev_epi_6 <- psem(
  lme(disease_prevalence_transect ~ CDiffMeanHeat, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(epifauna_per_g_plant_log_transect ~ disease_prevalence_transect,  
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_6)
# BAD MODEL
# Global goodness-of-fit:
# Chi-Squared = 4.284 with P-value = 0.038 and on 1 degrees of freedom
# Fisher's C = 16.963 with P-value = 0 and on 2 degrees of freedom
# epifauna_per_g_plant_log_transect ~ CDiffMeanHeat P = 2e-04 ***


# Temperature > Epifauna > disease
sem_prev_epi_7 <- psem(
  lme(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect, 
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona),
  lme(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat,  
      random = ~1|region, na.action = na.omit, data = EWD_2019_transect_data_nona)
) 
summary(sem_prev_epi_7)
# Warning message:
# Check model convergence: log-likelihood estimates lead to negative Chi-squared! summary(sem_prev_epi_5)
# OK model:
# Global goodness-of-fit:
# Chi-Squared = NA with P-value = NA and on 1 degrees of freedom
# Fisher's C = 3.043 with P-value = 0.218 and on 2 degrees of freedom
# disease_prevalence_transect ~ CDiffMeanHeat P = 0.2184

AIC(sem_prev_epi_1, sem_prev_epi_2, sem_prev_epi_3, sem_prev_epi_4, sem_prev_epi_5, sem_prev_epi_6, sem_prev_epi_7)
#       AIC K   n
# 1 267.998 7 161
# 2 782.156 7 161
# 3 267.204 8 161
# 4 255.560 9 161
# 5 259.330 9 161
# 6 257.844 8 161
# 7 249.794 8 161

# RESULT: Best model in terms of AIC and fit is 7: Temperature > Epifauna > disease

summary(sem_prev_epi_7)
#                      Response                         Predictor Estimate Std.Error  DF Crit.Value P.Value Std.Estimate    
#   disease_prevalence_transect epifauna_per_g_plant_log_transect   0.1449    0.0380 154     3.8182  0.0002       0.2738 ***
#   epifauna_per_g_plant_log_transect               CDiffMeanHeat   0.0450    0.0136 154     3.3237  0.0011       0.5889  **
#
# Individual R-squared:
#                          Response   method Marginal Conditional
#       disease_prevalence_transect     none     0.07        0.42
# epifauna_per_g_plant_log_transect     none     0.18        0.64


# Model diagnostics: residual plots
par(mfrow = c(1, 2))
ypred = predict(sem_prev_epi_7)
res = residuals(sem_prev_epi_7, type = 'pearson')
# plot(ypred,res, xlab = "predicted", ylab = "residuals",)
hist(res, xlab = "residuals", main = "Disease prevalence (sem_prev_epi_7)",)

#QQ plot
qqnorm(res, xlab = "Model Quantiles", ylab = "Observation Quantiles", main = "") 
qqline(res, col = "blue", lwd = 2)
# RESULT: Some overestimation by model at high end of quantiles. Check these out - who are they? 

