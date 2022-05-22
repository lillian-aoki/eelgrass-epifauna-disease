###################################################################################
#                                                                                ##
# NSF East Pacific Eelgrass Wasting Disease (EGWD) ecology project               ##
# SCRIPT: EPIFAUNA MODELING                                                      ## 
# Data are current as of 2021-02-02                                              ##
# Emmett Duffy (duffye@si.edu)                                                   ##  
# Last updated 2021-05-26 by Emmett Duffy                                        ##
#                                                                                ##
###################################################################################


###################################################################################
# TABLE OF CONTENTS                                                               #
#                                                                                 #
# METADATA AND APPROACH                                                           #
# LOAD PACKAGES                                                                   #
# READ IN AND PREPARE DATA                                                        #
# SEM COMPARISON: DISEASE PREVALENCE AND EPIFAUNA (TRANSECT LEVEL)                #
# SEM COMPARISON: DISEASE PREVALENCE AND LACUNA (TRANSECT LEVEL)                  #
# SEM COMPARISON - DISEASE SEVERITY AND LACUNA (TRANSECT LEVEL)                   #
# SEM COMPARISON - DISEASE PREVALENCE AND LACUNA (SITE LEVEL)                     #
# SEM COMPARISON - DISEASE PREVALENCE AND LACUNA: WASHINGTON (TRANSECT LEVEL)     #
# SEM COMPARISON - DISEASE PREVALENCE AND LACUNA: ALASKA (TRANSECT LEVEL)         #
#                                                                                 #
###################################################################################


###################################################################################
# METADATA AND APPROACH                                                           #
###################################################################################

# This script fits alternative path models (SEMs) to test the relative and interactive influence 
# of local warming (CDiffMeanHeat = local cumulative temperature anomaly) and grazer 
# abundance on eelgrass wasting disease.

# Models are run on transect-level means; each site has six transects, with 2 epifauna
# samples each, which are therefore pooled within transects. The models include a random 
# component (intercept) for site but NOT regions since prelim analysis shpows little to 
# no variance explained by region.  


###################################################################################
# LOAD PACKAGES                                                                   #
###################################################################################

# Load packages:
library(tidyverse)
library(nlme)
library(piecewiseSEM)
library(psych)
library(lme4) # for models from Jon's code


###################################################################################
# READ IN AND PREPARE DATA                                                        #
###################################################################################

# NOTE: The following files were assembled using this script: NSF_EGWD_project_2019_epifauna_data_assembly_version_20210201.R

EWD_2019_transect_data <- read.csv("EGWD_2019_data_transect_level_20210203.csv", header = TRUE)
EWD_2019_site_data <- read.csv("EGWD_2019_data_site_level_20210203.csv", header = TRUE)

# Reorder variables 'region': S to N
EWD_2019_site_data$region <- as.factor(EWD_2019_site_data$region)
EWD_2019_site_data$region <- factor(EWD_2019_site_data$region, levels = c("SD", "BB", "OR", "WA", "BC", "AK"))

EWD_2019_transect_data$region <- as.factor(EWD_2019_transect_data$region)
EWD_2019_transect_data$region <- factor(EWD_2019_transect_data$region, levels = c("SD", "BB", "OR", "WA", "BC", "AK"))

# Data frame has NA's for the temp anomaly (CDiffMeanHeat) for several sites. Delete these so all models have same data: 
EWD_2019_transect_data_nona <- EWD_2019_transect_data %>% drop_na(CDiffMeanHeat) # requires tidyr/tidyverse
EWD_2019_site_data_nona <- EWD_2019_site_data %>% drop_na(CDiffMeanHeat)


# Visualize tested predictors of disease prevalence (site means):  
pairs.panels(EWD_2019_site_data[,c("region", "PrevalenceMean", "SeverityMean", "CDiffMeanHeat", 
  "epifauna_per_g_plant_log_site", "Lacuna_spp_per_g_plant_log_site", "ampithoids_per_g_plant_log_site", 
  "idoteids_per_g_plant_log_site")], 
  smooth=T,density=F,ellipses=F,lm=F,digits=2,scale=F, cex.cor = 6)

# Visualize tested predictors of disease prevalence (transect means):  
pairs.panels(EWD_2019_transect_data[,c("region", "disease_prevalence_transect", 
  "disease_severity_transect", "CDiffMeanHeat", "epifauna_per_g_plant_log_transect",
  "Lacuna_spp_per_g_plant_log_transect", "ampithoids_per_g_plant_log_transect", "idoteids_per_g_plant_log_transect")], 
  smooth=T,density=F,ellipses=F,lm=F,digits=2,scale=F, cex.cor = 6)


###################################################################################
# TESTS FOR OPTIMAL STRUCTURE OF RANDOM COMPONENT                                 #
###################################################################################

# Tests for optimal structure of LOGISTIC (BINOMIAL) model, starting with a saturated model

# Random: site and region
test1 <- glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect + CDiffMeanHeat
               + (1 | region/site_code),
               family = "binomial", na.action = na.omit,
               data = EWD_2019_transect_data_nona) 

# Random: site 
test2 <- glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect + CDiffMeanHeat
               + (1 | site_code),
               family = "binomial", na.action = na.omit,
               data = EWD_2019_transect_data_nona) 

# Random: region
test3 <- glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect + CDiffMeanHeat
               + (1 | region),
               family = "binomial", na.action = na.omit,
               data = EWD_2019_transect_data_nona) 

AIC(test1, test2, test3)
#       df     AIC
# test1  5 179.5912
# test2  4 179.7720
# test3  4 180.858
# Model with only region is worst. 

# get variance components on most nested model
VarCorr(test1) # standard dev is near zero for region.  

# Since AIC of models 1 and 2 are very close, and region explains almost no variance,
# we go forward using only site in random component. 


###################################################################################
# SEMs: DISEASE PREVALENCE AND EPIFAUNA (TRANSECT LEVEL - LOGISTIC)               #
###################################################################################

# Here we compare a set of candidate logistic models to infer direction of causality for 
# eelgrass disease prevalence, using site code as random term. 

# 1. Temperature > disease
sem_prev_epi_1 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat
        + (1 | site_code),
        family = "binomial",
        na.action = na.omit, data = EWD_2019_transect_data_nona),
  epifauna_per_g_plant_log_transect ~ 1,
  data = EWD_2019_transect_data_nona
)
summary(sem_prev_epi_1)
# NOT GREAT model
# Chi-Squared = 6.012 with P-value = 0.014 and on 1 degrees of freedom
# Fisher's C = 8.31 with P-value = 0.016 and on 2 degrees of freedom
# disease_prevalence_transect ~ epifauna_per_g_plant_log_transect + ... P = 0.0157 *


# 2. Epifauna > disease
sem_prev_epi_2 <- psem(
  glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect 
        + (1 | site_code),
        family = "binomial", 
        na.action = na.omit, data = EWD_2019_transect_data_nona), 
  CDiffMeanHeat ~ 1, 
  data = EWD_2019_transect_data_nona
)  
summary(sem_prev_epi_2)
# BAD model
# Chi-Squared = 34.653 with P-value = 0 and on 1 degrees of freedom
# Fisher's C = 27.835 with P-value = 0 and on 2 degrees of freedom


# 3. Temperature > (disease, Epifauna)
sem_prev_epi_3 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat
        + (1 | site_code),
        family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat
       + (1 | site_code),
       family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  (disease_severity_transect %~~% epifauna_per_g_plant_log_transect)
)
summary(sem_prev_epi_3)
# No independence claims present. Tests of directed separation not possible.
# NOTE: strong correlation between epifauna and disease, presumably this means with 
# temperature effect on both factored out. I think this means there is a relationship
# between epifauna and disease independent of temperature animaly, which is presumably grazing.


# 4. Temperature > (disease, Epifauna); disease > Epifauna
sem_prev_epi_4 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat 
        + (1 | site_code),
        family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + disease_prevalence_transect 
       + (1 | site_code),
       family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_4) 
# No independence claims present. Tests of directed separation not possible.


# 5. Temperature > (disease, Epifauna); Epifauna > disease
sem_prev_epi_5 <- psem(
  glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect + CDiffMeanHeat 
        + (1 | site_code),
        family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat 
       + (1 | site_code),
       family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_5)
# No independence claims present. Tests of directed separation not possible.


# 6. Temperature > disease > Epifauna
sem_prev_epi_6 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat 
        + (1 | site_code),
        family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ disease_prevalence_transect  
       + (1 | site_code),
       family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_prev_epi_6)
# BAD MODEL
# Global goodness-of-fit:
# Chi-Squared = 4.284 with P-value = 0.038 and on 1 degrees of freedom
# Fisher's C = 16.963 with P-value = 0 and on 2 degrees of freedom
# epifauna_per_g_plant_log_transect ~ CDiffMeanHeat P = 2e-04 ***


# 7. Temperature > Epifauna > disease
sem_prev_epi_7 <- psem(
  glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect 
        + (1 | site_code),
        family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat  
       + (1 | site_code),
       family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona)
) 
summary(sem_prev_epi_7)
# OK model:
# Global goodness-of-fit:
# Chi-Squared = 1.351 with P-value = 0.245 and on 1 degrees of freedom
# Fisher's C = 5.175 with P-value = 0.075 and on 2 degrees of freedom
# epifauna_per_g_plant_log_transect ~ disease_prevalence_transect + ... P = 0.0752


# 8. (Temperature, Epifauna) > disease
sem_prev_epi_8 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat + epifauna_per_g_plant_log_transect 
        + (1 | site_code),
        family = "binomial", na.action = na.omit,data = EWD_2019_transect_data_nona),
  (CDiffMeanHeat %~~% epifauna_per_g_plant_log_transect)
  
) 
summary(sem_prev_epi_8)
# No independence claims present. Tests of directed separation not possible.
# NOTE: Strong positive correlation between epifauna and temperature anomaly, meaning
# important term is left out of the causality. 


AIC(sem_prev_epi_1, sem_prev_epi_2, sem_prev_epi_3, sem_prev_epi_4, sem_prev_epi_5, sem_prev_epi_6, sem_prev_epi_7, sem_prev_epi_8)
#       AIC K   n
# 1 183.784 3 161 NOT GREAT 
# 2 212.425 3 161 BAD
# 3 267.204 8 161
# 4 255.560 9 161
# 5 259.330 9 161
# 6 257.844 8 161 BAD
# 7 367.470 9 161
# 8 179.772 4 161

# Model 8 is best. But not clear what it means. Otherwise, best model is 1: epifauna don't 
# even need to be included. BUT not sure I trust Jon's coding of the 'epifauna ~ 1' term ...


summary(sem_prev_epi_8)
# Tests of directed separation:
# No independence claims present. Tests of directed separation not possible.
# Chi-Squared = 0 with P-value = 1 and on 0 degrees of freedom
# Fisher's C = NA with P-value = NA and on 0 degrees of freedom
# 
# Coefficients:
# 
#                      Response                         Predictor Estimate Std.Error  DF Crit.Value P.Value Std.Estimate    
#   disease_prevalence_transect                     CDiffMeanHeat   0.1659    0.0338 161     4.9116  0.0000       0.5254 ***
#   disease_prevalence_transect epifauna_per_g_plant_log_transect   0.9098    0.3766 161     2.4161  0.0157       0.2203   *
#             ~~CDiffMeanHeat ~~epifauna_per_g_plant_log_transect   0.2235         - 159     2.8913  0.0044       0.2235  **
# 
# Individual R-squared:
# 
#                      Response method Marginal Conditional
#   disease_prevalence_transect  delta     0.32        0.34

# RESULT: Both temperature anomaly and epifaunal abundance increase disease, but effect of temperature
# anomaly is more than twice as strong. 


# Model diagnostics: residual plots
par(mfrow = c(1, 2))
ypred = predict(sem_prev_epi_8)
res = residuals(sem_prev_epi_8, type = 'pearson')
# plot(ypred,res, xlab = "predicted", ylab = "residuals",)
hist(res, xlab = "residuals", main = "Disease prevalence (sem_prev_epi_7)",)
qqnorm(res, xlab = "Model Quantiles", ylab = "Observation Quantiles", main = "") 
qqline(res, col = "blue", lwd = 2)
# RESULT: Good. Some overestimation by model at high end of quantiles. Check these out - who are they? 


###################################################################################
# SEM COMPARISON: DISEASE SEVERITY AND EPIFAUNA (TRANSECT LEVEL)                  #
###################################################################################

# Temperature > disease
sem_sev_epi_1 <- psem(
  lmer(disease_severity_transect ~ CDiffMeanHeat 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ 1
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_sev_epi_1) 
# BAD MODEL: 
# disease_severity_transect ~ epifauna_per_g_plant_log_transect P = 0 ***
# Chi-Squared = 9.202 with P-value = 0.002 and on 1 degrees of freedom
# Fisher's C = 19.823 with P-value = 0 and on 2 degrees of freedom


# Epifauna > disease
sem_sev_epi_2 <- psem(
  lmer(disease_severity_transect ~ epifauna_per_g_plant_log_transect 
      + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(CDiffMeanHeat ~ 1
      + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_sev_epi_2)
# BAD model
# disease_severity_transect ~ CDiffMeanHeat P = 0.0059 **
# Chi-Squared = NA with P-value = NA and on 1 degrees of freedom
# Fisher's C = 10.279 with P-value = 0.006 and on 2 degrees of freedom


# Temperature > (disease, Epifauna)
sem_sev_epi_3 <- psem(
  lmer(disease_severity_transect ~ CDiffMeanHeat
      + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat
      + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  (disease_severity_transect %~~% epifauna_per_g_plant_log_transect)
)
summary(sem_sev_epi_3)
# No independence claims present. Tests of directed separation not possible.
# NOTE: strong correlation between epifauna and disease, presumably this means with 
# temperature effect on both factored out. I think this means there is a relationship
# between epifauna and disease independent of temperature anomaly, which is presumably grazing.


# 4. Temperature > (disease, Epifauna); disease > Epifauna
sem_sev_epi_4 <- psem(
  lmer(disease_severity_transect ~ CDiffMeanHeat 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + disease_severity_transect 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_sev_epi_4) 
# No independence claims present. Tests of directed separation not possible.


# 5. Temperature > (disease, Epifauna); Epifauna > disease
sem_sev_epi_5 <- psem(
  lmer(disease_severity_transect ~ epifauna_per_g_plant_log_transect + CDiffMeanHeat 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_sev_epi_5)
# No independence claims present. Tests of directed separation not possible.


# 6. Temperature > disease > Epifauna
sem_sev_epi_6 <- psem(
  lmer(disease_severity_transect ~ CDiffMeanHeat 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ disease_severity_transect  
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona)
)  
summary(sem_sev_epi_6)
# Warning message:
# Check model convergence: log-likelihood estimates lead to negative Chi-squared! 
# OK MODEL
# Chi-Squared = NA with P-value = NA and on 1 degrees of freedom
# Fisher's C = 3.573 with P-value = 0.168 and on 2 degrees of freedom


# 7. Temperature > Epifauna > disease
sem_sev_epi_7 <- psem(
  lmer(disease_severity_transect ~ epifauna_per_g_plant_log_transect 
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat  
       + (1 | site_code), na.action = na.omit,data = EWD_2019_transect_data_nona)
) 
summary(sem_sev_epi_7)
# BAD model:
# Chi-Squared = 1.076 with P-value = 0.3 and on 1 degrees of freedom
# Fisher's C = 16.733 with P-value = 0 and on 2 degrees of freedom
# disease_severity_transect ~ CDiffMeanHeat P = 2e-04 ***


# 8. (Temperature, Epifauna) > disease
sem_sev_epi_8 <- psem(
  lmer(disease_severity_transect ~ CDiffMeanHeat + epifauna_per_g_plant_log_transect 
        + (1 | site_code),
        na.action = na.omit,data = EWD_2019_transect_data_nona),
  (CDiffMeanHeat %~~% epifauna_per_g_plant_log_transect)
) 
summary(sem_sev_epi_8)
# No independence claims present. Tests of directed separation not possible.
# NOTE: strong correlation between epifauna and T anomaly, meaning relationship
# between epifauna and T anomaly, independent of disease.


AIC(sem_sev_epi_1, sem_sev_epi_2, sem_sev_epi_3, sem_sev_epi_4, sem_sev_epi_5, sem_sev_epi_6, sem_sev_epi_7, sem_sev_epi_8)
#       AIC K   n
# 1 -135.492 7 161 BAD
# 2  380.490 7 161 BAD
# 3 -107.294 8 161
# 4 -119.291 9 161
# 5 -112.246 9 161
# 6 -127.827 8 161
# 7 -113.170 8 161 BAD
# 8 -382.637 5 161

# RESULT: Very interesting. Best model is 8: both T anomaly and epifauna influence 
# disease severity. BUT 4 is better than 5, and 6 is better than 7. In both those
# comparisons, the better models shows disease influencing epifauna, rather than vice versa.


summary(sem_sev_epi_8)
# 

# Model diagnostics: residual plots
par(mfrow = c(1, 2))
ypred = predict(sem_sev_epi_4)
res = residuals(sem_sev_epi_4, type = 'pearson')
# plot(ypred,res, xlab = "predicted", ylab = "residuals",)
hist(res, xlab = "residuals", main = "Disease prevalence (sem_sev_epi_4)",)

#QQ plot
qqnorm(res, xlab = "Model Quantiles", ylab = "Observation Quantiles", main = "") 
qqline(res, col = "blue", lwd = 2)
# RESULT: Some overestimation by model at high end of quantiles. Check these out - who are they? 




