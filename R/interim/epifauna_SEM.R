
# Load packages:
library(tidyverse)
library(nlme)
library(lme4)
library(piecewiseSEM)
#library(glmmTMB)
#library(psych)

EWD_2019_transect_data <- read.csv("EGWD_2019_data_transect_level_20210203.csv", header = TRUE)

# Data frame has NA's for the temp anomaly (CDiffMeanHeat) for several sites. Delete these so all models have same data: 
EWD_2019_transect_data_nona <- EWD_2019_transect_data %>% drop_na(CDiffMeanHeat) # requires tidyr/tidyverse
tran <- read.csv("data/all_survey_metrics_transect.csv")
tran$TidalHeightTransect <- paste(tran$TidalHeight, tran$Transect, sep="")
dat <- left_join(EWD_2019_transect_data_nona, tran, by=c("region"="Region", "site_code"="SiteCode", "transect" = "TidalHeightTransect"))

# create GLMM of prevalence - note need to use nlme() not glmmTMB() (piecewiseSEM doesn't support glmmTMB yet)
prev_cpta <- glmer(disease_prevalence_transect ~ CDiffMeanHeat +(1|region), 
                 data = dat,
                 weights=CountBlades,
                 family=binomial)
epi <-   lmer(epifauna_per_g_plant_log_transect ~ 1 +(1|region), data = dat)

cpta_epi <- 
summary(prev_cpta)

# Temperature > disease
sem_prev_epi_1 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat +(1|region), 
        data = dat,
        weights=CountBlades,
        family=binomial),
  lmer(epifauna_per_g_plant_log_transect ~ 1 +(1|region), data = dat),
  #disease_prevalence_transect %~~% epifauna_per_g_plant_log_transect,
  data=dat
)  
sem_1 <- psem(
  prev_cpta,
  epi,
  data=dat
)
summary(sem_prev_epi_1, conserve=TRUE) # this gives an error (not enough dimensions) and I think it may be the lack of paths?

# Epifauna > disease
sem_prev_epi_2 <- psem(
  glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect +(1|region), 
        data = dat,
        weights=CountBlades,
        family=binomial),
  lmer(CDiffMeanHeat ~ 1 +(1|region), na.action= na.omit, data=dat),
  data=dat
)  
# can't fit this with psem because it gives an error - some data are transformed, some are not? but it shouldn't give an error here
summary(sem_prev_epi_2)

# Temperature > (disease, Epifauna)
sem_prev_epi_3 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat +(1|site_unique_code),data = dat,
        weights=CountBlades, family=binomial),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + (1|site_unique_code),
      na.action = na.omit, data = dat),
  disease_prevalence_transect %~~% epifauna_per_g_plant_log_transect,
  data=dat)

summary(sem_prev_epi_3)
# No independence claims present. Tests of directed separation not possible.

# Temperature > (disease, Epifauna); disease > Epifauna
sem_prev_epi_4 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat +(1|region),data = dat,
        weights=CountBlades, family=binomial),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + disease_prevalence_transect + (1|region), 
      data=dat)
)  
summary(sem_prev_epi_4) 
# No independence claims present. Tests of directed separation not possible.


# Temperature > (disease, Epifauna); Epifauna > disease
sem_prev_epi_5 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat + epifauna_per_g_plant_log_transect +(1|site_unique_code),data = dat,
        weights=CountBlades, family=binomial),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + (1|site_unique_code), 
       data=dat)
)  
summary(sem_prev_epi_5)
# No independence claims present. Tests of directed separation not possible.


# Temperature > disease > Epifauna
sem_prev_epi_6 <- psem(
  glmer(disease_prevalence_transect ~ CDiffMeanHeat  +(1|region),data = dat,
        weights=CountBlades, family=binomial),
  lmer(epifauna_per_g_plant_log_transect ~ disease_prevalence_transect + (1|region), 
       data=dat)
)  
summary(sem_prev_epi_6)
# BAD MODEL
# Global goodness-of-fit:
# Chi-Squared = 4.284 with P-value = 0.038 and on 1 degrees of freedom
# Fisher's C = 16.963 with P-value = 0 and on 2 degrees of freedom
# epifauna_per_g_plant_log_transect ~ CDiffMeanHeat P = 2e-04 ***


# Temperature > Epifauna > disease
sem_prev_epi_7 <- psem(
  glmer(disease_prevalence_transect ~ epifauna_per_g_plant_log_transect +(1|site_unique_code), data = dat, 
        weights=dat$CountBlades, family=binomial),
  lmer(epifauna_per_g_plant_log_transect ~ CDiffMeanHeat + (1|site_unique_code), 
       data=dat)
) 
summary(sem_prev_epi_7)
# Warning message:
# Check model convergence: log-likelihood estimates lead to negative Chi-squared! summary(sem_prev_epi_5)
# OK model:
# Global goodness-of-fit:
# Chi-Squared = NA with P-value = NA and on 1 degrees of freedom
# Fisher's C = 3.043 with P-value = 0.218 and on 2 degrees of freedom
# disease_prevalence_transect ~ CDiffMeanHeat P = 0.2184

AIC(sem_prev_epi_3, sem_prev_epi_4, sem_prev_epi_5, sem_prev_epi_6, sem_prev_epi_7)
AIC(sem_prev_epi_3)
AIC(sem_prev_epi_4)
AIC(sem_prev_epi_5)
AIC(sem_prev_epi_6)
AIC(sem_prev_epi_7)
